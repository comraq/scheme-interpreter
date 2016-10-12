module Core (eval, primitiveEnv) where

import Control.Arrow ((&&&))
import Control.Monad.Except
import Data.Maybe (fromMaybe, isNothing)

import Definition
import IOFunction
import PrimFunction
import LispVector (vectorSet, vectorFill, vectorLength)
import Parser
import Variable
import Unpacker (unpackBoolCoerce)


eval :: Env -> LispVal -> IOEvaled LispVal
eval env expr = evalDeep env expr >>= derefPtrValSafe

-- TODO: Literal lists/dotted lists/vectors should be mutable by default
evalDeep :: Env -> LispVal -> IOEvaled LispVal
evalDeep env (LAtom var)           = getVarDeep env var
evalDeep _   var@(LNumber _)       = return var
evalDeep _   var@(LString _)       = return var
evalDeep _   var@(LBool _)         = return var
evalDeep _   var@(LChar _)         = return var

evalDeep env var@(LPointer _)      = derefPtrVal var

evalDeep env val@(LList lvs)       = case lvs of
  -- Special Lists
  [LAtom "quote",      vs] -> return vs
  (LAtom "quasiquote": vs) -> LList <$> sequence (evalUnquote env vs)

  -- Eval as a Function
  (func : args) -> callFunc env func args
evalDeep _   var@(LDottedList _ _) = throwError $ BadSpecialForm "Must be a proper list" var
evalDeep env (LVector lvs)         = LVector <$> mapM (evalDeep env) lvs

evalDeep _   badForm               = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalOnce :: Env -> LispVal -> IOEvaled LispVal
evalOnce env (LAtom var)      = getVar env var
evalOnce env var              = evalDeep env var

evalUnquote :: Env -> [LispVal] -> [IOEvaled LispVal]
evalUnquote env = go
  where go :: [LispVal] -> [IOEvaled LispVal]
        go (val:vals) = case val of
          LList [LAtom "unquote", expr]          -> evalDeep env expr         : go vals
          LList (LAtom "unquote-splicing":exprs) -> map (evalDeep env) exprs ++ go vals
          _                                      -> return val                : go vals
        go _      = []


------- Calling and Getting Scheme Functions -------

callFunc :: Env -> LispVal -> LIOFunction
callFunc env func args = evalDeep env func >>= funcApply
  where
    funcApply :: LispVal -> IOEvaled LispVal
    funcApply (LEnvFunc _ f)   = f env args
    funcApply f@LLambdaFunc {} = mapM (evalOnce env) args >>= apply f
    funcApply f                = mapM (eval env) args >>= apply f

apply :: LispVal -> LIOFunction
apply (LPrimitiveFunc _ f)                      args = liftEvaled $ f args
apply (LIOFunc _ f)                             args = f args
apply (LLambdaFunc params varargs body closure) args
  | isNothing varargs && num params /= num args = throwError $ NumArgs (num params) args
  | num params > num args                       = throwError $ NumArgs (num params) args
  | otherwise                                   =
      -- Binds param names and function args to lambda closure env,
      -- then lift the IO results back to the monad transformers stack
      (liftIO . bindVars closure $ zip params args)

      -- Bind the optional 'varargs' is applicable
      >>= bindVarArgs varargs

      -- Evals all expressions in the body and
      -- returns the results of the last expression
      >>= evalBody

  where
    num :: [a] -> Int
    num = length

    evalBody :: Env -> IOEvaled LispVal
    evalBody env = mapMLast (evalDeep env) body

    bindVarArgs :: Maybe String -> Env -> IOEvaled Env
    bindVarArgs Nothing        env = return env
    bindVarArgs (Just argName) env =
      liftIO $ bindVars env [(argName, LList remainingArgs)]

    remainingArgs :: [LispVal]
    remainingArgs = drop (length params) args

apply expr _ = throwError . NotFunction "Trying to call non-function" $ show expr


------- Environment with all Function Preset Bindings -------

primitiveEnv :: IO Env
primitiveEnv = emptyEnv >>= (`bindVars` (primFuncs ++ envFuncs ++ ioFuncs))
  where
    makeFunc :: (LFuncName -> func -> LispVal)
             -> (LFuncName, func)
             -> (LFuncName, LispVal)
    makeFunc f = fst &&& uncurry f

    primFuncs :: [(String, LispVal)]
    primFuncs = map (makeFunc LPrimitiveFunc) primitiveFunctions

    envFuncs :: [(String, LispVal)]
    envFuncs = map (makeFunc LEnvFunc) envFunctions

    ioFuncs :: [(String, LispVal)]
    ioFuncs = map (makeFunc LIOFunc) ioFunctions


------ Environment Interacting Functions -------

envFunctions :: [(LFuncName, Env -> LIOFunction)]
envFunctions =
  [ ("set!"        , setBang        )
  , ("define"      , define         )
  , ("lambda"      , lambda         )
  , ("string-set!" , stringSetBang  )
  , ("vector-set!" , vectorSetBang  )
  , ("vector-fill!", vectorFillBang )
  , ("load"        , loadProc       )
  , ("if"          , evalIf         )
  , ("cond"        , evalCond       )
  , ("case"        , evalCase       )

  , ("eq?"         , eqv            )
  , ("eqv?"        , eqv            )
  , ("apply"       , applyProc      )
  ]

setBang :: Env -> LIOFunction
setBang env [LAtom var, form] = evalDeep env form >>= setVar env var
setBang _   args              = throwError $ NumArgs 2 args

define :: Env -> LIOFunction

-- 'define' a function binding
define env (LList (LAtom func:params):body) =
  makeNormalFunc env params body >>= defineVar env func
define env (LDottedList (LAtom func:params) varargs:body) =
  makeVarargs varargs env params body >>= defineVar env func

-- 'define' a variable binding
define env [LAtom var, form] = evalOnce env form >>= defineVar env var
define _   args              = throwError $ NumArgs 2 args

lambda :: Env -> LIOFunction
lambda env (LList params:body)               = makeNormalFunc env params body
lambda env (LDottedList params varargs:body) = makeVarargs varargs env params body
lambda env (varargs@(LAtom _):body)          = makeVarargs varargs env []     body
lambda _ args = throwError $ InvalidArgs "Bad lambda expression" args

stringSetBang :: Env -> LIOFunction
stringSetBang env args@[_, _, _] = mapM (evalOnce env) args >>= setStr
  where
    setStr :: LIOFunction
    setStr [ptr, LNumber i, LChar c] = modifyPtrVal modifyStr ptr
      where
        modifyStr :: LispVal -> IOEvaled LispVal
        modifyStr (LString str)
          | fromIntegral i < length str = return . LString $ newStr str i c
          | otherwise = throwError $ InvalidArgs "index for 'string-set!' exceeds string length" args
        modifyStr _ = throwError badArgs
    setStr _ = throwError badArgs

    badArgs :: LispError
    badArgs = InvalidArgs "'string-set!' expects mutable string, number and char" args

    newStr :: (Integral a, Eq a) => String -> a -> Char -> String
    newStr str i c =
      let (hd, _:tl) = splitAt index str
          index      = fromIntegral i :: Int
      in  hd ++ c:tl
stringSetBang _   args           = throwError $ NumArgs 3 args

vectorSetBang :: Env -> LIOFunction
vectorSetBang env args@[_, _, _] = mapM (evalOnce env) args >>= setVec
  where
    setVec :: LIOFunction
    setVec [ptr, LNumber i, val] = modifyPtrVal modifyVec ptr
      where
        modifyVec :: LispVal -> IOEvaled LispVal
        modifyVec (LVector vec)
          | fromIntegral i < vectorLength vec = return . LVector $ vectorSet (fromIntegral i) val vec
          | otherwise = throwError $ Default "index for 'vector-set!' exceeds vector length"
        modifyVec _ = throwError badArgs
    setVec _ = throwError badArgs

    badArgs :: LispError
    badArgs = InvalidArgs "'vector-set!' expects vector, number and object" args
vectorSetBang _ args = throwError $ NumArgs 3 args

vectorFillBang :: Env -> LIOFunction
vectorFillBang env args@[_, _] = mapM (evalOnce env) args >>= fillVec
  where
    fillVec :: LIOFunction
    fillVec [ptr, val] = modifyPtrVal modifyVec ptr
      where
        modifyVec :: LispVal -> IOEvaled LispVal
        modifyVec (LVector vec) = return . LVector $ vectorFill val vec
        modifyVec _             = throwError badArgs
    fillVec _ = throwError badArgs

    badArgs :: LispError
    badArgs = InvalidArgs "'vector-fill!' expects vector and object" args
vectorFillBang _ args = throwError $ NumArgs 3 args

loadProc :: Env -> LIOFunction
loadProc env [LString filename] = load filename >>= mapMLast (evalDeep env)
loadProc _   args               = throwError $ NumArgs 1 args

evalIf :: Env -> LIOFunction
evalIf env [pred, conseq, alt] = fmap unpackBoolCoerce (evalDeep env pred) >>= \bool ->
  evalDeep env $ if bool then conseq else alt
evalIf _   args = throwError $ NumArgs 3 args

evalCond :: Env -> LIOFunction
evalCond env = go
  where
    go :: LIOFunction
    go (LList (LAtom "else":exprs):_)    = evalExprs env exprs
    go (LList [val, LAtom "=>", expr]:_) = evalDeep env expr >>= callAsFunc
      where callAsFunc :: LispVal -> IOEvaled LispVal
            callAsFunc expr = callFunc env expr [val]

    go (LList [pred]:_)          = evalDeep env pred
    go (LList (pred:exprs):rest) = fmap unpackBoolCoerce (evalDeep env pred) >>= \bool ->
      if bool
        then evalExprs env exprs
        else go rest
    go args = throwError $ InvalidArgs "No true condition for 'cond'" args

evalCase :: Env -> LIOFunction
evalCase env (keyArg:clauses) = checkCases clauses
  where
    checkCases :: LIOFunction
    checkCases (LList (LAtom "else":exprs):clauses) = evalExprs env exprs
    checkCases (LList (LList keys  :exprs):clauses) =
      evalDeep env keyArg >>= (`matchKey` keys) >>= \match ->
        if match
          then evalExprs env exprs
          else checkCases clauses
    checkCases (_:clauses) = checkCases clauses
    checkCases [] = throwError $ Default "Exhausted datum matches in 'case'"

    matchKey :: LispVal -> [LispVal] -> IOEvaled Bool
    matchKey _   []       = return False
    matchKey key (v:vals) = eqv env [key, v] >>= \(LBool bool) ->
      if bool then return bool else matchKey key vals

evalCase _ args =
  throwError $ InvalidArgs "'case' excepts 'key' expression followed by 'clauses'" args

eqv :: Env -> LIOFunction
eqv env [arg1, arg2] = do
  a <- evalOnce env arg1
  b <- evalOnce env arg2
  return . LBool $ a == b
eqv _   args         = throwError $ NumArgs 2 args

applyProc :: Env -> LIOFunction
applyProc env [func, LList args] = callFunc env func args
applyProc env (func : args)      = callFunc env func args
applyProc _   _                  = throwError $ InvalidArgs "Invalid arguments to 'apply'" []


------- Utility Functions -------

{-
 - Evaluates all lispval expressions only returning the result of the last
 - evaluated lispval
 -}
evalExprs :: Env -> LIOFunction
evalExprs env = mapMLast (evalDeep env)

makeFunc :: Maybe String -> Env -> [LispVal] -> LIOFunction
makeFunc varargs env params body =
  liftIO . toPtrVal $ LLambdaFunc (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> LIOFunction
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> LIOFunction
makeVarargs = makeFunc . Just . show

mapMLast :: Monad m => (a -> m b) -> [a] -> m b
mapMLast f = fmap last . mapM f
