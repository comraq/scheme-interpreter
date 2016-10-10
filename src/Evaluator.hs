module Evaluator (eval, primitiveEnv) where

import Control.Arrow
import Control.Monad.Except
import Data.Array.MArray (thaw)
import Data.Array.ST (runSTArray)
import Data.Maybe (fromMaybe, isNothing)
import System.IO

import Definition
import LispFunction
import LispVector (vectorSet, vectorFill, vectorLength)
import Parser
import Variable
import Unpacker (unpackBoolCoerce)


type LFuncParams = [LispVal]

eval :: Env -> LispVal -> IOEvaled LispVal
eval env expr = evalDeep env expr >>= evalIfPtr
  where
    evalIfPtr :: LispVal -> IOEvaled LispVal
    evalIfPtr val@(LPointer _) = readPtrVal val
    evalIfPtr val              = return val

evalDeep :: Env -> LispVal -> IOEvaled LispVal
evalDeep env (LAtom var)           = getVarDeep env var
evalDeep env var@(LPointer _)      = readPtrVal var
evalDeep env (LDottedList lvs lst) = LDottedList <$> mapM (evalDeep env) lvs <*> evalDeep env lst
evalDeep env (LVector lvs)         = LVector <$> mapM (evalDeep env) lvs
evalDeep env val@(LList lvs)       = case lvs of

  -- Special Lists
  [LAtom "quote",      vs] -> return vs
  (LAtom "quasiquote": vs) -> LList <$> sequence (evalUnquoted env vs)

  -- Eval as a Function
  (LAtom func : args) -> callFunc env (LAtom func) args

  -- Normal List
  _ -> LList <$> mapM (evalDeep env) lvs

evalDeep _   val                   = return val

evalOnce :: Env -> LispVal -> IOEvaled LispVal
evalOnce env (LAtom var)      = getVar env var
evalOnce env var              = evalDeep env var

evalUnquoted :: Env -> [LispVal] -> [IOEvaled LispVal]
evalUnquoted env = go
  where go :: [LispVal] -> [IOEvaled LispVal]
        go (LList (LAtom "unquoted":vals):rest) = case vals of
          LAtom "unpack":exprs -> map (evalDeep env) exprs ++ go rest
          [expr]               -> evalDeep env expr : go rest

        go (v:vs) = return v  : go vs
        go []     = []


------- Calling and Getting Scheme Functions -------

callFunc :: Env -> LispVal -> LIOFunction
callFunc env func args = evalDeep env func >>= funcApply
  where
    funcApply :: LispVal -> IOEvaled LispVal
    funcApply (LEnvFunc f) = f env args
    funcApply v            = mapM (eval env) args >>= apply v

apply :: LispVal -> LIOFunction
apply (LPrimitiveFunc f)                        args = liftEvaled $ f args
apply (LIOFunc f)                               args = f args
apply (LLambdaFunc params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else (liftIO . bindVars closure $ zip params args) -- Binds param names and function args to lambda closure env, then lift the IO results back to the monad transformers stack
         >>= bindVarArgs varargs                       -- Bind the optional 'varargs' is applicable
         >>= evalBody                                  -- Evals all expressions in the body and returns the results of the last expression

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
    makeFunc = second

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
    setStr [strVal, LNumber i, LChar c] = modifyPtrVal modifyStr strVal
      where
        modifyStr :: LispVal -> IOEvaled LispVal
        modifyStr (LString str)
          | fromIntegral i < length str = return . LString $ newStr str i c
          | otherwise = throwError $ InvalidArgs
                                     "index for 'string-set!' exceeds string length"
                                     args
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
vectorSetBang env args@[LAtom expr, _, _] = mapM (evalDeep env) args >>= setVec
  where
    setVec :: LIOFunction
    setVec [LVector vec, LNumber i, val]
      | fromIntegral i < vectorLength vec = setVar env expr . LVector $
          runSTArray (thaw vec >>= vectorSet (fromIntegral i) val)
      | otherwise = throwError
                  $ Default "index for 'vector-set!' exceeds vector length"

    setVec _ = throwError
             $ InvalidArgs
               "'vector-set!' expects vector, number and object"
               args
vectorSetBang _ args = throwError $ NumArgs 3 args

vectorFillBang :: Env -> LIOFunction
vectorFillBang env args@[LAtom expr, _] = mapM (evalDeep env) args >>= fillVec
  where
    fillVec :: LIOFunction
    fillVec [LVector vec, val] = setVar env expr . LVector $
      runSTArray (thaw vec >>= vectorFill val)

    fillVec _ = throwError
              $ InvalidArgs
                "'vector-fill!' expects vector and object"
                args
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

    eqv :: Env -> LIOFunction
    eqv env = evalDeep env . LList . (LAtom "eqv?":)

evalCase _ args =
  throwError $ InvalidArgs "'case' excepts 'key' expression followed by 'clauses'" args

eqv :: Env -> LIOFunction
eqv env [arg1, arg2] = do
  a <- evalOnce env arg1
  b <- evalOnce env arg2
  return . LBool $ a == b


------- IO Primitive Functions -------

ioFunctions :: [(String, LIOFunction)]
ioFunctions =
  [ ("apply"            , applyProc         )
  , ("open-input-file"  , makePort ReadMode )
  , ("open-output-file" , makePort WriteMode)
  , ("close-input-port" , closePort         )
  , ("close-output-port", closePort         )
  , ("read"             , readProc          )
  , ("write"            , writeProc         )
  , ("read-contents"    , readContents      )
  , ("read-all"         , readAll           )

  , ("make-string"      , makeString        )
  , ("substring"        , substring         )
  , ("string-append"    , stringAppend      )
  , ("list->string"     , listToString      )
  ]

applyProc :: LIOFunction
applyProc [func, LList args] = apply func args
applyProc (func : args)      = apply func args
applyProc _                  = throwError $ InvalidArgs "Invalid arguments to 'apply'" []

makePort :: IOMode -> LIOFunction
makePort mode [LString filename] = LPort <$> liftIO (openFile filename mode)
makePort _    args               = throwError $ NumArgs 1 args

closePort :: LIOFunction
closePort [LPort port] = liftIO $  hClose port
                                >> return (LBool True)
closePort _            = return $ LBool False

readProc :: LIOFunction
readProc []           = readProc [LPort stdin]
readProc [LPort port] = liftIO (hGetLine port) >>= liftEvaled . readExpr
readProc args         = throwError $ NumArgs 1 args

writeProc :: LIOFunction
writeProc [obj]             = writeProc [obj, LPort stdout]
writeProc [obj, LPort port] = liftIO $  hPrint port obj
                                     >> return (LBool True)
writeProc args              = throwError $ InvalidArgs "Invalid arguments to 'write'" args

readContents :: LIOFunction
readContents [LString filename] = LString <$> liftIO (readFile filename)
readContents args               = throwError $ NumArgs 1 args

readAll :: LIOFunction
readAll [LString filename] = LList <$> load filename
readAll args               = throwError $ NumArgs 1 args

makeString :: LIOFunction
makeString args = case args of
    [LNumber n]          -> mkStr (fromIntegral n, ' ')
    [LNumber n, LChar c] -> mkStr (fromIntegral n, c)
    _                    -> throwError $ NumArgs 1 args

  where mkStr :: (Int, Char) -> IOEvaled LispVal
        mkStr = liftIO . toPtrVal . LString . uncurry replicate

substring :: LIOFunction
substring [LString str, LNumber start, LNumber end] =
  let startI = fromIntegral $ toInteger start
      sublen = fromIntegral (toInteger end) - startI
  in  liftIO . toPtrVal . LString . take sublen . drop startI $ str
substring args = throwError $ NumArgs 3 args

stringAppend :: LIOFunction
stringAppend args = go args >>= liftIO . toPtrVal
  where go :: LIOFunction
        go []               = return $ LString ""
        go (LString s:strs) = (\(LString s') -> LString $ s ++ s') <$> go strs
        go args             = throwError $ InvalidArgs "Expected string list" args

listToString :: LIOFunction
listToString [LList vals] = toString vals >>= liftIO . toPtrVal . LString
  where toString :: [LispVal] -> IOEvaled String
        toString []            = return ""
        toString (LChar c:lvs) = (c:) <$> toString lvs
        toString args          = throwError $ InvalidArgs "Expected a char list" args
listToString args         = throwError $ InvalidArgs "Expected a char list" args



load :: String -> IOEvaled [LispVal]
load filename = liftIO (readFile filename) >>= liftEvaled . readExprList


------- Utility Functions -------

{-
 - Evaluates all lispval expressions only returning the result of the last
 - evaluated lispval
 -}
evalExprs :: Env -> LIOFunction
evalExprs env = mapMLast (evalDeep env)

makeFunc :: Maybe String -> Env -> LFuncParams -> LIOFunction
makeFunc varargs env params body =
  return $ LLambdaFunc (map show params) varargs body env

makeNormalFunc :: Env -> LFuncParams -> LIOFunction
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> LFuncParams -> LIOFunction
makeVarargs = makeFunc . Just . show

mapMLast :: Monad m => (a -> m b) -> [a] -> m b
mapMLast f = fmap last . mapM f
