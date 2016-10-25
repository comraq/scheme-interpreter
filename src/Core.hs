{-# LANGUAGE TupleSections #-}

module Core (eval, primitiveEnv) where

import Control.Arrow ((&&&), first)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe (fromMaybe, isNothing)

import Definition
import IOFunction
import PrimFunction
import LispVector (vectorSet, vectorFill, vectorLength)
import Parser
import Variable
import Unpacker (unpackBoolCoerce)


-- Pseudo Continuation/Callback for evaluating quasiquoted expressions
type QuasiquoteCont = LispVal -> IOEvaled LispVal


eval :: Env -> LispVal -> IOEvaled LispVal
eval env expr = runReaderT (evalDeep expr) env >>= derefPtrValSafe

evalDeep :: LispVal -> EnvEvaled LispVal
evalDeep (LAtom var)           = liftIOEvaled $ getVarDeep var
evalDeep var@(LNumber _)       = return var
evalDeep var@(LString _)       = return var
evalDeep var@(LBool _)         = return var
evalDeep var@(LChar _)         = return var

evalDeep var@(LPointer _)      = lift $ derefPtrVal var

evalDeep val@(LList lvs)       = case lvs of
  -- Special Lists
  [LAtom "quote",      vs] -> return vs
  [LAtom "quasiquote", vs] -> evalQuasiquote vs

  -- Eval as a Function
  (func : args) -> callFunc func args

evalDeep var@(LDottedList _ _) = liftThrowErr $ BadSpecialForm "Literal dotted lists must be quoted!" var
evalDeep var@(LVector _)       = liftThrowErr $ BadSpecialForm "Literal vectors must be quoted!" var
evalDeep badForm               = liftThrowErr $ BadSpecialForm "Unrecognized special form" badForm

evalOnce :: LispVal -> EnvEvaled LispVal
evalOnce (LAtom var) = liftIOEvaled $ getVar var
evalOnce var         = evalDeep var

evalQuasiquote :: LispVal -> EnvEvaled LispVal
evalQuasiquote lispval = case lispval of
    LList [LAtom "unquote-splicing", _] ->
      liftThrowErr $ BadSpecialForm "unquote-splicing found outside of list" lispval

    _ -> do
      env <- ask
      let cont val = runReaderT (evalDeep val) env
      lift $ evalUnquotedCont cont lispval

evalUnquotedListCont :: QuasiquoteCont -> [LispVal] -> IOEvaled ([LispVal], Maybe LispVal)
evalUnquotedListCont cont [ LList [LAtom "unquote-splicing", expr] ] = do
  evaled <- cont expr
  case evaled of
    LList vs         -> return (vs, Nothing    )
    LDottedList vs v -> return (vs, Just v     )
    _                -> return ([], Just evaled)
evalUnquotedListCont cont (LList [LAtom "unquote-splicing", expr]:vals) =
        first . (++)
    <$> (cont expr >>= unpackSpliced)
    <*> evalUnquotedListCont cont vals
  where unpackSpliced :: LispVal -> IOEvaled [LispVal]
        unpackSpliced (LList vs) = return vs
        unpackSpliced v          =
          throwError $ TypeMismatch "unquote-splicing evaluated to non-list" v
evalUnquotedListCont cont (val:vals) =
      first . (:)
  <$> evalUnquotedCont cont val
  <*> evalUnquotedListCont cont vals
evalUnquotedListCont _     _          = return ([], Nothing)

evalUnquotedCont :: QuasiquoteCont -> LispVal -> IOEvaled LispVal
evalUnquotedCont cont (LDottedList hd tl) = do
  evaledHead <- evalUnquotedListCont cont hd
  case evaledHead of
    (vs, Nothing) -> LDottedList vs <$> evalUnquotedCont cont tl
    (_,  Just v ) -> throwError $ TypeMismatch "unquote-splicing evaluated to non-list" v
evalUnquotedCont cont (LList vs) = case vs of
    [LAtom "unquote", expr]    ->  cont expr
    [LAtom "quasiquote", expr] ->  (\v -> LList [LAtom "quasiquote", v])
                               <$> evalUnquotedCont (fmap wrapInUnquote . evalUnquotedCont cont) expr

    _                          ->  makeLispVal <$> evalUnquotedListCont cont vs
  where
    makeLispVal :: ([LispVal], Maybe LispVal) -> LispVal
    makeLispVal (vals, Just val) = LDottedList vals val
    makeLispVal (vals, _       ) = LList vals

    wrapInUnquote :: LispVal -> LispVal
    wrapInUnquote v = LList [LAtom "unquote", v]

evalUnquotedCont _    lispval = return lispval


------- Calling and Getting Scheme Functions -------

callFunc :: LispVal -> LEnvFunction
callFunc func args = evalDeep func >>= funcApply
  where
    funcApply :: LispVal -> EnvEvaled LispVal
    funcApply (LEnvFunc _ f)   = f args
    funcApply f@LLambdaFunc {} = mapM evalOnce args >>= lift . apply f
    funcApply f                = mapM evalDeep args >>= lift . apply f

apply :: LispVal -> LIOFunction
apply (LPrimitiveFunc _ f)                      args =
  mapM derefPtrValSafe args >>= liftEvaled . f >>= liftPtr
apply (LIOFunc _ f)                             args = f args
apply (LLambdaFunc params varargs body closure) args
  | isNothing varargs && num params /= num args = throwError $ NumArgs (num params) args
  | num params > num args                       = throwError $ NumArgs (num params) args
  | otherwise                                   =
      -- Binds param names and function args to lambda closure env,
      -- then lift the IO results back to the monad transformers stack
      zip params <$> mapM derefPtrValSafe args
      >>= liftIO . bindVars closure

      -- Bind the optional 'varargs' is applicable
      >>= bindVarArgs varargs

      -- Evals all expressions in the body and
      -- returns the results of the last expression
      >>= evalBody
      >>= liftPtr

  where
    num :: [a] -> Int
    num = length

    evalBody :: Env -> IOEvaled LispVal
    evalBody = runReaderT (evalExprs body)

    bindVarArgs :: Maybe String -> Env -> IOEvaled Env
    bindVarArgs Nothing        env = return env
    bindVarArgs (Just argName) env =
      liftIO $ bindVars env [(argName, LList remainingArgs)]

    remainingArgs :: [LispVal]
    remainingArgs = drop (length params) args

apply expr _ = throwError . NotFunction "Trying to call non-function" $ show expr


------- Environment with all Function Preset Bindings -------

primitiveEnv :: IO Env
primitiveEnv = emptyEnv >>= (`mBindVars` (primFuncs ++ envFuncs ++ ioFuncs))
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

envFunctions :: [(LFuncName, LEnvFunction)]
envFunctions =
  [ ("define" , define    )
  , ("lambda" , lambda    )
  , ("apply"  , applyProc )
  , ("load"   , loadProc  )
  , ("if"     , evalIf    )
  , ("cond"   , evalCond  )
  , ("case"   , evalCase  )

  , ("set!"         , setBang        )
  , ("string-set!"  , stringSetBang  )
  , ("set-car!"     , setCarBang     )
  , ("set-cdr!"     , setCdrBang     )
  , ("vector-set!"  , vectorSetBang  )
  , ("vector-fill!" , vectorFillBang )

  , ("eq?"  , eqv )
  , ("eqv?" , eqv )
  ]

define :: LEnvFunction
define args = case args of
    -- 'define' a function binding
    LList (LAtom fnName:params):body               -> makeNormalFunc params body >>= defineBinding fnName
    LDottedList (LAtom fnName:params) varargs:body -> makeVarargs varargs params body >>= defineBinding fnName

    -- 'define' a variable binding
    [LAtom var, form]                              -> evalOnce form >>= defineBinding var
    _                                              -> liftThrowErr $ NumArgs 2 args

  where defineBinding :: VarName -> LispVal -> EnvEvaled LispVal
        defineBinding name val = ask >>= liftIO . defineVar name val

lambda :: LEnvFunction
lambda (LList params:body)               = makeNormalFunc params body
lambda (LDottedList params varargs:body) = makeVarargs varargs params body
lambda (varargs@(LAtom _):body)          = makeVarargs varargs []     body
lambda args = liftThrowErr $ InvalidArgs "Bad lambda expression" args

applyProc :: LEnvFunction
applyProc [func, LList args] = callFunc func args
applyProc (func : args)      = callFunc func args
applyProc _                  = liftThrowErr $ InvalidArgs "Invalid arguments to 'apply'" []

loadProc :: LEnvFunction
loadProc [LString filename] = lift (load filename) >>= evalExprs

evalIf :: LEnvFunction
evalIf [pred, conseq, alt] = fmap unpackBoolCoerce (evalDeep pred) >>= \bool ->
  evalDeep $ if bool then conseq else alt
evalIf args = liftThrowErr $ NumArgs 3 args

evalCond :: LEnvFunction
evalCond (LList (LAtom "else":exprs):_)    = evalExprs exprs
evalCond (LList [val, LAtom "=>", expr]:_) = evalDeep expr >>= (`callFunc` [val])
evalCond (LList [pred]:_)          = evalDeep pred
evalCond (LList (pred:exprs):rest) =
  fmap unpackBoolCoerce (evalDeep pred) >>= \bool ->
    if bool
      then evalExprs exprs
      else evalCond rest
evalCond args = liftThrowErr $ InvalidArgs "No true condition for 'cond'" args

evalCase :: LEnvFunction
evalCase (keyArg:clauses) = checkCases clauses
  where
    checkCases :: LEnvFunction
    checkCases (LList (LAtom "else":exprs):clauses) = evalExprs exprs
    checkCases (LList (LList keys  :exprs):clauses) =
      evalDeep keyArg >>= (`matchKey` keys) >>= \match ->
        if match
          then evalExprs exprs
          else checkCases clauses
    checkCases (_:clauses) = checkCases clauses
    checkCases [] = liftThrowErr $ Default "Exhausted datum matches in 'case'"

    matchKey :: LispVal -> [LispVal] -> EnvEvaled Bool
    matchKey _   []       = return False
    matchKey key (v:vals) = eqv [key, v] >>= \(LBool bool) ->
      if bool
        then return bool
        else matchKey key vals

evalCase args =
  liftThrowErr $ InvalidArgs "'case' excepts 'key' expression followed by 'clauses'" args

setBang :: LEnvFunction
setBang [LAtom var, form] = evalDeep form >>= liftIOEvaled . setVar var
setBang args              = liftThrowErr $ NumArgs 2 args

stringSetBang :: LEnvFunction
stringSetBang args@[_, _, _] = mapM evalOnce args >>= lift . setStr
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
stringSetBang args           = liftThrowErr $ NumArgs 3 args

setCarBang :: LEnvFunction
setCarBang args@[_, _] = mapM evalOnce args >>= lift . setCar
  where
    setCar :: LIOFunction
    setCar [ptr, newVal] = modifyPtrVal modifyList ptr
      where
        modifyList :: LispVal -> IOEvaled LispVal
        modifyList (LList vs)          = LList <$> replaceHd newVal vs
        modifyList (LDottedList hd tl) = (`LDottedList` tl) <$> replaceHd newVal hd
        modifyList _                   = throwError badArgs

    replaceHd :: LispVal -> [LispVal] -> IOEvaled [LispVal]
    replaceHd v (_:vs) = return $ v:vs
    replaceHd _ _      = throwError badArgs

    badArgs :: LispError
    badArgs = InvalidArgs "'set-car!' expects non-empty pair/list and an object" args
setCarBang args = liftThrowErr $ NumArgs 2 args

setCdrBang :: LEnvFunction
setCdrBang args@[_, _] = mapM evalOnce args >>= lift . setCdr
  where
    setCdr :: LIOFunction
    setCdr [ptr, newVal] = modifyPtrVal modifyList ptr
      where
        modifyList :: LispVal -> IOEvaled LispVal
        modifyList (LList vs)         = replaceTl newVal vs
        modifyList (LDottedList hd _) = replaceTl newVal hd
        modifyList _                  = throwError badArgs

    replaceTl :: LispVal -> [LispVal] -> IOEvaled LispVal
    replaceTl v (hd:_) = case v of
      LList vs          -> return $ LList (hd:vs)
      LDottedList vs tl -> return $ LDottedList (hd:vs) tl
      _                 -> return $ LDottedList [hd] v
    replaceTl _ _      = throwError badArgs

    badArgs :: LispError
    badArgs = InvalidArgs "'set-cdr!' expects non-empty pair/list and an object" args
setCdrBang args = liftThrowErr $ NumArgs 3 args

vectorSetBang :: LEnvFunction
vectorSetBang args@[_, _, _] = mapM evalOnce args >>= lift . setVec
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
vectorSetBang args = throwError $ NumArgs 3 args

vectorFillBang :: LEnvFunction
vectorFillBang args@[_, _] = mapM evalOnce args >>= lift . fillVec
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
vectorFillBang args = liftThrowErr $ NumArgs 3 args

eqv :: LEnvFunction
eqv [arg1, arg2] = do
  a <- evalOnce arg1
  b <- evalOnce arg2
  return . LBool $ a == b
eqv args         = liftThrowErr $ NumArgs 2 args


------- Utility Functions -------

{-
 - Evaluates all lispval expressions only returning the result of the last
 - evaluated lispval
 -}
evalExprs :: LEnvFunction
evalExprs = mapMLast evalDeep

makeFunc :: Maybe String -> [LispVal] -> [LispVal] -> Env -> IOEvaled LispVal
makeFunc varargs params body env =
  liftIO . toPtrVal $ LLambdaFunc (map show params) varargs body env

makeNormalFunc :: [LispVal] -> LEnvFunction
makeNormalFunc params body = liftIOEvaled $ makeFunc Nothing params body

makeVarargs :: LispVal -> [LispVal] -> LEnvFunction
makeVarargs vararg params body =
  let varargs = Just $ show vararg
  in  liftIOEvaled $ makeFunc varargs params body

mapMLast :: Monad m => (a -> m b) -> [a] -> m b
mapMLast f = fmap last . mapM f

liftThrowErr :: LispError -> EnvEvaled a
liftThrowErr = lift . throwError

liftIOEvaled :: (Env -> IOEvaled a) -> EnvEvaled a
liftIOEvaled f = ask >>= lift . f
