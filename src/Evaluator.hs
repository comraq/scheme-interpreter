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
eval env (LAtom var)           = getVar env var
eval env (LMutable val)        = eval env val
eval env (LDottedList lvs lst) = LDottedList <$> mapM (eval env) lvs <*> eval env lst
eval env (LVector lvs)         = LVector <$> mapM (eval env) lvs
eval env val@(LList lvs)       = case lvs of

  -- Special Lists
  [LAtom "quote",      vs] -> return vs
  (LAtom "quasiquote": vs) -> LList <$> sequence (evalUnquoted env vs)

  -- Conditional Functions
  [LAtom "if", pred, conseq, alt] -> evalIf env pred conseq alt
  (LAtom "cond" : args)           -> evalCond env args
  (LAtom "case" : args)           -> evalCase env args

  -- Eval as a Function
  (LAtom func : args) -> case lookup func envFuncs of
    Just f -> f env args
    _      -> callFunc env (LAtom func) args

  -- Normal List
  _ -> LList <$> mapM (eval env) lvs

eval _   val                   = return val

evalUnquoted :: Env -> [LispVal] -> [IOEvaled LispVal]
evalUnquoted env = go
  where go :: [LispVal] -> [IOEvaled LispVal]
        go (LList (LAtom "unquoted":vals):rest) = case vals of
          LAtom "unpack":exprs -> map (eval env) exprs ++ go rest
          [expr]               -> eval env expr : go rest

        go (v:vs) = return v  : go vs
        go []     = []


------- Calling and Getting Scheme Functions -------

callFunc :: Env -> LispVal -> LIOFunction
callFunc env func args = join $ apply <$> eval env func <*> mapM (eval env) args

apply :: LispVal -> LIOFunction
apply (LPrimitiveFunc f)                        args = liftEvaled $ f args
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
    evalBody env = mapMLast (eval env) body

    bindVarArgs :: Maybe String -> Env -> IOEvaled Env
    bindVarArgs Nothing        env = return env
    bindVarArgs (Just argName) env =
      liftIO $ bindVars env [(argName, LList remainingArgs)]

    remainingArgs :: [LispVal]
    remainingArgs = drop (length params) args


------- Evaluating Conditional Expressions -------

evalIf :: Env -> LispVal -> LispVal -> LispVal -> IOEvaled LispVal
evalIf env pred conseq alt = fmap unpackBoolCoerce (eval env pred) >>= \bool ->
  eval env $ if bool then conseq else alt

evalCond :: Env -> LIOFunction
evalCond env = go
  where
    go :: LIOFunction
    go (LList (LAtom "else":exprs):_)    = evalExprs env exprs
    go (LList [val, LAtom "=>", func]:_) = case func of
      LAtom _ -> callFunc env func [val]
      _       ->
        throwError . NotFunction "'=>' in 'cond' expects a function" $ show func

    go (LList [pred]:_)          = eval env pred
    go (LList (pred:exprs):rest) = fmap unpackBoolCoerce (eval env pred) >>= \bool ->
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
      eval env keyArg >>= (`matchKey` keys) >>= \match ->
        if match
          then evalExprs env exprs
          else checkCases clauses
    checkCases (_:clauses) = checkCases clauses
    checkCases [] = throwError $ Default "Exhausted datum matches in 'case'"

    matchKey :: LispVal -> [LispVal] -> IOEvaled Bool
    matchKey _   []       = return False
    matchKey key (v:vals) = eqv [key, v] >>= \(LBool bool) ->
      if bool then return bool else matchKey key vals

    eqv :: LIOFunction
    eqv = eval env . LList . (LAtom "eqv?":)

evalCase _ args =
  throwError $ InvalidArgs "'case' excepts 'key' expression followed by 'clauses'" args


{-
 - ------ Environment Interacting Functions -------
 -
 - > functions involving bindings or mutations
 -}
envFuncs :: [(LFuncName, Env -> LIOFunction)]
envFuncs =
  [ ("set!"        , setBang        )
  , ("define"      , define         )
  , ("lambda"      , lambda         )
  , ("string-set!" , stringSetBang  )
  , ("vector-set!" , vectorSetBang  )
  , ("vector-fill!", vectorFillBang )
  , ("load"        , loadProc       )
  ]

setBang :: Env -> LIOFunction
setBang env [LAtom var, form] = eval env form >>= setVar env var
setBang _   args              = throwError $ NumArgs 2 args

define :: Env -> LIOFunction

-- 'define' a function binding
define env (LList (LAtom func:params):body) =
  makeNormalFunc env params body >>= defineVar env func
define env (LDottedList (LAtom func:params) varargs:body) =
  makeVarargs varargs env params body >>= defineVar env func

-- 'define' a variable binding
define env [LAtom var, form] = eval env form >>= defineVar env var
define _   args              = throwError $ NumArgs 2 args

lambda :: Env -> LIOFunction
lambda env (LList params:body)               = makeNormalFunc env params body
lambda env (LDottedList params varargs:body) = makeVarargs varargs env params body
lambda env (varargs@(LAtom _):body)          = makeVarargs varargs env []     body
lambda _ args = throwError $ InvalidArgs "Bad lambda expression" args

stringSetBang :: Env -> LIOFunction
stringSetBang env args = case args of
    LAtom var:[_, _] -> mapM (eval env) args >>= setStr >>= setVar env var
    [_, _, _]        -> mapM (eval env) args >>= setStr
    _                -> throwError $ NumArgs 3 args
  where
    setStr :: LIOFunction
    setStr [LMutable (LString str), LNumber i, LChar c]
      | fromIntegral i < length str = return . LMutable . LString $ newStr str i c
      | otherwise      = throwError
                       $ Default "index for 'string-set!' exceeds string length"

    setStr (LString str:_) = throwError $ ImmutableArg "expects mutable string" (LString str)
    setStr _ = throwError
             $ InvalidArgs
               "'string-set' expects mutable string, number and char"
               args

    newStr :: (Num a, Eq a) => String -> a -> Char -> String
    newStr (x:xs) i c
      | i == 0    = c:xs
      | otherwise = x : newStr xs (i - 1) c

vectorSetBang :: Env -> LIOFunction
vectorSetBang env args@[LAtom expr, _, _] = mapM (eval env) args >>= setVec
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
vectorFillBang env args@[LAtom expr, _] = mapM (eval env) args >>= fillVec
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
loadProc env [LString filename] = load filename >>= mapMLast (eval env)


------- Environment with both Primitive and IOPrimitive Function Bindings -------

primitiveEnv :: IO Env
primitiveEnv = emptyEnv >>= (`bindVars` (primFuncs ++ ioPrimFuncs))
  where
    makeFunc = second

    primFuncs :: [(String, LispVal)]
    primFuncs = map (makeFunc LPrimitiveFunc) primitiveFuncs

    ioPrimFuncs :: [(String, LispVal)]
    ioPrimFuncs = map (makeFunc LIOFunc) ioPrimitiveFuncs

ioPrimitiveFuncs :: [(String, LIOFunction)]
ioPrimitiveFuncs =
  [ ("apply",             applyProc         )
  , ("open-input-file",   makePort ReadMode )
  , ("open-output-file",  makePort WriteMode)
  , ("close-input-port",  closePort         )
  , ("close-output-port", closePort         )
  , ("read",              readProc          )
  , ("write",             writeProc         )
  , ("read-contents",     readContents      )
  , ("read-all",          readAll           )
  ]


------- IO Primitive Functions -------

applyProc :: LIOFunction
applyProc [func, LList args] = apply func args
applyProc (func : args)      = apply func args

makePort :: IOMode -> LIOFunction
makePort mode [LString filename] = LPort <$> liftIO (openFile filename mode)

closePort :: LIOFunction
closePort [LPort port] = liftIO $  hClose port
                                >> return (LBool True)
closePort _            = return $ LBool False

readProc :: LIOFunction
readProc []           = readProc [LPort stdin]
readProc [LPort port] = liftIO (hGetLine port) >>= liftEvaled . readExpr

writeProc :: LIOFunction
writeProc [obj]             = writeProc [obj, LPort stdout]
writeProc [obj, LPort port] = liftIO $  hPrint port obj
                                     >> return (LBool True)

readContents :: LIOFunction
readContents [LString filename] = LString <$> liftIO (readFile filename)

readAll :: LIOFunction
readAll [LString filename] = LList <$> load filename

load :: String -> IOEvaled [LispVal]
load filename = liftIO (readFile filename) >>= liftEvaled . readExprList


------- Utility Functions -------

{-
 - Evaluates all lispval expressions only returning the result of the last
 - evaluated lispval
 -}
evalExprs :: Env -> LIOFunction
evalExprs env = mapMLast (eval env)

makeFunc :: Maybe String -> Env -> LFuncParams -> LIOFunction
makeFunc varargs env params body = return $ LLambdaFunc (map show params)
                                                        varargs
                                                        body
                                                        env

makeNormalFunc :: Env -> LFuncParams -> LIOFunction
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> LFuncParams -> LIOFunction
makeVarargs = makeFunc . Just . show

mapMLast :: Monad m => (a -> m b) -> [a] -> m b
mapMLast f = fmap last . mapM f
