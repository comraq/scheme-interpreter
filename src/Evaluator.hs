module Evaluator (eval) where

import Control.Arrow
import Control.Monad.Except
import Data.Array.MArray (thaw)
import Data.Array.ST (runSTArray)
import Data.Maybe (fromMaybe)

import Definition
import LispFunction
import LispVector (vectorSet, vectorFill, vectorLength)
import Parser
import Variable
import Unpacker (unpackBoolCoerce)


eval :: Env -> LispVal -> IOEvaled LispVal
eval env (LAtom var)           = getVar env var
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
    _      -> callFunc env func args

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

callFunc :: Env -> LFuncName -> [LispVal] -> IOEvaled LispVal
callFunc env fName args = join $ getFunc fName <*> mapM (eval env) args

getFunc :: LFuncName -> IOEvaled LFunction
getFunc = (notFuncErr &&& (fmap return . lookupFunc)) >>> uncurry fromMaybe
  where notFuncErr :: LFuncName -> IOEvaled LFunction
        notFuncErr = throwError . NotFunction "Unrecognized function binding"


------- Evaluating Conditional Expressions -------

evalIf :: Env -> LispVal -> LispVal -> LispVal -> IOEvaled LispVal
evalIf env pred conseq alt = fmap unpackBoolCoerce (eval env pred) >>= \bool ->
  eval env $ if bool then conseq else alt

evalCond :: Env -> LFunction
evalCond env = go
  where
    go :: LFunction
    go (LList (LAtom "else":exprs):_)    = evalExprs env exprs
    go (LList [val, LAtom "=>", func]:_) = case func of
      LAtom funcName -> callFunc env funcName [val]
      _              ->
        throwError . NotFunction "'=>' in 'cond' expects a function" $ show func

    go (LList [pred]:_)          = eval env pred
    go (LList (pred:exprs):rest) = fmap unpackBoolCoerce (eval env pred) >>= \bool ->
      if bool
        then evalExprs env exprs
        else go rest
    go args = throwError $ InvalidArgs "No true condition for 'cond'" args

evalCase :: Env -> LFunction
evalCase env (keyArg:clauses) = checkCases clauses
  where
    checkCases :: LFunction
    checkCases (LList (datum:exprs):clauses) = case datum of
      LAtom "else" -> evalExprs env exprs
      LList keys   -> eval env keyArg >>= (`matchKey` keys) >>= \match ->
                        if match
                          then evalExprs env exprs
                          else checkCases clauses
    checkCases args = throwError $ InvalidArgs "Exhausted datum matches in 'case'" args

    matchKey :: LispVal -> [LispVal] -> IOEvaled Bool
    matchKey _   []       = return False
    matchKey key (v:vals) = eqv [key, v] >>= \(LBool bool) ->
      if bool then return bool else matchKey key vals

evalCase _ args =
  throwError $ InvalidArgs "'case' excepts 'key' expression followed by 'clauses'" args


{-
 - ------ Environment Interacting Functions -------
 -
 - > functions involving bindings or mutations
 -}
envFuncs :: [(LFuncName, Env -> LFunction)]
envFuncs =
  [ ("set!"       ,  setBang        )
  , ("define"     ,  define         )
  , ("string-set!",  stringSetBang  )
  , ("vector-set!",  vectorSetBang  )
  , ("vector-fill!", vectorFillBang )
  ]

setBang :: Env -> LFunction
setBang env [LAtom var, form] = eval env form >>= setVar env var
setBang _   args              = throwError $ NumArgs 2 args

define :: Env -> LFunction
define env [LAtom var, form] = eval env form >>= defineVar env var
define _   args              = throwError $ NumArgs 2 args

stringSetBang :: Env -> LFunction
stringSetBang env args@[LAtom expr, _, _] = mapM (eval env) args >>= setStr
  where
    setStr :: LFunction
    setStr [LString str, LNumber i, LChar c]
      | fromIntegral i < length str = setVar env expr . LString $ newStr str i c
      | otherwise      = throwError
                       $ Default "index for 'string-set!' exceeds string length"

    setStr _ = throwError
             $ InvalidArgs
               "'string-set' expects string, number and char"
               args

    newStr :: (Num a, Eq a) => String -> a -> Char -> String
    newStr (x:xs) i c
      | i == 0    = c:xs
      | otherwise = x : newStr xs (i - 1) c

stringSetBang _   args@[x, y, z] = throwError
                             $ InvalidArgs
                               "'string-set' expects a binding to a string"
                               args
stringSetBang _   args = throwError $ NumArgs 3 args

vectorSetBang :: Env -> LFunction
vectorSetBang env args@[LAtom expr, _, _] = mapM (eval env) args >>= setVec
  where
    setVec :: LFunction
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

vectorFillBang :: Env -> LFunction
vectorFillBang env args@[LAtom expr, _] = mapM (eval env) args >>= fillVec
  where
    fillVec :: LFunction
    fillVec [LVector vec, val] = setVar env expr . LVector $
      runSTArray (thaw vec >>= vectorFill val)

    fillVec _ = throwError
              $ InvalidArgs
                "'vector-fill!' expects vector and object"
                args

vectorFillBang _ args = throwError $ NumArgs 3 args


------- Utility Functions -------

{-
 - Evaluates all lispval expressions only returning the result of the last
 - evaluated lispval
 -}
evalExprs :: Env -> [LispVal] -> IOEvaled LispVal
evalExprs env = mapMLast (eval env)

mapMLast :: Monad m => (a -> m b) -> [a] -> m b
mapMLast f = fmap last . mapM f
