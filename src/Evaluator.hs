module Evaluator (eval) where

import Control.Monad.Except

import Definition
import LispError
import LispFunction
import Parser
import Variable
import Unpacker (unpackBoolCoerce)


eval :: Env -> LispVal -> IOEvaled LispVal
eval env (LAtom var)           = getVar env var
eval env val@(LList lvs)       = case lvs of

  -- Special Lists
  [LAtom "quote",      vs] -> return vs
  (LAtom "quasiquote": vs) -> LList <$> sequence (evalUnquoted env vs)

  -- Conditional Functions
  [LAtom "if", pred, conseq, alt] -> evalIf env pred conseq alt
  (LAtom "cond" : args)           -> evalCond env args
  (LAtom "case" : args)           -> evalCase env args

  -- Variable Bindings and Mutable Functions
  [LAtom "set!", LAtom var, form]   -> eval env form >>= setVar env var
  [LAtom "define", LAtom var, form] -> eval env form >>= defineVar env var

  -- Regular Functions
  (LAtom func : args) -> mapM (eval env) args >>= apply func

  -- Normal List
  _ -> return val

eval _   val                   = return val

-- Currently unecessary pattern match cases for 'eval':
--   eval _   val@(LString _)       = return val
--   eval _   val@(LNumber _)       = return val
--   eval _   val@(LBool _)         = return val
--   eval _   val@(LChar _)         = return val
--   eval _   val@(LDottedList _ _) = return val


evalUnquoted :: Env -> [LispVal] -> [IOEvaled LispVal]
evalUnquoted env = go
  where go :: [LispVal] -> [IOEvaled LispVal]
        go (LList (LAtom "unquoted":vals):rest) = case vals of
          LAtom "unpack":exprs -> map (eval env) exprs ++ go rest
          [expr]               -> eval env expr : go rest

        go (v:vs) = return v  : go vs
        go []     = []

apply :: LFuncName -> LFunction
apply func args = maybe notFuncErr ($ args) $ lookupFunc func
  where
    notFuncErr :: IOEvaled LispVal
    notFuncErr = throwError $ NotFunction "Unrecognized primitive function args" func

evalIf :: Env -> LispVal -> LispVal -> LispVal -> IOEvaled LispVal
evalIf env pred conseq alt = fmap unpackBoolCoerce (eval env pred) >>= \bool ->
  eval env $ if bool then conseq else alt

evalCond :: Env -> LFunction
evalCond env = go
  where
    go :: LFunction
    go (LList (LAtom "else":exprs):_)    = evalExprs env exprs
    go (LList [val, LAtom "=>", func]:_) = do
      arg <- eval env val
      case func of
        LAtom funcName -> apply funcName [arg]
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


------- Utility Functions -------

{-
 - Evaluates all lispval expressions only returning the result of the last
 - evaluated lispval
 -}
evalExprs :: Env -> [LispVal] -> IOEvaled LispVal
evalExprs env = mapMLast (eval env)

mapMLast :: Monad m => (a -> m b) -> [a] -> m b
mapMLast f = fmap last . mapM f
