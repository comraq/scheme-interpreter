module Evaluator (eval) where

import Control.Monad.Except

import Definition
import LispError
import LispFunction
import Parser
import Unpacker


eval :: LispVal -> Evaled LispVal
eval val@(LString _)       = return val
eval val@(LNumber _)       = return val
eval val@(LBool _)         = return val
eval val@(LChar _)         = return val
eval val@(LDottedList _ _) = return val

eval val@(LList lvs)       = case lvs of
  [LAtom "quote",      vs]        -> return vs
  (LAtom "quasiquote": vs)        -> LList <$> sequence (evalUnquoted vs)
  [LAtom "if", pred, conseq, alt] -> evalIf pred conseq alt
  (LAtom "cond" : args)           -> evalCond args
  (LAtom "case" : args)           -> evalCase args
  (LAtom func : args)             -> mapM eval args >>= apply func
  _                               -> return val

eval badForm               =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

evalUnquoted :: [LispVal] -> [Evaled LispVal]
evalUnquoted (LList (LAtom "unquoted":vals):rest) = case vals of
  LAtom "unpack":exprs -> map eval exprs ++ evalUnquoted rest
  [expr]               -> eval expr : evalUnquoted rest

evalUnquoted (v:vs) = return v  : evalUnquoted vs
evalUnquoted []     = []

apply :: LFuncName -> LFunction
apply func args = maybe notFuncErr ($ args) $ lookupFunc func
  where
    notFuncErr :: Evaled LispVal
    notFuncErr = throwError $ NotFunction "Unrecognized primitive function args" func

evalIf :: LispVal -> LispVal -> LispVal -> Evaled LispVal
evalIf pred conseq alt = fmap unpackBoolCoerce (eval pred) >>= \bool ->
  eval $ if bool then conseq else alt

evalCond :: LFunction
evalCond (LList (LAtom "else":exprs):_)    = mapMLast eval exprs
evalCond (LList [val, LAtom "=>", func]:_) = do
  arg <- eval val
  case func of
    LAtom funcName -> apply funcName [arg]
    _              ->
      throwError . NotFunction "'=>' in 'cond' expects a function" $ show func

evalCond (LList [pred]:_)          = eval pred
evalCond (LList (pred:exprs):rest) = fmap unpackBoolCoerce (eval pred) >>= \bool ->
  if bool
    then mapMLast eval exprs
    else evalCond rest
evalCond args = throwError $ InvalidArgs "No true condition for 'cond'" args

evalCase :: LFunction
evalCase (keyArg:clauses) = checkCases clauses
  where checkCases :: LFunction
        checkCases (LList (datum:exprs):clauses) = case datum of
          LAtom "else" -> mapMLast eval exprs
          LList keys   -> eval keyArg >>= (`matchKey` keys) >>= \match ->
                            if match
                              then mapMLast eval exprs
                              else checkCases clauses
        checkCases args = throwError $ InvalidArgs "Exhausted datum matches in 'case'" args

        matchKey :: LispVal -> [LispVal] -> Evaled Bool
        matchKey _   []       = return False
        matchKey key (v:vals) = eqv [key, v] >>= \(LBool bool) ->
          if bool then return bool else matchKey key vals

evalCase args =
  throwError $ InvalidArgs "'case' excepts 'key' expression followed by 'clauses'" args


------- Utility Functions -------

mapMLast :: Monad m => (a -> m b) -> [a] -> m b
mapMLast f = fmap last . mapM f
