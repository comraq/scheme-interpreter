module Evaluator (eval) where

import Control.Monad.Except

import Definition
import LispError
import LispFunction
import Parser
import Unpacker


eval :: LispVal -> Except LispError LispVal
eval val@(LString _)       = return val
eval val@(LNumber _)       = return val
eval val@(LBool _)         = return val
eval val@(LChar _)         = return val
eval val@(LDottedList _ _) = return val

eval val@(LList lvs)       = case lvs of
  [LAtom "quote",      vs] -> return vs
  (LAtom "quasiquote": vs) -> LList <$> sequence (evalUnquoted vs)
  [LAtom "if", pred, conseq, alt] -> evalIf pred conseq alt
  (LAtom "cond" : args)    -> evalCond args
  (LAtom func : args)      -> mapM eval args >>= apply func
  _                        -> return val

eval badForm               =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

evalUnquoted :: [LispVal] -> [Except LispError LispVal]
evalUnquoted (LList (LAtom "unquoted":vals):rest) = case vals of
  LAtom "unpack":exprs -> map eval exprs ++ evalUnquoted rest
  [expr]               -> eval expr : evalUnquoted rest

evalUnquoted (v:vs) = return v  : evalUnquoted vs
evalUnquoted []     = []

apply :: LFuncName -> LFunction
apply func args = maybe notFuncErr ($ args) $ lookupFunc func
  where
    notFuncErr :: Except LispError LispVal
    notFuncErr = throwError $ NotFunction "Unrecognized primitive function args" func

evalIf :: LispVal -> LispVal -> LispVal -> Except LispError LispVal
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

mapMLast :: Monad m => (a -> m b) -> [a] -> m b
mapMLast f [x]    = f x
mapMLast f (x:xs) = f x >> mapMLast f xs
