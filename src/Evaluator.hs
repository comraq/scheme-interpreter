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
  [LAtom "if", pred, conseq, alt] -> do
    result <- eval pred
    case result of
      LBool False -> eval alt
      _           -> eval conseq
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

