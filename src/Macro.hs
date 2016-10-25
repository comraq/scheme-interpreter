module Macro
  ( defineSyntax
  ) where

import Control.Monad.Except

import Definition
import Variable


defineSyntax :: [LispVal] -> Env -> IOEvaled LispVal
defineSyntax [ synId@(LAtom identifier), LList rs ] env = case rs of
  [ LAtom "syntax-rules", LList literals, LList rules ]
    | validateRules rules -> do
      let syntax = LSyntax $ SyntaxRule env (map show literals) rules
      liftIO $ defineVar identifier syntax env
      return synId

    | otherwise -> throwError . BadSpecialForm "Invalid 'syntax-rules' " $ LList rs

  _ -> throwError . BadSpecialForm "Invalid 'syntax-rules' " $ LList rs
defineSyntax args _ = throwError . BadSpecialForm "Invalid 'define-syntax' " $ LList args

validateRules :: [LispVal] -> Bool
validateRules = all validateRule
  where validateRule :: LispVal -> Bool
        validateRule (LList [_, _]) = True
        validateRule _              = False
