{-# LANGUAGE FlexibleContexts #-}

module LispError
  ( LispError(..)
  , Parsed
  , trapError
  , extractValue
  , bindingNotFound
  ) where

import Control.Monad.Except
import Text.Parsec.Error

import Definition

type Parsed a = Except LispError a

data LispError = NumArgs        Integer    [LispVal]
               | TypeMismatch   String     LispVal
               | ParserErr      ParseError
               | BadSpecialForm String     LispVal
               | NotFunction    String     String
               | UnboundVar     String     String
               | InvalidArgs    String     [LispVal]
               | Default        String

instance Show LispError where
  show = showError

showError :: LispError -> String
showError (UnboundVar     message  varname) = message ++ ": " ++ varname
showError (BadSpecialForm message  form)    = message ++ ": " ++ show form
showError (NotFunction    message  func)    = message ++ ": " ++ show func
showError (NumArgs        expected found)   =
  "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch   expected found)   =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (ParserErr      parseErr)         =
  "Parse error at " ++ show parseErr
showError (InvalidArgs    message  args)    = message ++ ", got :" ++ show args
showError (Default        message)          = "Error: " ++ message

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

trapError :: MonadError LispError m => m String -> m String
trapError action = catchError action $ return . show

-- Undefined because 'extractValue' should never be called if error
extractValue :: Parsed a -> a
extractValue = either undefined id . runExcept


------- Helper Functions to Create LispErrors -------

bindingNotFound :: String -> LispError
bindingNotFound = UnboundVar "Cannot find binding"
