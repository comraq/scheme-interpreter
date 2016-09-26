{-# LANGUAGE FlexibleContexts #-}

module LispError
  ( LispError(..)
  , trapError
  , extractValue
  ) where

import Control.Monad.Except
import Text.Parsec.Error

import Definition

data LispError = NumArgs        Integer    [LispVal]
               | TypeMismatch   String     LispVal
               | Parser         ParseError
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
showError (Parser         parseErr)         =
  "Parse error at " ++ show parseErr
showError (InvalidArgs    message  args)    = message ++ ", got :" ++ show args

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

trapError :: MonadError LispError m => m String -> m String
trapError action = catchError action $ return . show

extractValue :: Except LispError a -> a
extractValue = either undefined id . runExcept
