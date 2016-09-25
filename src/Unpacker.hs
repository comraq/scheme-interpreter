{-# LANGUAGE ExistentialQuantification #-}

module Unpacker where

import Control.Monad.Except

import Definition
import LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> Except LispError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> Except LispError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
  `catchError` const (return False)

unpackNum :: LispVal -> Except LispError SchemeNumber
unpackNum (LNumber n) = return n
unpackNum (LString n) =
  let parsed = reads n
  in  if null parsed
        then throwError $ TypeMismatch "number" $ LString n
        else return . SInt . fst . head $ parsed
unpackNum notNum      = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> Except LispError String
unpackStr (LString s) = return s
unpackStr (LNumber n) = return $ show n
unpackStr (LBool   b) = return $ show b
unpackStr (LChar   c) = return $ show c
unpackStr notString   = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> Except LispError Bool
unpackBool (LBool b) = return b
unpackBool notBool   = throwError $ TypeMismatch "boolean" notBool
