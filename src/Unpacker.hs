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

unpackStr :: (String -> String) -> LispVal -> Except LispError String
unpackStr f (LString s) = return $ f s
unpackStr f (LNumber n) = return . f $ show n
unpackStr f (LBool   b) = return . f $ show b
unpackStr f (LChar   c) = return . f $ show c
unpackStr _ notString   = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> Except LispError Bool
unpackBool (LBool b) = return b
unpackBool notBool   = throwError $ TypeMismatch "boolean" notBool

unpackBoolCoerce :: LispVal -> Bool
unpackBoolCoerce (LBool False) = False
unpackBoolCoerce _             = True
