module Evaluator (eval) where

import Control.Monad.Except

import Definition
import LispError
import Parser

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

apply :: String -> [LispVal] -> Except LispError LispVal
apply func args = maybe notFuncErr ($ args) $ lookup func primitives
  where
    notFuncErr :: Except LispError LispVal
    notFuncErr = throwError $ NotFunction "Unrecognized primitive function args" func

primitives :: [(String, [LispVal] -> Except LispError LispVal)]
primitives = [
             -- Numeric Operations
               ("+",         numericBinop (+)  )
             , ("-",         numericBinop (-)  )
             , ("*",         numericBinop (*)  )
             , ("/",         numericBinop (/)  )
             , ("div",       numericBinop div  )
             , ("mod",       numericBinop mod  )
             , ("quotient",  numericBinop quot )
             , ("remainder", numericBinop rem  )

             -- Operations Resulting in Boolean Equalities
             , ("=",         numBoolBinop (==) )
             , ("<",         numBoolBinop (<)  )
             , (">",         numBoolBinop (>)  )
             , ("/=",        numBoolBinop (/=) )
             , (">=",        numBoolBinop (>=) )
             , ("<=",        numBoolBinop (<=) )
             , ("&&",        boolBoolBinop (&&))
             , ("||",        boolBoolBinop (||))
             , ("string=?",  strBoolBinop (==) )
             , ("string<=?", strBoolBinop (<=) )
             , ("string>=?", strBoolBinop (>=) )
             , ("string<?",  strBoolBinop (<) )
             , ("string>?",  strBoolBinop (>) )

             -- Type Testing
             , ("string?",   isLString         )
             , ("number?",   isLNumber         )
             , ("symbol?",   isLAtom           )

             -- Symbol Handling
             , ("symbol->string", atomToString)
             , ("string->symbol", stringToAtom)
             ]

numericBinop :: (SchemeNumber -> SchemeNumber -> SchemeNumber)
             -> [LispVal] -> Except LispError LispVal
numericBinop op params = LNumber . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> Except LispError SchemeNumber
unpackNum (LNumber n) = return n
unpackNum (LList [n]) = unpackNum n
unpackNum (LString n) =
  let parsed = reads n
  in  if null parsed
        then throwError $ TypeMismatch "number" $ LString n
        else return . SInt . fst . head $ parsed
unpackNum notNum      = throwError $ TypeMismatch "number" notNum

boolBinop :: (LispVal -> Except LispError a)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> Except LispError LispVal
boolBinop unpacker op args = if length args /= 2
                               then throwError $ NumArgs 2 args
                               else do left  <- unpacker $ head args
                                       right <- unpacker $ args !! 1
                                       return . LBool $ left `op` right

numBoolBinop :: (SchemeNumber -> SchemeNumber -> Bool) -> [LispVal] -> Except LispError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> Except LispError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> Except LispError LispVal
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> Except LispError String
unpackStr (LString s) = return s
unpackStr (LNumber n) = return $ show n
unpackStr (LBool   b) = return $ show b
unpackStr notString   = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> Except LispError Bool
unpackBool (LBool b) = return b
unpackBool notBool   = throwError $ TypeMismatch "boolean" notBool


isLString :: [LispVal] -> Except LispError LispVal
isLString [LString _] = return $ LBool True
isLString vals        = throwError $ NumArgs 1 vals

isLNumber :: [LispVal] -> Except LispError LispVal
isLNumber [LNumber _] = return $ LBool True
isLNumber vals        = throwError $ NumArgs 1 vals

isLAtom :: [LispVal] -> Except LispError LispVal
isLAtom [LAtom _] = return $ LBool True
isLAtom vals      = throwError $ NumArgs 1 vals

atomToString :: [LispVal] -> Except LispError LispVal
atomToString [LAtom a] = return $ LString a
atomToString vals      = throwError $ NumArgs 1 vals

stringToAtom :: [LispVal] -> Except LispError LispVal
stringToAtom [LString s] = return $ LAtom s
stringToAtom vals        = throwError $ NumArgs 1 vals
