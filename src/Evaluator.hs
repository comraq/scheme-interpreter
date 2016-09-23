module Evaluator (eval) where

import Definition
import Parser

eval :: LispVal -> LispVal
eval val@(LList lvs) = case lvs of
  [LAtom "quote",      vs] -> vs
  (LAtom "quasiquote": vs) -> LList vs
  (LAtom func : args)      -> apply func $ map eval args
  _                        -> val
eval val@_       = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (LBool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [

             -- Numeric Operations
               ("+",         numericBinop (+) )
             , ("-",         numericBinop (-) )
             , ("*",         numericBinop (*) )
             , ("/",         numericBinop (/) )
             , ("div",       numericBinop div )
             , ("mod",       numericBinop mod )
             , ("quotient",  numericBinop quot)
             , ("remainder", numericBinop rem )

             -- Type Testing
             , ("string?",   isLString        )
             , ("number?",   isLNumber        )
             , ("symbol?",   isLAtom          )

             -- Symbol Handling
             , ("symbol->string", atomToString)
             , ("string->symbol", stringToAtom)
             ]

numericBinop :: (SchemeNumber -> SchemeNumber -> SchemeNumber)
             -> [LispVal] -> LispVal
numericBinop op params = LNumber . foldl1 op $ map unpackNum params

unpackNum :: LispVal -> SchemeNumber
unpackNum (LNumber n) = n
unpackNum (LList [n]) = unpackNum n
unpackNum (LString n) =
  let parsed = reads n
  in  if null parsed
        then SInt 0
        else SInt . fst $ head parsed
unpackNum _ = SInt 0

{-
 - TODO: Return error values for the below (1-arity) functions
 -       if length of input args > 1
 -}
isLString :: [LispVal] -> LispVal
isLString (LString _:_) = LBool True
isLString _             = LBool False

isLNumber :: [LispVal] -> LispVal
isLNumber (LNumber _:_) = LBool True
isLNumber _             = LBool False

isLAtom :: [LispVal] -> LispVal
isLAtom (LAtom _:_) = LBool True
isLAtom _           = LBool False

atomToString :: [LispVal] -> LispVal
atomToString (LAtom a:_) = LString a
atomToString _           = LBool False

stringToAtom :: [LispVal] -> LispVal
stringToAtom (LString s:_) = LAtom s
stringToAtom _             = LBool False
