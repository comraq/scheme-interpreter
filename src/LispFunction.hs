module LispFunction
  ( LFuncName
  , LFunction
  , lookupFunc
  ) where

import Control.Monad.Except
import Data.Char (toLower)

import Definition
import LispError
import Unpacker

type LFuncName = String
type LFunction = [LispVal] -> Except LispError LispVal

lookupFunc :: LFuncName -> Maybe LFunction
lookupFunc name = lookup name functionsMap

------- Function Mapping Tuples -------

functionsMap :: [(String, LFunction)]
functionsMap = [
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
             , ("=",            numBoolBinop (==) )
             , ("<",            numBoolBinop (<)  )
             , (">",            numBoolBinop (>)  )
             , ("/=",           numBoolBinop (/=) )
             , (">=",           numBoolBinop (>=) )
             , ("<=",           numBoolBinop (<=) )
             , ("&&",           boolBoolBinop (&&))
             , ("||",           boolBoolBinop (||))
             , ("string=?",     strBoolBinop id (==) )
             , ("string<=?",    strBoolBinop id (<=) )
             , ("string>=?",    strBoolBinop id (>=) )
             , ("string<?",     strBoolBinop id (<)  )
             , ("string>?",     strBoolBinop id (>)  )
             , ("string-ci=?",  strBoolBinop (map toLower) (==) )
             , ("string-ci<=?", strBoolBinop (map toLower) (<=) )
             , ("string-ci>=?", strBoolBinop (map toLower) (>=) )
             , ("string-ci<?",  strBoolBinop (map toLower) (<)  )
             , ("string-ci>?",  strBoolBinop (map toLower) (>)  )
             , ("eq?",          eqv               )
             , ("eqv?",         eqv               )
             , ("equal?",       equal             )

             -- Lists/Pairs
             , ("car",  car  )
             , ("cdr",  cdr  )
             , ("cons", cons )

             -- Type Testing
             , ("string?", isLString )
             , ("number?", isLNumber )
             , ("symbol?", isLAtom   )

             -- Symbol Handling
             , ("symbol->string", atomToString )
             , ("string->symbol", stringToAtom )

             -- String Functions
             , ("make-string",   makeString   )
             , ("string-length", stringLength )
             , ("string-ref",    stringRef    )
             , ("string-append", stringAppend )
             , ("string-list",   stringToList )
             , ("list-string",   listToString )
             , ("substring",     substring    )
             ]


------- Polymorphic Binary Operations -------

numericBinop :: (SchemeNumber -> SchemeNumber -> SchemeNumber)
             -> LFunction
numericBinop op params = LNumber . foldl1 op <$> mapM unpackNum params

boolBinop :: (LispVal -> Except LispError a)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> Except LispError LispVal
boolBinop unpacker op args = if length args /= 2
                               then throwError $ NumArgs 2 args
                               else do left  <- unpacker $ head args
                                       right <- unpacker $ args !! 1
                                       return . LBool $ left `op` right

numBoolBinop :: (SchemeNumber -> SchemeNumber -> Bool) -> LFunction
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String) -> (String -> String -> Bool) -> LFunction
strBoolBinop = boolBinop . unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> LFunction
boolBoolBinop = boolBinop unpackBool


------- List/Pair Operations -------

car :: LFunction
car [LList (x:xs)]         = return x
car [LDottedList (x:xs) _] = return x
car [badArg]               = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: LFunction
cdr [LList (_:xs)]         = return $ LList xs
cdr [LDottedList (_:xs) x] = return $ LDottedList xs x
cdr [badArg]               = throwError $ TypeMismatch "pair" badArg
cdr badArgList             = throwError $ NumArgs 1 badArgList

cons :: LFunction
cons [x, LList []]             = return $ LList [x]
cons [x, LList xs]             = return . LList $ x:xs
cons [x, LDottedList xs xlast] = return $ LDottedList (x:xs) xlast
cons [x1, x2]                  = return $ LDottedList [x1] x2
cons badArgList                = throwError $ NumArgs 2 badArgList


------- Equality Checks -------

eqv :: LFunction
eqv [LBool arg1,       LBool arg2]       = return . LBool $ arg1 == arg2
eqv [LChar arg1,       LChar arg2]       = return . LBool $ arg1 == arg2
eqv [LNumber arg1,     LNumber arg2]     = return . LBool $ arg1 == arg2
eqv [LString arg1,     LString arg2]     = return . LBool $ arg1 == arg2
eqv [LAtom arg1,       LAtom arg2]       = return . LBool $ arg1 == arg2
eqv [LDottedList xs x, LDottedList ys y] = eqv [LList $ xs ++ [x], LList $ ys ++ [y]]
eqv [LList arg1,       LList arg2]       = return . LBool $
    (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where eqvPair :: (LispVal, LispVal) -> Bool
        eqvPair (x1, x2) = case runExcept $ eqv [x1, x2] of
          Left err          -> False
          Right (LBool val) -> val
eqv [_,                _]                = return $ LBool False
eqv badArgList                           = throwError $ NumArgs 2 badArgList

equal :: LFunction
equal [LList xs, LList ys] = return . LBool $ listEqual xs ys
equal [LDottedList xs xlast, LDottedList ys ylast] = do
  (LBool lastEquals) <- equal [xlast, ylast]
  return . LBool $ lastEquals && listEqual xs ys
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                                 [ AnyUnpacker unpackNum
                                 , AnyUnpacker (unpackStr id)
                                 , AnyUnpacker unpackBool
                                 ]

  eqvEquals <- eqv [arg1, arg2]
  return . LBool $ (primitiveEquals || let (LBool x) = eqvEquals in x)
equal badArgList   = throwError $ NumArgs 2 badArgList

listEqual :: [LispVal] -> [LispVal] -> Bool
listEqual []     []     = True
listEqual (x:xs) (y:ys) = case runExcept $ equal [x, y] of
  Right (LBool True) -> listEqual xs ys
  _                  -> False
listEqual _      _      = False


------- Type Testing -------

isLString :: LFunction
isLString [LString _] = return $ LBool True
isLString vals        = throwError $ NumArgs 1 vals

isLNumber :: LFunction
isLNumber [LNumber _] = return $ LBool True
isLNumber vals        = throwError $ NumArgs 1 vals

isLAtom :: LFunction
isLAtom [LAtom _] = return $ LBool True
isLAtom vals      = throwError $ NumArgs 1 vals


------- Symbol Handling -------

atomToString :: LFunction
atomToString [LAtom a] = return $ LString a
atomToString vals      = throwError $ NumArgs 1 vals

stringToAtom :: LFunction
stringToAtom [LString s] = return $ LAtom s
stringToAtom vals        = throwError $ NumArgs 1 vals


------- String Functions -------

makeString :: LFunction
makeString args = case args of
    [LNumber n]          -> mkStr (fromIntegral n, ' ')
    [LNumber n, LChar c] -> mkStr (fromIntegral n, c)
    _                    -> throwError $ NumArgs 1 args

  where mkStr :: (Int, Char) -> Except LispError LispVal
        mkStr = return . LString . uncurry replicate

stringLength :: LFunction
stringLength [LString s] = return . LNumber . SInt . fromIntegral $ length s
stringLength args        = throwError $ NumArgs 1 args

stringRef :: LFunction
stringRef [LString s, LNumber n] =
  let index = fromIntegral $ toInteger n
  in  if length s > index
        then return . LChar $ s !! index
        else throwError $ InvalidArgs "Index is longer than string" [LString s, LNumber n]
stringRef args = throwError $ NumArgs 2 args

substring :: LFunction
substring [LString str, LNumber start, LNumber end] =
  let s = fromIntegral $ toInteger start
      e = fromIntegral (toInteger end) - s
  in  return . LString . take e . drop s $ str
substring args = throwError $ NumArgs 3 args

stringAppend :: LFunction
stringAppend []               = return $ LString ""
stringAppend (LString s:strs) = (\(LString s') -> LString $ s ++ s') <$> stringAppend strs
stringAppend args             = throwError $ InvalidArgs "Expected string list" args

stringToList :: LFunction
stringToList [LString s] = return . LList . map LChar $ s
stringToList args        = throwError $ InvalidArgs "Expected string" args

listToString :: LFunction
listToString [LList lispvals] = LString <$> toString lispvals
  where toString :: [LispVal] -> Except LispError String
        toString []            = return ""
        toString (LChar c:lvs) = (c:) <$> toString lvs
        toString args          = throwError $ InvalidArgs "Expected a char list" args
listToString args             = throwError $ InvalidArgs "Expected a char list" args
