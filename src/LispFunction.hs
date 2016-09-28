module LispFunction
  ( LFuncName
  , LFunction
  , lookupFunc
  , eqv
  ) where

import Control.Monad.Except
import Control.Monad.ST
import Data.Char (toLower)
import Data.Foldable (foldrM)

import Definition
import LispError
import Variable (STEvaled, runSTEvaled)
import Unpacker

type LFuncName = String
type LFunction s = [LispVal] -> STEvaled s LispVal

lookupFunc :: LFuncName -> Maybe (LFunction s)
lookupFunc name = lookup name functionsMap

------- Function Mapping Tuples -------

functionsMap :: [(String, LFunction s)]
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
             -> LFunction s
numericBinop op params = LNumber . foldl1 op <$> mapM unpackNum params

boolBinop :: (LispVal -> STEvaled s a)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> STEvaled s LispVal
boolBinop unpacker op args = if length args /= 2
                               then throwError $ NumArgs 2 args
                               else do left  <- unpacker $ head args
                                       right <- unpacker $ args !! 1
                                       return . LBool $ left `op` right

numBoolBinop :: (SchemeNumber -> SchemeNumber -> Bool) -> LFunction s
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String) -> (String -> String -> Bool) -> LFunction s
strBoolBinop = boolBinop . unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> LFunction s
boolBoolBinop = boolBinop unpackBool


------- List/Pair Operations -------

car :: LFunction s
car [LList (x:xs)]         = return x
car [LDottedList (x:xs) _] = return x
car [badArg]               = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: LFunction s
cdr [LList (_:xs)]         = return $ LList xs
cdr [LDottedList (_:xs) x] = return $ LDottedList xs x
cdr [badArg]               = throwError $ TypeMismatch "pair" badArg
cdr badArgList             = throwError $ NumArgs 1 badArgList

cons :: LFunction s
cons [x, LList []]             = return $ LList [x]
cons [x, LList xs]             = return . LList $ x:xs
cons [x, LDottedList xs xlast] = return $ LDottedList (x:xs) xlast
cons [x1, x2]                  = return $ LDottedList [x1] x2
cons badArgList                = throwError $ NumArgs 2 badArgList


------- Equality Checks -------

eqv :: LFunction s
eqv [LBool arg1,       LBool arg2]       = return . LBool $ arg1 == arg2
eqv [LChar arg1,       LChar arg2]       = return . LBool $ arg1 == arg2
eqv [LNumber arg1,     LNumber arg2]     = return . LBool $ arg1 == arg2
eqv [LString arg1,     LString arg2]     = return . LBool $ arg1 == arg2
eqv [LAtom arg1,       LAtom arg2]       = return . LBool $ arg1 == arg2
eqv [LDottedList xs x, LDottedList ys y] = eqv [LList $ xs ++ [x], LList $ ys ++ [y]]
eqv [LList arg1,       LList arg2]
  | length arg1 /= length arg2 = return $ LBool False
  | otherwise                  = lift $ LBool <$> allM eqvPair (zip arg1 arg2)
      where
        eqvPair :: (LispVal, LispVal) -> ST s Bool
        eqvPair (x1, x2) = runSTEvaled (const False) extractBool $ eqv [x1, x2]

        extractBool :: LispVal -> Bool
        extractBool (LBool b) = b
        extractBool _         = False

eqv [_,                _]                = return $ LBool False
eqv badArgList                           = throwError $ NumArgs 2 badArgList

equal :: LFunction s
equal [LList xs, LList ys] = lift . fmap LBool $ listEqual xs ys
equal [LDottedList xs xlast, LDottedList ys ylast] = do
  (LBool lastEquals) <- equal [xlast, ylast]
  if lastEquals
    then lift $ LBool <$> listEqual xs ys
    else return $ LBool False

equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                                 [ AnyUnpacker unpackNum
                                 , AnyUnpacker (unpackStr id)
                                 , AnyUnpacker unpackBool
                                 ]

  eqvEquals <- eqv [arg1, arg2]
  return . LBool $ (primitiveEquals || let (LBool x) = eqvEquals in x)
equal badArgList   = throwError $ NumArgs 2 badArgList

listEqual :: [LispVal] -> [LispVal] -> ST s Bool
listEqual xs ys
  | length xs /= length ys = return False
  | otherwise              = allM equalPair $ zip xs ys
      where equalPair :: (LispVal, LispVal) -> ST s Bool
            equalPair (x, y) = runSTEvaled (const False)
                                           unpackBoolCoerce
                                         $ equal [x, y]

------- Type Testing -------

isLString :: LFunction s
isLString [LString _] = return $ LBool True
isLString vals        = throwError $ NumArgs 1 vals

isLNumber :: LFunction s
isLNumber [LNumber _] = return $ LBool True
isLNumber vals        = throwError $ NumArgs 1 vals

isLAtom :: LFunction s
isLAtom [LAtom _] = return $ LBool True
isLAtom vals      = throwError $ NumArgs 1 vals


------- Symbol Handling -------

atomToString :: LFunction s
atomToString [LAtom a] = return $ LString a
atomToString vals      = throwError $ NumArgs 1 vals

stringToAtom :: LFunction s
stringToAtom [LString s] = return $ LAtom s
stringToAtom vals        = throwError $ NumArgs 1 vals


------- String Functions -------

makeString :: LFunction s
makeString args = case args of
    [LNumber n]          -> mkStr (fromIntegral n, ' ')
    [LNumber n, LChar c] -> mkStr (fromIntegral n, c)
    _                    -> throwError $ NumArgs 1 args

  where mkStr :: (Int, Char) -> STEvaled s LispVal
        mkStr = return . LString . uncurry replicate

stringLength :: LFunction s
stringLength [LString s] = return . LNumber . SInt . fromIntegral $ length s
stringLength args        = throwError $ NumArgs 1 args

stringRef :: LFunction s
stringRef [LString s, LNumber n] =
  let index = fromIntegral $ toInteger n
  in  if length s > index
        then return . LChar $ s !! index
        else throwError $ InvalidArgs "Index is longer than string" [LString s, LNumber n]
stringRef args = throwError $ NumArgs 2 args

substring :: LFunction s
substring [LString str, LNumber start, LNumber end] =
  let s = fromIntegral $ toInteger start
      e = fromIntegral (toInteger end) - s
  in  return . LString . take e . drop s $ str
substring args = throwError $ NumArgs 3 args

stringAppend :: LFunction s
stringAppend []               = return $ LString ""
stringAppend (LString s:strs) = (\(LString s') -> LString $ s ++ s') <$> stringAppend strs
stringAppend args             = throwError $ InvalidArgs "Expected string list" args

stringToList :: LFunction s
stringToList [LString s] = return . LList . map LChar $ s
stringToList args        = throwError $ InvalidArgs "Expected string" args

listToString :: LFunction s
listToString [LList lispvals] = LString <$> toString lispvals
  where toString :: [LispVal] -> STEvaled s String
        toString []            = return ""
        toString (LChar c:lvs) = (c:) <$> toString lvs
        toString args          = throwError $ InvalidArgs "Expected a char list" args
listToString args             = throwError $ InvalidArgs "Expected a char list" args


------- Utility Functions -------
allM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
allM f = foldrM (\a b -> (b &&) <$> f a) True
