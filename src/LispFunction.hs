module LispFunction (primitiveFunctions, equivalent) where

import Control.Arrow
import Control.Monad.Except
import Data.Array.ST (runSTArray)
import Data.Char (toLower)
import Data.Foldable (foldrM)

import Definition
import qualified LispVector as V
import Variable (runEvaled)
import Unpacker


------- Primitive Function Mapping Tuples -------

primitiveFunctions :: [(String, LFunction)]
primitiveFunctions =
  [
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
  , ("equal?",       equal             )

  -- Lists/Pairs
  , ("car",  car  )
  , ("cdr",  cdr  )
  , ("cons", cons )

  -- Type Testing
  , ("string?", isLString )
  , ("number?", isLNumber )
  , ("symbol?", isLAtom   )
  , ("vector?", isLVector )

  -- Symbol Handling
  , ("symbol->string", atomToString )
  , ("string->symbol", stringToAtom )

  -- String Functions
  , ("make-string",   makeString   )
  , ("string-length", stringLength )
  , ("string-ref",    stringRef    )
  , ("string-append", stringAppend )
  , ("string->list",  stringToList )
  , ("list->string",  listToString )
  , ("substring",     substring    )

  -- Vector Functions,
  , ("vector",        vector       )
  , ("make-vector",   makeVector   )
  , ("vector-length", vectorLength )
  , ("vector-ref",    vectorRef    )
  , ("list->vector",  listToVector )
  , ("vector->list",  listToVector )
  ]


------- Polymorphic Binary Operations -------

numericBinop :: (SchemeNumber -> SchemeNumber -> SchemeNumber)
             -> LFunction
numericBinop op params = LNumber . foldl1 op <$> mapM unpackNum params

boolBinop :: (LispVal -> Evaled a)
          -> (a -> a -> Bool)
          -> LFunction
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

equivalent :: LFunction
equivalent [LMutable _,       _]                = return $ LBool False
equivalent [_,                LMutable _]       = return $ LBool False
equivalent [LBool arg1,       LBool arg2]       = return . LBool $ arg1 == arg2
equivalent [LChar arg1,       LChar arg2]       = return . LBool $ arg1 == arg2
equivalent [LNumber arg1,     LNumber arg2]     = return . LBool $ arg1 == arg2
equivalent [LString arg1,     LString arg2]     = return . LBool $ arg1 == arg2
equivalent [LAtom arg1,       LAtom arg2]       = return . LBool $ arg1 == arg2
equivalent [LDottedList xs x, LDottedList ys y] =
  equivalent [LList $ xs ++ [x], LList $ ys ++ [y]]
equivalent [LList arg1,       LList arg2]
  | length arg1 /= length arg2 = return $ LBool False
  | otherwise                  = return . LBool $ all eqvPair (zip arg1 arg2)
      where
        eqvPair :: (LispVal, LispVal) -> Bool
        eqvPair (x1, x2) = runEvaled (const False) extractBool $ equivalent [x1, x2]

        extractBool :: LispVal -> Bool
        extractBool (LBool b) = b
        extractBool _         = False

equivalent [_, _]     = return $ LBool False
equivalent badArgList = throwError $ NumArgs 2 badArgList

equal :: LFunction
equal = unwrapMut go
  where
    go :: LFunction
    go [LList xs, LList ys] = return . LBool $ listEqual xs ys
    go [LDottedList xs xlast, LDottedList ys ylast] = do
      (LBool lastEquals) <- equal [xlast, ylast]
      if lastEquals
        then return . LBool $ listEqual xs ys
        else return $ LBool False

    go [arg1, arg2] = do
      primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                                     [ AnyUnpacker unpackNum
                                     , AnyUnpacker (unpackStr id)
                                     , AnyUnpacker unpackBool
                                     ]

      eqvEquals <- equivalent [arg1, arg2]
      return . LBool $ (primitiveEquals || let (LBool x) = eqvEquals in x)
    go badArgList   = throwError $ NumArgs 2 badArgList

listEqual :: [LispVal] -> [LispVal] -> Bool
listEqual xs ys
  | length xs /= length ys = False
  | otherwise              = all equalPair $ zip xs ys
      where equalPair :: (LispVal, LispVal) -> Bool
            equalPair (x, y) = runEvaled (const False)
                                         unpackBoolCoerce
                                         (equal [x, y])


------- Type Testing -------

isLString :: LFunction
isLString [LString _] = return $ LBool True
isLString [val]       = return $ LBool False
isLString vals        = throwError $ NumArgs 1 vals

isLNumber :: LFunction
isLNumber [LNumber _] = return $ LBool True
isLNumber [val]       = return $ LBool False
isLNumber vals        = throwError $ NumArgs 1 vals

isLAtom :: LFunction
isLAtom [LAtom _] = return $ LBool True
isLAtom [val]     = return $ LBool False
isLAtom vals      = throwError $ NumArgs 1 vals

isLVector :: LFunction
isLVector [LVector _] = return $ LBool True
isLVector [val]       = return $ LBool False
isLVector vals        = throwError $ NumArgs 1 vals


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

  where mkStr :: (Int, Char) -> Evaled LispVal
        mkStr = return . LMutable . LString . uncurry replicate

stringLength :: LFunction
stringLength (LMutable v:vs) = stringLength $ v:vs
stringLength [LString s]     = return . LNumber . SInt . toInteger $ length s
stringLength args            = throwError $ NumArgs 1 args

stringRef :: LFunction
stringRef [LString s, LNumber n] =
  let index = fromIntegral $ toInteger n
  in  if length s > index
        then return . LChar $ s !! index
        else throwError $ InvalidArgs "Index is longer than string" [LString s, LNumber n]
stringRef args = throwError $ NumArgs 2 args

substring :: LFunction
substring (LMutable v:vs) = substring $ v:vs
substring [LString str, LNumber start, LNumber end] =
  let startI = fromIntegral $ toInteger start
      sublen = fromIntegral (toInteger end) - startI
  in  return . LMutable . LString . take sublen . drop startI $ str
substring args = throwError $ NumArgs 3 args

stringAppend :: LFunction
stringAppend []               = return . LMutable $ LString ""
stringAppend (LMutable v:vs)  = stringAppend $ v:vs
stringAppend (LString s:strs) = (\(LMutable (LString s')) -> LMutable . LString $ s ++ s') <$> stringAppend strs
stringAppend args             = throwError $ InvalidArgs "Expected string list" args

stringToList :: LFunction
stringToList (LMutable v:vs) = stringToList $ v:vs
stringToList [LString s]     = return . LMutable . LList $ map LChar s
stringToList args            = throwError $ InvalidArgs "Expected string" args

listToString :: LFunction
listToString (LMutable v:vs)  = listToString $ v:vs
listToString [LList lispvals] = LString <$> toString lispvals
  where toString :: [LispVal] -> Evaled String
        toString []            = return ""
        toString (LChar c:lvs) = (c:) <$> toString lvs
        toString args          = throwError $ InvalidArgs "Expected a char list" args
listToString args             = throwError $ InvalidArgs "Expected a char list" args


------- Vector Functions -------

vector :: LFunction
vector args = return . LVector $ runSTArray (V.vector args)

makeVector :: LFunction
makeVector [LNumber n] =
  return . LVector $ runSTArray (V.makeVector (fromIntegral n) $ LBool False)
makeVector [LNumber n, val] =
  return . LVector $ runSTArray (V.makeVector (fromIntegral n) val)
makeVector args =
  throwError $ InvalidArgs "Expected vector length and optional fill value" args

vectorLength :: LFunction
vectorLength [LVector v] = return . LNumber . SInt . toInteger $ V.vectorLength v
vectorLength args        = throwError $ NumArgs 1 args

vectorRef :: LFunction
vectorRef [LVector v, LNumber n] = return . V.vectorRef v $ fromIntegral n
vectorRef args                   = throwError $ NumArgs 2 args

vectorToList :: LFunction
vectorToList [LVector v] = return . LList $ V.vectorToList v
vectorToList args        = throwError $ NumArgs 1 args

listToVector :: LFunction
listToVector [LList vals] = vector vals
listToVector args         = throwError $ NumArgs 1 args


------- Utility Functions -------

allM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
allM f = foldrM (\a b -> (b &&) <$> f a) True

unwrapMut :: LFunction -> LFunction
unwrapMut f args = f $ map unwrap args
  where unwrap :: LispVal -> LispVal
        unwrap (LMutable a) = a
        unwrap a            = a
