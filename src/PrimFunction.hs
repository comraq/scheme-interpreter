module PrimFunction (primitiveFunctions) where

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
  , ("string-length", stringLength )
  , ("string-ref",    stringRef    )
  , ("string->list",  stringToList )

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

equal :: LFunction
equal = go
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
      return . LBool $ primitiveEquals || (arg1 == arg2)

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

stringLength :: LFunction
stringLength [LString s]     = return . LNumber . SInt . toInteger $ length s
stringLength args            = throwError $ NumArgs 1 args

stringRef :: LFunction
stringRef [LString s, LNumber n] =
  let index = fromIntegral $ toInteger n
  in  if length s > index
        then return . LChar $ s !! index
        else throwError $ InvalidArgs "Index is longer than string" [LString s, LNumber n]
stringRef args = throwError $ NumArgs 2 args

-- TODO: Need to return mutable LPointer list
stringToList :: LFunction
stringToList [LString s]     = return . LList $ map LChar s
stringToList args            = throwError $ InvalidArgs "Expected string" args


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
