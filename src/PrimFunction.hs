module PrimFunction (primitiveFunctions) where

import Control.Monad.Except
import Data.Array.IArray ((!), bounds)
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
    ("+"         , numericBinop (+)  )
  , ("-"         , numericBinop (-)  )
  , ("*"         , numericBinop (*)  )
  , ("/"         , numericBinop (/)  )
  , ("div"       , numericBinop div  )
  , ("mod"       , numericBinop mod  )
  , ("quotient"  , numericBinop quot )
  , ("remainder" , numericBinop rem  )

  -- Operations Resulting in Boolean Equalities
  , ("="            , numBoolBinop (==)               )
  , ("<"            , numBoolBinop (<)                )
  , (">"            , numBoolBinop (>)                )
  , ("/="           , numBoolBinop (/=)               )
  , (">="           , numBoolBinop (>=)               )
  , ("<="           , numBoolBinop (<=)               )
  , ("&&"           , boolBoolBinop (&&)              )
  , ("||"           , boolBoolBinop (||)              )
  , ("string=?"     , strBoolBinop id (==)            )
  , ("string<=?"    , strBoolBinop id (<=)            )
  , ("string>=?"    , strBoolBinop id (>=)            )
  , ("string<?"     , strBoolBinop id (<)             )
  , ("string>?"     , strBoolBinop id (>)             )
  , ("string-ci=?"  , strBoolBinop (map toLower) (==) )
  , ("string-ci<=?" , strBoolBinop (map toLower) (<=) )
  , ("string-ci>=?" , strBoolBinop (map toLower) (>=) )
  , ("string-ci<?"  , strBoolBinop (map toLower) (<)  )
  , ("string-ci>?"  , strBoolBinop (map toLower) (>)  )
  , ("equal?"       , equal                           )

  -- Lists/Pairs
  , ("car"  , car  )
  , ("cdr"  , cdr  )
  , ("cons" , cons )

  -- Type Testing
  , ("string?"    , isLString   )
  , ("number?"    , isLNumber   )
  , ("symbol?"    , isLAtom     )
  , ("boolean?"   , isLBoolean  )
  , ("char?"      , isLChar     )
  , ("port?"      , isLPort     )
  , ("pair?"      , isPair      )
  , ("vector?"    , isLVector   )
  , ("procedure?" , isProcedure )
  , ("list?"      , isLList     )

  -- Symbol Handling
  , ("string->symbol" , stringToAtom )

  -- String Functions
  , ("make-string"   , makeString   )
  , ("substring"     , substring    )
  , ("string-append" , stringAppend )
  , ("list->string"  , listToString )
  , ("string->list"  , stringToList )
  , ("string-length" , stringLength )
  , ("string-ref"    , stringRef    )

  -- Vector Functions,
  , ("vector"        , vector       )
  , ("make-vector"   , makeVector   )
  , ("list->vector"  , listToVector )
  , ("vector->list"  , vectorToList )
  , ("vector-length" , vectorLength )
  , ("vector-ref"    , vectorRef    )
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

-- TODO: Currently 'equal' coerces values before checking equality
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
    go [LVector xs, LVector ys] = return . LBool $ vectorEqual xs ys

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

equalPair :: (LispVal, LispVal) -> Bool
equalPair (x, y) = runEvaled (const False) unpackBoolCoerce (equal [x, y])

vectorEqual :: SVector LispVal -> SVector LispVal -> Bool
vectorEqual xs ys =
  let (_, xHigh) = bounds xs
      (_, yHigh) = bounds ys
      allEqual i = (i == xHigh) ||
                     (equalPair (xs ! i, ys ! i) && allEqual (i + 1))

  in  xHigh == yHigh && allEqual 0

------- Type Testing -------

isLString :: LFunction
isLString [LString _] = return $ LBool True
isLString [_]         = return $ LBool False
isLString vals        = throwError $ NumArgs 1 vals

isLNumber :: LFunction
isLNumber [LNumber _] = return $ LBool True
isLNumber [_]         = return $ LBool False
isLNumber vals        = throwError $ NumArgs 1 vals

isLBoolean :: LFunction
isLBoolean [LBool _] = return $ LBool True
isLBoolean [_]       = return $ LBool False
isLBoolean vals      = throwError $ NumArgs 1 vals

isLChar :: LFunction
isLChar [LChar _] = return $ LBool True
isLChar [_]       = return $ LBool False
isLChar vals      = throwError $ NumArgs 1 vals

isLPort :: LFunction
isLPort [LPort _] = return $ LBool True
isLPort [_]       = return $ LBool False
isLPort vals      = throwError $ NumArgs 1 vals

isLAtom :: LFunction
isLAtom [LAtom _] = return $ LBool True
isLAtom [_]       = return $ LBool False
isLAtom vals      = throwError $ NumArgs 1 vals

isLList :: LFunction
isLList [LList _] = return $ LBool True
isLList [_]       = return $ LBool False
isLList vals      = throwError $ NumArgs 1 vals

isLDottedList :: LFunction
isLDottedList [LDottedList _ _] = return $ LBool True
isLDottedList [_]               = return $ LBool False
isLDottedList vals              = throwError $ NumArgs 1 vals

isPair :: LFunction
isPair [LDottedList _ _] = return $ LBool True
isPair [LList _]         = return $ LBool True
isPair [_]               = return $ LBool False
isPair vals              = throwError $ NumArgs 1 vals

isLVector :: LFunction
isLVector [LVector _] = return $ LBool True
isLVector [_]         = return $ LBool False
isLVector vals        = throwError $ NumArgs 1 vals

isProcedure :: LFunction
isProcedure [LPrimitiveFunc _ _] = return $ LBool True
isProcedure [LIOFunc        _ _] = return $ LBool True
isProcedure [LEnvFunc       _ _] = return $ LBool True
isProcedure [LLambdaFunc    {} ] = return $ LBool True
isProcedure [_]                  = return $ LBool False
isProcedure vals                 = throwError $ NumArgs 1 vals


------- Symbol Handling -------

stringToAtom :: LFunction
stringToAtom [LString s] = return $ LAtom s
stringToAtom vals        = throwError $ NumArgs 1 vals


------- String Functions -------

makeString :: LFunction
makeString args = case args of
    [LNumber n]          -> return $ mkStr (fromIntegral n, ' ')
    [LNumber n, LChar c] -> return $ mkStr (fromIntegral n, c)
    _                    -> throwError $ NumArgs 1 args

  where mkStr :: (Int, Char) -> LispVal
        mkStr = LString . uncurry replicate

substring :: LFunction
substring [LString str, LNumber start, LNumber end] =
  let startI = fromIntegral $ toInteger start
      sublen = fromIntegral (toInteger end) - startI
  in  return . LString . take sublen . drop startI $ str
substring args = throwError $ NumArgs 3 args

stringAppend :: LFunction
stringAppend = go
  where go :: LFunction
        go []               = return $ LString ""
        go (LString s:strs) = (\(LString s') -> LString $ s ++ s') <$> go strs
        go args             = throwError $ InvalidArgs "Expected string list" args

listToString :: LFunction
listToString [LList vals] = LString <$> toString vals
  where toString :: [LispVal] -> Evaled String
        toString []            = return ""
        toString (LChar c:lvs) = (c:) <$> toString lvs
        toString args          = throwError $ InvalidArgs "Expected a char list" args
listToString args         = throwError $ InvalidArgs "Expected a char list" args

stringToList :: LFunction
stringToList [LString s] = return . LList $ map LChar s
stringToList args        = throwError $ InvalidArgs "Expected string" args

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


------- Vector Functions -------

vector :: LFunction
vector args = return . LVector $ V.vector args

makeVector :: LFunction
makeVector [LNumber n] = return . LVector $ V.makeVector (fromIntegral n) (LBool False)
makeVector [LNumber n, val] = return . LVector $ V.makeVector (fromIntegral n) val
makeVector args =
  throwError $ InvalidArgs "Expected vector length and optional fill value" args

vectorToList :: LFunction
vectorToList [LVector v] = return . LList $ V.vectorToList v
vectorToList args        = throwError $ NumArgs 1 args

listToVector :: LFunction
listToVector [LList vals] = vector vals
listToVector args         = throwError $ NumArgs 1 args

vectorLength :: LFunction
vectorLength [LVector v] = return . LNumber . SInt . toInteger $ V.vectorLength v
vectorLength args        = throwError $ NumArgs 1 args

vectorRef :: LFunction
vectorRef [LVector v, LNumber n] = return . V.vectorRef v $ fromIntegral n
vectorRef args                   = throwError $ NumArgs 2 args


------- Utility Functions -------

allM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
allM f = foldrM (\a b -> (b &&) <$> f a) True
