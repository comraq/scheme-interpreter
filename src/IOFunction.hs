module IOFunction (ioFunctions, load) where

import Control.Monad.Except
import System.IO

import Definition
import qualified LispVector as V
import Parser
import Variable


load :: String -> IOEvaled [LispVal]
load filename = liftIO (readFile filename) >>= liftEvaled . readExprList

------- IO Primitive Functions -------

ioFunctions :: [(String, LIOFunction)]
ioFunctions =
  [
  -- Primitive IO Functions
    ("open-input-file"  , makePort ReadMode  )
  , ("open-output-file" , makePort WriteMode )
  , ("close-input-port" , closePort          )
  , ("close-output-port", closePort          )
  , ("read"             , readProc           )
  , ("write"            , writeProc          )
  , ("read-contents"    , readContents       )
  , ("read-all"         , readAll            )

  -- List Functions
  , ("list"             , list               )

  -- String Functions
  , ("make-string"      , makeString         )
  , ("substring"        , substring          )
  , ("string-append"    , stringAppend       )
  , ("list->string"     , listToString       )
  , ("string->list"     , stringToList       )

  -- Vector Functions,
  , ("vector",        vector       )
  , ("make-vector",   makeVector   )
  , ("list->vector",  listToVector )
  , ("vector->list",  vectorToList )
  ]

makePort :: IOMode -> LIOFunction
makePort mode [LString filename] = LPort <$> liftIO (openFile filename mode)
makePort _    args               = throwError $ NumArgs 1 args

closePort :: LIOFunction
closePort [LPort port] = liftIO $  hClose port
                                >> return (LBool True)
closePort _            = return $ LBool False

readProc :: LIOFunction
readProc []           = readProc [LPort stdin]
readProc [LPort port] = liftIO (hGetLine port) >>= liftEvaled . readExpr
readProc args         = throwError $ NumArgs 1 args

writeProc :: LIOFunction
writeProc [obj]             = writeProc [obj, LPort stdout]
writeProc [obj, LPort port] = liftIO $  hPrint port obj
                                     >> return (LBool True)
writeProc args              = throwError $ InvalidArgs "Invalid arguments to 'write'" args

readContents :: LIOFunction
readContents [LString filename] = LString <$> liftIO (readFile filename)
readContents args               = throwError $ NumArgs 1 args

readAll :: LIOFunction
readAll [LString filename] = LList <$> load filename
readAll args               = throwError $ NumArgs 1 args


------- List Functions -------

list :: LIOFunction
list = liftIO . toPtrVal . LList


------- String Functions -------

makeString :: LIOFunction
makeString args = case args of
    [LNumber n]          -> mkStr (fromIntegral n, ' ')
    [LNumber n, LChar c] -> mkStr (fromIntegral n, c)
    _                    -> throwError $ NumArgs 1 args

  where mkStr :: (Int, Char) -> IOEvaled LispVal
        mkStr = liftIO . toPtrVal . LString . uncurry replicate

substring :: LIOFunction
substring [LString str, LNumber start, LNumber end] =
  let startI = fromIntegral $ toInteger start
      sublen = fromIntegral (toInteger end) - startI
  in  liftIO . toPtrVal . LString . take sublen . drop startI $ str
substring args = throwError $ NumArgs 3 args

stringAppend :: LIOFunction
stringAppend args = go args >>= liftIO . toPtrVal
  where go :: LIOFunction
        go []               = return $ LString ""
        go (LString s:strs) = (\(LString s') -> LString $ s ++ s') <$> go strs
        go args             = throwError $ InvalidArgs "Expected string list" args

listToString :: LIOFunction
listToString [LList vals] = toString vals >>= liftIO . toPtrVal . LString
  where toString :: [LispVal] -> IOEvaled String
        toString []            = return ""
        toString (LChar c:lvs) = (c:) <$> toString lvs
        toString args          = throwError $ InvalidArgs "Expected a char list" args
listToString args         = throwError $ InvalidArgs "Expected a char list" args

stringToList :: LIOFunction
stringToList [LString s] = liftIO . toPtrVal . LList $ map LChar s
stringToList args        = throwError $ InvalidArgs "Expected string" args


------- Vector Functions -------

vector :: LIOFunction
vector args = liftIO . toPtrVal . LVector $ V.vector args

makeVector :: LIOFunction
makeVector [LNumber n] =
  liftIO . toPtrVal . LVector $ V.makeVector (fromIntegral n) (LBool False)
makeVector [LNumber n, val] =
  liftIO . toPtrVal . LVector $ V.makeVector (fromIntegral n) val
makeVector args =
  throwError $ InvalidArgs "Expected vector length and optional fill value" args

vectorToList :: LIOFunction
vectorToList [LVector v] = liftIO . toPtrVal . LList $ V.vectorToList v
vectorToList args        = throwError $ NumArgs 1 args

listToVector :: LIOFunction
listToVector [LList vals] = vector vals
listToVector args         = throwError $ NumArgs 1 args
