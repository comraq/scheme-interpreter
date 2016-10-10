module IOFunction (ioFunctions, load) where

import Control.Monad.Except
import System.IO

import Definition
import Parser
import Variable


load :: String -> IOEvaled [LispVal]
load filename = liftIO (readFile filename) >>= liftEvaled . readExprList

------- IO Primitive Functions -------

ioFunctions :: [(String, LIOFunction)]
ioFunctions =
  [ --("apply"            , applyProc         )
    ("open-input-file"  , makePort ReadMode )
  , ("open-output-file" , makePort WriteMode)
  , ("close-input-port" , closePort         )
  , ("close-output-port", closePort         )
  , ("read"             , readProc          )
  , ("write"            , writeProc         )
  , ("read-contents"    , readContents      )
  , ("read-all"         , readAll           )

  , ("make-string"      , makeString        )
  , ("substring"        , substring         )
  , ("string-append"    , stringAppend      )
  , ("list->string"     , listToString      )
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
