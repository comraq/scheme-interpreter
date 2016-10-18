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
    ("open-input-file"   , makePort ReadMode  )
  , ("open-output-file"  , makePort WriteMode )
  , ("close-input-port"  , closePort          )
  , ("close-output-port" , closePort          )
  , ("read"              , readProc           )
  , ("write"             , writeProc          )
  , ("read-contents"     , readContents       )
  , ("read-all"          , readAll            )
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
