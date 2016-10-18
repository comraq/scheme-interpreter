module IOFunction (ioFunctions, load, liftPtr) where

import Control.Monad.Except
import System.IO

import Definition
import qualified LispVector as V
import Parser
import Variable


load :: String -> IOEvaled [LispVal]
load filename = liftIO (readFile filename) >>= liftEvaled . readExprList

liftPtr :: LispVal -> IOEvaled LispVal
liftPtr val = case val of
  LString _       -> liftIO $ toPtrVal val
  LList _         -> liftIO $ toPtrVal val
  LDottedList _ _ -> liftIO $ toPtrVal val
  LVector _       -> liftIO $ toPtrVal val
  _               -> return val

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

  , ("symbol->string" , atomToString )
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
readProc [LPort port] = liftIO (hGetLine port) >>= liftEvaled . readExpr >>= liftPtr
readProc args         = throwError $ NumArgs 1 args

writeProc :: LIOFunction
writeProc [obj]             = writeProc [obj, LPort stdout]
writeProc [obj, LPort port] = liftIO $  hPrint port obj
                                     >> return (LBool True)
writeProc args              = throwError $ InvalidArgs "Invalid arguments to 'write'" args

readContents :: LIOFunction
readContents [LString filename] = liftIO $ LString <$> readFile filename >>= toPtrVal
readContents args               = throwError $ NumArgs 1 args

readAll :: LIOFunction
readAll [LString filename] = fmap LList (load filename) >>= liftIO . toPtrVal
readAll args               = throwError $ NumArgs 1 args

atomToString :: LIOFunction
atomToString [LAtom a] = return $ LString a
atomToString args      = throwError $ NumArgs 1 args
