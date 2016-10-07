module Main where

import Control.Arrow
import Control.Monad (unless, void)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import System.Console.Haskeline
import System.Environment
import System.IO

import Definition
import Evaluator (eval, primitiveEnv)
import Parser (readExpr)
import Variable (liftEvaled, runIOEvaledSafe, bindVars)


------ Entry Point (Main) -------

main :: IO ()
main = do
  args <- getArgs
  runInputT defaultSettings $ if null args
                                then runRepl
                                else runCmd args

mainI :: String -> IO ()
mainI = runInputT defaultSettings . runOne


------- REPL Functions -------

runRepl :: InputT IO ()
runRepl = liftIO primitiveEnv >>= until_ userQuits (readPrompt "Lisp>>> ") . evalAndPrint
  where userQuits :: String -> Bool
        userQuits = ((== "quit") &&& (== ":q")) >>> uncurry (||)

runOne :: String -> InputT IO ()
runOne expr = liftIO primitiveEnv >>= (`evalAndPrint` expr)

runCmd :: [String] -> InputT IO ()
runCmd args = do
    env    <- liftIO $ (`bindVars` argsList) =<< primitiveEnv
    output <- liftIO $ runIOEvaledSafe (show <$> load env [filename])
    liftIO $ hPutStrLn stderr output

  where
    argsList :: [(String, LispVal)]
    argsList = [("args", LList . map LString $ drop 1 args)]

    filename :: LispVal
    filename = LString $ head args

    load :: Env -> LIOFunction
    load env = eval env . LList . (LAtom "load":)


------- Helper Functions -------

readPrompt :: MonadException m => String -> InputT m String
readPrompt = fmap (fromMaybe "") . getInputLine

evalInputIO :: Env -> String -> IO String
evalInputIO env = runIOEvaledSafe . fmap show . (eval env =<<) . parseExpr
  where parseExpr = liftEvaled . readExpr

evalInputT :: Env -> String -> InputT IO String
evalInputT env = liftIO . evalInputIO env

evalAndPrint :: Env -> String -> InputT IO ()
evalAndPrint env ""  = return ()
evalAndPrint env str = evalInputT env str >>= outputStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >>
                         until_ pred prompt action
