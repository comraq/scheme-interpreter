module Main where

import Control.Arrow
import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import System.Console.Haskeline
import System.Environment
import System.IO

import Evaluator (eval)
import Parser (readExpr)
import Variable (Env, emptyEnv, liftParsed, runIOEvaledSafe)

------ Entry Point (Main) -------

main :: IO ()
main = do
  args <- getArgs
  runInputT defaultSettings $ case length args of
    0 -> runRepl
    1 -> runOne $ head args
    _ -> outputStrLn "Program only takes 0 to 1 argument(s)!"

mainI :: String -> IO ()
mainI = runInputT defaultSettings . runOne


------- REPL Functions -------

readPrompt :: MonadException m => String -> InputT m String
readPrompt = fmap (fromMaybe "") . getInputLine

evalInputIO :: Env -> String -> IO String
evalInputIO env = runIOEvaledSafe . fmap show . (eval env =<<) . parseExpr
  where parseExpr = liftParsed . readExpr

evalInputT :: Env -> String -> InputT IO String
evalInputT env = liftIO . evalInputIO env

evalAndPrint :: Env -> String -> InputT IO ()
evalAndPrint env str = evalInputT env str >>= outputStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >>
                         until_ pred prompt action

runRepl :: InputT IO ()
runRepl = liftIO emptyEnv >>= until_ userQuits (readPrompt "Lisp>>> ") . evalAndPrint
  where userQuits :: String -> Bool
        userQuits = ((== "quit") &&& (== ":q")) >>> uncurry (||)

runOne :: String -> InputT IO ()
runOne expr = liftIO emptyEnv >>= (`evalAndPrint` expr)
