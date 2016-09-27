module Main where

import Control.Monad (unless)
import System.Environment
import System.IO

import Evaluator (eval)
import LispError
import Parser (readExpr)
import Variable

------ Entry Point (Main) -------

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ head args
    _ -> putStrLn "Program only takes 0 to 1 argument(s)!"

mainI :: String -> IO ()
mainI = runOne

-- evalInput :: String -> String
-- evalInput = extractValue . trapError . fmap show . (eval =<<) . readExpr

evalInputIO :: Env -> String -> IO String
evalInputIO env = runIOEvaledStr . fmap show . (eval env =<<) . liftEvaled . readExpr


------- REPL Functions -------

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalInputIO env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >>
                         until_ pred prompt action

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runOne :: String -> IO ()
runOne expr = nullEnv >>= (`evalAndPrint` expr)
