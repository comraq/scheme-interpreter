module Main where

import Control.Monad (unless)
import System.Environment
import System.IO

import Evaluator (eval)
import LispError
import Parser (readExpr)

------ Entry Point (Main) -------

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Program only takes 0 to 1 argument(s)!"

mainI :: String -> IO ()
mainI = putStrLn . evalInput

evalInput :: String -> String
evalInput = extractValue . trapError . fmap show . (eval =<<) . readExpr


------- REPL Functions -------

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString = return . evalInput

evalAndPrint :: String -> IO ()
evalAndPrint = putStrLn . evalInput

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >>
                         until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint
