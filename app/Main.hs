module Main where

import Control.Monad (unless)
import System.Environment
import System.IO

import Evaluator (eval)
import Parser (readExpr)
import Variable (Env, emptyEnv, liftParsed, runIOEvaledSafe)

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


------- REPL Functions -------

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalInput :: Env -> String -> IO String
evalInput env = runIOEvaledSafe . fmap show . (eval env =<<) . parseExpr
  where parseExpr = liftParsed . readExpr

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env str = evalInput env str >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >>
                         until_ pred prompt action

runRepl :: IO ()
runRepl = emptyEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runOne :: String -> IO ()
runOne expr = emptyEnv >>= (`evalAndPrint` expr)
