module Main where

import Control.Arrow
import Control.Monad.ST (stToIO, ST, runST)
import Control.Monad (unless)
import System.Environment
import System.IO

import Definition
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


------- REPL Functions -------

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalInputST :: String -> String
evalInputST input = runST $ emptyEnv >>= evalInput input
  where
    getEvaled :: String -> STEvaled s LispVal
    getEvaled = liftEvaled . readExpr

    evalInput :: String -> Env s -> ST s String
    evalInput = curry $ (getEvaled *** eval)
      >>> uncurry (>>=)
      >>> fmap show
      >>> runSTEvaledStr

evalAndPrint :: String -> IO ()
evalAndPrint = putStrLn . evalInputST

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >>
                         until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

runOne :: String -> IO ()
runOne = evalAndPrint
