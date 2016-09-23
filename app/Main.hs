module Main where

import System.Environment

import Evaluator (eval)
import LispError
import Parser (readExpr)

------ Entry Point (Main) -------

main :: IO ()
main = getArgs >>= putStrLn . evalInput . head

mainI :: IO ()
mainI = getLine >>= putStrLn . evalInput

mainI' :: String -> IO ()
mainI' = putStrLn . evalInput

evalInput :: String -> String
evalInput = extractValue . trapError . fmap show . (eval =<<) . readExpr
