module Main where

import System.Environment
import Parser (readExpr)
import Evaluator (eval)

------ Entry Point (Main) -------

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

mainI :: IO ()
mainI = do
  input <- getLine
  print . eval . readExpr $ input

mainI' :: String -> IO ()
mainI' = print . eval . readExpr
