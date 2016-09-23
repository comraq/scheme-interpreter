module Main where

import System.Environment
import Control.Monad (join)

main :: IO ()
main =  do
  args <- getInput
  putStrLn ("Hello, " ++ function [args])

  where function = showArgs
        getInput = getLine

joinWith   :: (Foldable t, Monoid a) => a -> t a -> a
joinWith x =  foldr (\a as -> a `mappend` x `mappend` as) mempty

showArgs :: [String] -> String
showArgs =  joinWith " " . take 2

calcArgs   :: ([Int] -> Int) -> [String] -> String
calcArgs f =  show . f . map read
