module LispVector
  ( makeVector
  , vector
  , vectorSet
  , vectorLength
  , vectorRef
  , vectorToList
  , vectorFill
  ) where

import Data.Array.IO
import Data.Array.IArray

import Definition


-- size must be >= 0
makeVector :: Int -> a -> SVector a
makeVector size val =
  let (low, high) = getLVecBounds size
  in  array (low, high) $ zip [low..high] (repeat val)

vector :: [a] -> SVector a
vector vals =
  let bounds = getLVecBounds $ length vals
  in  listArray bounds vals

getLVecBounds :: Int -> (Int, Int)
getLVecBounds size = (0, size - 1)

vectorSet :: Int -> a -> SVector a -> SVector a
vectorSet i val vec = vec // [(i, val)]

vectorLength :: SVector a -> Int
vectorLength = (+1) . snd . bounds

vectorRef :: SVector a -> Int -> a
vectorRef = (!)

vectorToList :: SVector a -> [a]
vectorToList = elems

vectorFill :: b -> SVector a -> SVector b
vectorFill val = amap (const val)
