module LispVector
  ( makeVector
  , vector
  , vectorSet
  , vectorLength
  , vectorRef
  , vectorToList
  , vectorFill
  ) where

import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST

import Definition


type MutSVector s = STArray s Int

-- size must be >= 0
makeVector :: Int -> a -> ST s (MutSVector s a)
makeVector size val =
  let bounds = getLVecBounds size
  in  newArray bounds val

vector :: [a] -> ST s (MutSVector s a)
vector vals =
  let bounds = getLVecBounds $ length vals
  in  newListArray bounds vals

getLVecBounds :: Int -> (Int, Int)
getLVecBounds size = (0, size - 1)

vectorSet :: Int -> a -> MutSVector s a -> ST s (MutSVector s a)
vectorSet i val vec = writeArray vec i val >> return vec

vectorLength :: SVector a -> Int
vectorLength = (+1) . snd . bounds

vectorRef :: SVector a -> Int -> a
vectorRef = (!)

vectorToList :: SVector a -> [a]
vectorToList = elems

vectorFill :: a -> MutSVector s a -> ST s (MutSVector s a)
vectorFill val = mapArray (const val)
