
-- |
-- Module     : Simulation.Aivika.Vector
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- An imperative vector.
--
module Simulation.Aivika.Vector
       (Vector, 
        newVector, 
        copyVector,
        vectorCount, 
        appendVector, 
        readVector, 
        writeVector,
        vectorBinarySearch,
        freezeVector) where 

import Data.Array
import Data.Array.MArray
import Data.Array.IO
import Data.IORef
import Control.Monad

-- | Represents a resizable vector.
data Vector a = Vector { vectorArrayRef :: IORef (IOArray Int a),
                         vectorCountRef :: IORef Int, 
                         vectorCapacityRef :: IORef Int }

-- | Create a new vector.
newVector :: IO (Vector a)
newVector = 
  do array <- newArray_ (0, 4 - 1)
     arrayRef <- newIORef array
     countRef <- newIORef 0
     capacityRef <- newIORef 4
     return Vector { vectorArrayRef = arrayRef,
                     vectorCountRef = countRef,
                     vectorCapacityRef = capacityRef }

-- | Copy the vector.
copyVector :: Vector a -> IO (Vector a)
copyVector vector =
  do array <- readIORef (vectorArrayRef vector)
     count <- readIORef (vectorCountRef vector)
     array' <- newArray_ (0, count - 1)
     arrayRef' <- newIORef array'
     countRef' <- newIORef count
     capacityRef' <- newIORef count
     forM_ [0 .. count - 1] $ \i ->
       do x <- readArray array i
          writeArray array' i x
     return Vector { vectorArrayRef = arrayRef',
                     vectorCountRef = countRef',
                     vectorCapacityRef = capacityRef' }

-- | Ensure that the vector has the specified capacity.
vectorEnsureCapacity :: Vector a -> Int -> IO ()
vectorEnsureCapacity vector capacity =
  do capacity' <- readIORef (vectorCapacityRef vector)
     when (capacity' < capacity) $
       do array' <- readIORef (vectorArrayRef vector)
          count' <- readIORef (vectorCountRef vector)
          let capacity'' = max (2 * capacity') capacity
          array'' <- newArray_ (0, capacity'' - 1)
          forM_ [0 .. count' - 1] $ \i ->
            do x <- readArray array' i
               writeArray array'' i x
          writeIORef (vectorArrayRef vector) array''
          writeIORef (vectorCapacityRef vector) capacity''
          
-- | Return the element count.
vectorCount :: Vector a -> IO Int
vectorCount vector = readIORef (vectorCountRef vector)
          
-- | Add the specified element to the end of the vector.
appendVector :: Vector a -> a -> IO ()          
appendVector vector item =
  do count <- readIORef (vectorCountRef vector)
     vectorEnsureCapacity vector (count + 1)
     array <- readIORef (vectorArrayRef vector)
     writeArray array count item
     writeIORef (vectorCountRef vector) (count + 1)
     
-- | Read a value from the vector, where indices are started from 0.
readVector :: Vector a -> Int -> IO a
readVector vector index =
  do array <- readIORef (vectorArrayRef vector)
     readArray array index
          
-- | Set an array item at the specified index which is started from 0.
writeVector :: Vector a -> Int -> a -> IO ()
writeVector vector index item =
  do array <- readIORef (vectorArrayRef vector)
     writeArray array index item

vectorBinarySearch' :: Ord a => IOArray Int a -> a -> Int -> Int -> IO Int
vectorBinarySearch' array item left right =
  if left > right 
  then return $ - (right + 1) - 1
  else
    do let index = (left + right) `div` 2
       curr <- readArray array index
       if item < curr 
         then vectorBinarySearch' array item left (index - 1)
         else if item == curr
              then return index
              else vectorBinarySearch' array item (index + 1) right
                   
-- | Return the index of the specified element using binary search; otherwise, 
-- a negated insertion index minus one: 0 -> -0 - 1, ..., i -> -i - 1, ....
vectorBinarySearch :: Ord a => Vector a -> a -> IO Int
vectorBinarySearch vector item =
  do array <- readIORef (vectorArrayRef vector)
     count <- readIORef (vectorCountRef vector)
     vectorBinarySearch' array item 0 (count - 1)

freezeVector :: Vector a -> IO (Array Int a)
freezeVector vector = 
  do vector' <- copyVector vector
     array   <- readIORef (vectorArrayRef vector')
     freeze array
     