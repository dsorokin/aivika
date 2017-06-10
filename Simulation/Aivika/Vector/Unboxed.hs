
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Vector.Unboxed
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- An imperative unboxed vector.
--
module Simulation.Aivika.Vector.Unboxed
       (Vector, 
        newVector, 
        copyVector, 
        vectorCount, 
        appendVector, 
        readVector, 
        writeVector, 
        vectorBinarySearch,
        vectorInsert,
        vectorDeleteAt,
        vectorDeleteRange,
        vectorDelete,
        vectorDeleteBy,
        vectorIndex,
        vectorIndexBy,
        vectorContains,
        vectorContainsBy,
        freezeVector) where 

import Data.Array
import Data.Array.MArray.Safe
import Data.Array.IO.Safe
import Data.IORef
import Control.Monad

import Simulation.Aivika.Unboxed

-- | Represents an unboxed resizable vector.
data Vector a = Vector { vectorArrayRef :: IORef (IOUArray Int a),
                         vectorCountRef :: IORef Int, 
                         vectorCapacityRef :: IORef Int }

-- | Create a new vector.
newVector :: Unboxed a => IO (Vector a)
newVector = 
  do array <- newUnboxedArray_ (0, 4 - 1)
     arrayRef <- newIORef array
     countRef <- newIORef 0
     capacityRef <- newIORef 4
     return Vector { vectorArrayRef = arrayRef,
                     vectorCountRef = countRef,
                     vectorCapacityRef = capacityRef }

-- | Copy the vector.
copyVector :: Unboxed a => Vector a -> IO (Vector a)
copyVector vector =
  do array <- readIORef (vectorArrayRef vector)
     count <- readIORef (vectorCountRef vector)
     array' <- newUnboxedArray_ (0, count - 1)
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
vectorEnsureCapacity :: Unboxed a => Vector a -> Int -> IO ()
vectorEnsureCapacity vector capacity =
  do capacity' <- readIORef (vectorCapacityRef vector)
     when (capacity' < capacity) $
       do array' <- readIORef (vectorArrayRef vector)
          count' <- readIORef (vectorCountRef vector)
          let capacity'' = max (2 * capacity') capacity
          array'' <- newUnboxedArray_ (0, capacity'' - 1)
          forM_ [0 .. count' - 1] $ \i ->
            do x <- readArray array' i
               writeArray array'' i x
          writeIORef (vectorArrayRef vector) array''
          writeIORef (vectorCapacityRef vector) capacity''
          
-- | Return the element count.
vectorCount :: Unboxed a => Vector a -> IO Int
vectorCount vector = readIORef (vectorCountRef vector)
          
-- | Add the specified element to the end of the vector.
appendVector :: Unboxed a => Vector a -> a -> IO ()          
appendVector vector item =
  do count <- readIORef (vectorCountRef vector)
     vectorEnsureCapacity vector (count + 1)
     array <- readIORef (vectorArrayRef vector)
     writeArray array count item
     writeIORef (vectorCountRef vector) (count + 1)
     
-- | Read a value from the vector, where indices are started from 0.
readVector :: Unboxed a => Vector a -> Int -> IO a
readVector vector index =
  do array <- readIORef (vectorArrayRef vector)
     readArray array index
          
-- | Set an array item at the specified index which is started from 0.
writeVector :: Unboxed a => Vector a -> Int -> a -> IO ()
writeVector vector index item =
  do array <- readIORef (vectorArrayRef vector)
     writeArray array index item
          
vectorBinarySearch' :: (Unboxed a, Ord a) => IOUArray Int a -> a -> Int -> Int -> IO Int
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
vectorBinarySearch :: (Unboxed a, Ord a) => Vector a -> a -> IO Int
vectorBinarySearch vector item =
  do array <- readIORef (vectorArrayRef vector)
     count <- readIORef (vectorCountRef vector)
     vectorBinarySearch' array item 0 (count - 1)

-- | Return the elements of the vector in an immutable array.
freezeVector :: Unboxed a => Vector a -> IO (Array Int a)
freezeVector vector = 
  do vector' <- copyVector vector
     array   <- readIORef (vectorArrayRef vector')
     freeze array
     
-- | Insert the element in the vector at the specified index.
vectorInsert :: Unboxed a => Vector a -> Int -> a -> IO ()          
vectorInsert vector index item =
  do count <- readIORef (vectorCountRef vector)
     when (index < 0) $
       error $
       "Index cannot be " ++
       "negative: vectorInsert."
     when (index > count) $
       error $
       "Index cannot be greater " ++
       "than the count: vectorInsert."
     vectorEnsureCapacity vector (count + 1)
     array <- readIORef (vectorArrayRef vector)
     forM_ [count, count - 1 .. index + 1] $ \i ->
       do x <- readArray array (i - 1)
          writeArray array i x
     writeArray array index item
     writeIORef (vectorCountRef vector) (count + 1)
     
-- | Delete the element at the specified index.
vectorDeleteAt :: Unboxed a => Vector a -> Int -> IO ()
vectorDeleteAt vector index =
  do count <- readIORef (vectorCountRef vector)
     when (index < 0) $
       error $
       "Index cannot be " ++
       "negative: vectorDeleteAt."
     when (index >= count) $
       error $
       "Index must be less " ++
       "than the count: vectorDeleteAt."
     array <- readIORef (vectorArrayRef vector)
     forM_ [index, index + 1 .. count - 2] $ \i ->
       do x <- readArray array (i + 1)
          writeArray array i x
     writeArray array (count - 1) undefined
     writeIORef (vectorCountRef vector) (count - 1)

-- | Delete the specified range of elements.
vectorDeleteRange :: Unboxed a
                     => Vector a
                     -- ^ the vector
                     -> Int
                     -- ^ the start index
                     -> Int
                     -- ^ the count of items to be removed
                     -> IO ()
vectorDeleteRange vector index len =
  do count <- readIORef (vectorCountRef vector)
     when (index < 0) $
       error $
       "The first index cannot be " ++
       "negative: vectorDeleteRange."
     when (index + len - 1 >= count) $
       error $
       "The last index must be less " ++
       "than the count: vectorDeleteRange."
     when (len < 0) $
       error "Negative range length: vectorDeleteRange." 
     array <- readIORef (vectorArrayRef vector)
     forM_ [index, index + 1 .. (count - len) - 1] $ \i ->
       do x <- readArray array (i + len)
          writeArray array i x
     forM_ [(count - len) .. count - 1] $ \i ->
       writeArray array i undefined
     writeIORef (vectorCountRef vector) (count - len)
     
-- | Return the index of the item or -1.     
vectorIndex :: (Unboxed a, Eq a) => Vector a -> a -> IO Int
vectorIndex vector item =
  do count <- readIORef (vectorCountRef vector)
     array <- readIORef (vectorArrayRef vector)
     let loop index =
           if index >= count
           then return $ -1
           else do x <- readArray array index
                   if item == x
                     then return index
                     else loop $ index + 1
     loop 0
     
-- | Return an index of the item satisfying the predicate or -1.     
vectorIndexBy :: Unboxed a => Vector a -> (a -> Bool) -> IO Int
vectorIndexBy vector pred =
  do count <- readIORef (vectorCountRef vector)
     array <- readIORef (vectorArrayRef vector)
     let loop index =
           if index >= count
           then return $ -1
           else do x <- readArray array index
                   if pred x
                     then return index
                     else loop $ index + 1
     loop 0

-- | Remove the specified element and return a flag indicating
-- whether the element was found and removed.
vectorDelete :: (Unboxed a, Eq a) => Vector a -> a -> IO Bool
vectorDelete vector item =
  do index <- vectorIndex vector item
     if index >= 0
       then do vectorDeleteAt vector index
               return True
       else return False
            
-- | Remove an element by the specified predicate and return a flag indicating
-- whether the element was found and removed.
vectorDeleteBy :: Unboxed a => Vector a -> (a -> Bool) -> IO Bool
vectorDeleteBy vector pred =
  do index <- vectorIndexBy vector pred
     if index >= 0
       then do vectorDeleteAt vector index
               return True
       else return False

-- | Detect whether the specified element is contained in the vector.
vectorContains :: (Unboxed a, Eq a) => Vector a -> a -> IO Bool
vectorContains vector item =
  do index <- vectorIndex vector item
     return (index >= 0)
            
-- | Detect whether an element satisfying the specified predicate is contained in the vector.
vectorContainsBy :: Unboxed a => Vector a -> (a -> Bool) -> IO (Maybe a)
vectorContainsBy vector pred =
  do index <- vectorIndexBy vector pred
     if index >= 0
       then do a <- readVector vector index
               return (Just a)
       else return Nothing
