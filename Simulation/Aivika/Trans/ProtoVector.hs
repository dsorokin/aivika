
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.ProtoVector
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a prototype of mutable vectors.
--
module Simulation.Aivika.Trans.ProtoVector
       (ProtoVectoring,
        ProtoVector,
        newProtoVector, 
        copyProtoVector,
        protoVectorCount, 
        appendProtoVector, 
        readProtoVector, 
        writeProtoVector,
        protoVectorBinarySearch,
        protoVectorInsert,
        protoVectorDeleteAt,
        protoVectorIndex,
        freezeProtoVector) where 

import Data.Array

import Control.Monad

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.ProtoArray

-- | A computation within which we can create and work with the prototype of mutable vectors.
class (ProtoReferring m, ProtoArraying m) => ProtoVectoring m

instance ProtoVectoring IO

-- | A prototype of mutable vector.
data ProtoVector m a =
    ProtoVector { vectorSession  :: Session m,
                  vectorArrayRef :: ProtoRef m (ProtoArray m Int a),
                  vectorCountRef :: ProtoRef m Int, 
                  vectorCapacityRef :: ProtoRef m Int }

-- | Create a new vector within the specified simulation session.
newProtoVector :: ProtoVectoring m => Session m -> m (ProtoVector m a)
{-# INLINABLE newProtoVector #-}
newProtoVector session = 
  do array <- newProtoArray_ session (0, 4 - 1)
     arrayRef <- newProtoRef session array
     countRef <- newProtoRef session 0
     capacityRef <- newProtoRef session 4
     return ProtoVector { vectorSession  = session,
                          vectorArrayRef = arrayRef,
                          vectorCountRef = countRef,
                          vectorCapacityRef = capacityRef }

-- | Copy the vector.
copyProtoVector :: ProtoVectoring m => ProtoVector m a -> m (ProtoVector m a)
{-# INLINABLE copyProtoVector #-}
copyProtoVector vector =
  do let session = vectorSession vector
     array <- readProtoRef (vectorArrayRef vector)
     count <- readProtoRef (vectorCountRef vector)
     array' <- newProtoArray_ session (0, count - 1)
     arrayRef' <- newProtoRef session array'
     countRef' <- newProtoRef session count
     capacityRef' <- newProtoRef session count
     forM_ [0 .. count - 1] $ \i ->
       do x <- readProtoArray array i
          writeProtoArray array' i x
     return ProtoVector { vectorSession  = session,
                          vectorArrayRef = arrayRef',
                          vectorCountRef = countRef',
                          vectorCapacityRef = capacityRef' }
       
-- | Ensure that the vector has the specified capacity.
protoVectorEnsureCapacity :: ProtoVectoring m => ProtoVector m a -> Int -> m ()
{-# INLINABLE protoVectorEnsureCapacity #-}
protoVectorEnsureCapacity vector capacity =
  do capacity' <- readProtoRef (vectorCapacityRef vector)
     when (capacity' < capacity) $
       do array' <- readProtoRef (vectorArrayRef vector)
          count' <- readProtoRef (vectorCountRef vector)
          let capacity'' = max (2 * capacity') capacity
              session    = vectorSession vector
          array'' <- newProtoArray_ session (0, capacity'' - 1)
          forM_ [0 .. count' - 1] $ \i ->
            do x <- readProtoArray array' i
               writeProtoArray array'' i x
          writeProtoRef (vectorArrayRef vector) array''
          writeProtoRef (vectorCapacityRef vector) capacity''

-- | Return the element count.
protoVectorCount :: ProtoVectoring m => ProtoVector m a -> m Int
{-# INLINE protoVectorCount #-}
protoVectorCount vector = readProtoRef (vectorCountRef vector)

-- | Add the specified element to the end of the vector.
appendProtoVector :: ProtoVectoring m => ProtoVector m a -> a -> m ()          
{-# INLINE appendProtoVector #-}
appendProtoVector vector item =
  do count <- readProtoRef (vectorCountRef vector)
     protoVectorEnsureCapacity vector (count + 1)
     array <- readProtoRef (vectorArrayRef vector)
     writeProtoArray array count item
     writeProtoRef (vectorCountRef vector) (count + 1)

-- | Read a value from the vector, where indices are started from 0.
readProtoVector :: ProtoVectoring m => ProtoVector m a -> Int -> m a
{-# INLINE readProtoVector #-}
readProtoVector vector index =
  do array <- readProtoRef (vectorArrayRef vector)
     readProtoArray array index

-- | Set an array item at the specified index which is started from 0.
writeProtoVector :: ProtoVectoring m => ProtoVector m a -> Int -> a -> m ()
{-# INLINE writeProtoVector #-}
writeProtoVector vector index item =
  do array <- readProtoRef (vectorArrayRef vector)
     writeProtoArray array index item

-- | Return the index of the specified element using binary search; otherwise, 
-- a negated insertion index minus one: 0 -> -0 - 1, ..., i -> -i - 1, ....
protoVectorBinarySearch :: (ProtoVectoring m, Ord a) => ProtoVector m a -> a -> m Int
{-# INLINE protoVectorBinarySearch #-}
protoVectorBinarySearch vector item =
  do array <- readProtoRef (vectorArrayRef vector)
     count <- readProtoRef (vectorCountRef vector)
     vectorBinarySearch' array item 0 (count - 1)

-- | Return the index of the specified element using binary search
-- within the specified range; otherwise, a negated insertion index minus one.
protoVectorBinarySearchWithin :: (ProtoVectoring m, Ord a) => ProtoVector m a -> a -> Int -> Int -> m Int
{-# INLINE protoVectorBinarySearchWithin #-}
protoVectorBinarySearchWithin vector item left right =
  do array <- readProtoRef (vectorArrayRef vector)
     vectorBinarySearch' array item left right

-- | Return the elements of the vector in an immutable array.
freezeProtoVector :: ProtoVectoring m => ProtoVector m a -> m (Array Int a)
{-# INLINE freezeProtoVector #-}
freezeProtoVector vector =
  do array <- readProtoRef (vectorArrayRef vector)
     freezeProtoArray array

-- | Insert the element in the vector at the specified index.
protoVectorInsert :: ProtoVectoring m => ProtoVector m a -> Int -> a -> m ()
{-# INLINABLE protoVectorInsert #-}
protoVectorInsert vector index item =
  do count <- readProtoRef (vectorCountRef vector)
     when (index < 0) $
       error $
       "Index cannot be " ++
       "negative: protoVectorInsert."
     when (index > count) $
       error $
       "Index cannot be greater " ++
       "than the count: protoVectorInsert."
     protoVectorEnsureCapacity vector (count + 1)
     array <- readProtoRef (vectorArrayRef vector)
     forM_ [count, count - 1 .. index + 1] $ \i ->
       do x <- readProtoArray array (i - 1)
          writeProtoArray array i x
     writeProtoArray array index item
     writeProtoRef (vectorCountRef vector) (count + 1)

-- | Delete the element at the specified index.
protoVectorDeleteAt :: ProtoVectoring m => ProtoVector m a -> Int -> m ()
{-# INLINABLE protoVectorDeleteAt #-}
protoVectorDeleteAt vector index =
  do count <- readProtoRef (vectorCountRef vector)
     when (index < 0) $
       error $
       "Index cannot be " ++
       "negative: protoVectorDeleteAt."
     when (index >= count) $
       error $
       "Index must be less " ++
       "than the count: protoVectorDeleteAt."
     array <- readProtoRef (vectorArrayRef vector)
     forM_ [index, index + 1 .. count - 2] $ \i ->
       do x <- readProtoArray array (i + 1)
          writeProtoArray array i x
     writeProtoArray array (count - 1) undefined
     writeProtoRef (vectorCountRef vector) (count - 1)

-- | Return the index of the item or -1.
protoVectorIndex :: (ProtoVectoring m, Eq a) => ProtoVector m a -> a -> m Int
{-# INLINABLE protoVectorIndex #-}
protoVectorIndex vector item =
  do count <- readProtoRef (vectorCountRef vector)
     array <- readProtoRef (vectorArrayRef vector)
     let loop index =
           if index >= count
           then return $ -1
           else do x <- readProtoArray array index
                   if item == x
                     then return index
                     else loop $ index + 1
     loop 0

vectorBinarySearch' :: (ProtoVectoring m, Ord a) => ProtoArray m Int a -> a -> Int -> Int -> m Int
{-# INLINABLE vectorBinarySearch' #-}
vectorBinarySearch' array item left right =
  if left > right 
  then return $ - (right + 1) - 1
  else
    do let index = (left + right) `div` 2
       curr <- readProtoArray array index
       if item < curr 
         then vectorBinarySearch' array item left (index - 1)
         else if item == curr
              then return index
              else vectorBinarySearch' array item (index + 1) right
