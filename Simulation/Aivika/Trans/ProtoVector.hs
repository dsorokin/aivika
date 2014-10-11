
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.ProtoVector
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a prototype of all mutable vectors.
--
module Simulation.Aivika.Trans.ProtoVector
       (ProtoVectoring(..),
        ProtoVector) where

import Data.IORef
import Data.Array
import Data.Array.IO.Safe

import Control.Monad

import Simulation.Aivika.Trans.Session

-- | A monad within which computation we can create and work with
-- the prototype of mutable vectors.
class ProtoVectoring m where
  
  -- | A prototype of mutable vector.
  data ProtoVectorT m :: * -> *

  -- | Create a new vector within the specified simulation session.
  newProtoVector :: SessionT m -> m (ProtoVectorT m a)

  -- | Copy the vector.
  copyProtoVector :: ProtoVectorT m a -> m (ProtoVectorT m a)

  -- | Ensure that the vector has the specified capacity.
  protoVectorEnsureCapacity :: ProtoVectorT m a -> Int -> m ()

  -- | Return the element count.
  protoVectorCount :: ProtoVectorT m a -> m Int

  -- | Add the specified element to the end of the vector.
  appendProtoVector :: ProtoVectorT m a -> a -> m ()          

  -- | Read a value from the vector, where indices are started from 0.
  readProtoVector :: ProtoVectorT m a -> Int -> m a

  -- | Set an array item at the specified index which is started from 0.
  writeProtoVector :: ProtoVectorT m a -> Int -> a -> m ()

  -- | Return the index of the specified element using binary search; otherwise, 
  -- a negated insertion index minus one: 0 -> -0 - 1, ..., i -> -i - 1, ....
  protoVectorBinarySearch :: Ord a => ProtoVectorT m a -> a -> m Int

  -- | Return the index of the specified element using binary search
  -- within the specified range; otherwise, a negated insertion index minus one.
  protoVectorBinarySearchWithin :: Ord a => ProtoVectorT m a -> a -> Int -> Int -> m Int

  -- | Return the elements of the vector in an immutable array.
  freezeProtoVector :: ProtoVectorT m a -> m (Array Int a)

  -- | Insert the element in the vector at the specified index.
  protoVectorInsert :: ProtoVectorT m a -> Int -> a -> m ()          

  -- | Delete the element at the specified index.
  protoVectorDeleteAt :: ProtoVectorT m a -> Int -> m ()

  -- | Return the index of the item or -1.
  protoVectorIndex :: Eq a => ProtoVectorT m a -> a -> m Int

instance ProtoVectoring IO where

  data ProtoVectorT IO a =
    ProtoVector { vectorArrayRef :: IORef (IOArray Int a),
                  vectorCountRef :: IORef Int, 
                  vectorCapacityRef :: IORef Int }

  {-# SPECIALISE INLINE newProtoVector :: Session -> IO (ProtoVector a) #-}
  newProtoVector session = 
    do array <- newArray_ (0, 4 - 1)
       arrayRef <- newIORef array
       countRef <- newIORef 0
       capacityRef <- newIORef 4
       return ProtoVector { vectorArrayRef = arrayRef,
                            vectorCountRef = countRef,
                            vectorCapacityRef = capacityRef }

  {-# SPECIALISE INLINE copyProtoVector :: ProtoVector a -> IO (ProtoVector a) #-}
  copyProtoVector vector =
    do array <- readIORef (vectorArrayRef vector)
       count <- readIORef (vectorCountRef vector)
       array' <- newArray_ (0, count - 1)
       arrayRef' <- newIORef array'
       countRef' <- newIORef count
       capacityRef' <- newIORef count
       forM_ [0 .. count - 1] $ \i ->
         do x <- readArray array i
            writeArray array' i x
       return ProtoVector { vectorArrayRef = arrayRef',
                            vectorCountRef = countRef',
                            vectorCapacityRef = capacityRef' }

  {-# SPECIALISE INLINE protoVectorEnsureCapacity :: ProtoVector a -> Int -> IO () #-}
  protoVectorEnsureCapacity vector capacity =
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
          
  {-# SPECIALISE INLINE protoVectorCount :: ProtoVector a -> IO Int #-}
  protoVectorCount vector = readIORef (vectorCountRef vector)
          
  {-# SPECIALISE INLINE appendProtoVector :: ProtoVector a -> a -> IO () #-}
  appendProtoVector vector item =
    do count <- readIORef (vectorCountRef vector)
       protoVectorEnsureCapacity vector (count + 1)
       array <- readIORef (vectorArrayRef vector)
       writeArray array count item
       writeIORef (vectorCountRef vector) (count + 1)
     
  {-# SPECIALISE INLINE readProtoVector :: ProtoVector a -> Int -> IO a #-}
  readProtoVector vector index =
    do array <- readIORef (vectorArrayRef vector)
       readArray array index
          
  {-# SPECIALISE INLINE writeProtoVector :: ProtoVector a -> Int -> a -> IO () #-}
  writeProtoVector vector index item =
    do array <- readIORef (vectorArrayRef vector)
       writeArray array index item
                   
  {-# SPECIALISE INLINE protoVectorBinarySearch :: Ord a => ProtoVector a -> a -> IO Int #-}
  protoVectorBinarySearch vector item =
    do array <- readIORef (vectorArrayRef vector)
       count <- readIORef (vectorCountRef vector)
       vectorBinarySearch' array item 0 (count - 1)
                   
  {-# SPECIALISE INLINE protoVectorBinarySearchWithin :: Ord a => ProtoVector a -> a -> Int -> Int -> IO Int #-}
  protoVectorBinarySearchWithin vector item left right =
    do array <- readIORef (vectorArrayRef vector)
       vectorBinarySearch' array item left right

  {-# SPECIALISE INLINE freezeProtoVector :: ProtoVector a -> IO (Array Int a) #-}
  freezeProtoVector vector = 
    do array <- readIORef (vectorArrayRef vector)
       freeze array
     
  {-# SPECIALISE INLINE protoVectorInsert :: ProtoVector a -> Int -> a -> IO () #-}
  protoVectorInsert vector index item =
    do count <- readIORef (vectorCountRef vector)
       when (index < 0) $
         error $
         "Index cannot be " ++
         "negative: vectorInsert."
       when (index > count) $
         error $
         "Index cannot be greater " ++
         "than the count: vectorInsert."
       protoVectorEnsureCapacity vector (count + 1)
       array <- readIORef (vectorArrayRef vector)
       forM_ [count, count - 1 .. index + 1] $ \i ->
         do x <- readArray array (i - 1)
            writeArray array i x
       writeArray array index item
       writeIORef (vectorCountRef vector) (count + 1)
     
  {-# SPECIALISE INLINE protoVectorDeleteAt :: ProtoVector a -> Int -> IO () #-}
  protoVectorDeleteAt vector index =
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
     
  {-# SPECIALISE INLINE protoVectorIndex :: Ord a => ProtoVector a -> a -> IO Int #-}
  protoVectorIndex vector item =
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

-- | A convenient type synonym.
type ProtoVector a  = ProtoVectorT IO a
