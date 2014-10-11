
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.ProtoVector.Unboxed
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a prototype of all mutable unboxed vectors.
--
module Simulation.Aivika.Trans.ProtoVector.Unboxed
       (ProtoVectoring(..),
        ProtoVector) where

import Data.IORef
import Data.Array
import Data.Array.IO.Safe

import Control.Monad

import Simulation.Aivika.Trans.Session

-- | A monad within which computation we can create and work with
-- the prototype of mutable unboxed vectors.
class ProtoVectoring m a where
  
  -- | A prototype of mutable unboxed vector.
  data ProtoVectorT m a :: *

  -- | Create a new unboxed vector within the specified simulation session.
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

instance MArray IOUArray a IO => ProtoVectoring IO a where

  data ProtoVectorT IO a =
    ProtoVector { vectorArrayRef :: IORef (IOUArray Int a),
                  vectorCountRef :: IORef Int, 
                  vectorCapacityRef :: IORef Int }

  {-# SPECIALISE INLINE newProtoVector :: MArray IOUArray Double IO => Session -> IO (ProtoVector Double) #-}
  {-# SPECIALISE INLINE newProtoVector :: MArray IOUArray Float IO => Session -> IO (ProtoVector Float) #-}
  {-# SPECIALISE INLINE newProtoVector :: MArray IOUArray Int IO => Session -> IO (ProtoVector Int) #-}
  {-# SPECIALISE INLINE newProtoVector :: MArray IOUArray a IO => Session -> IO (ProtoVector a) #-}
  newProtoVector session = 
    do array <- newArray_ (0, 4 - 1)
       arrayRef <- newIORef array
       countRef <- newIORef 0
       capacityRef <- newIORef 4
       return ProtoVector { vectorArrayRef = arrayRef,
                            vectorCountRef = countRef,
                            vectorCapacityRef = capacityRef }

  {-# SPECIALISE INLINE copyProtoVector :: MArray IOUArray Double IO => ProtoVector Double -> IO (ProtoVector Double) #-}
  {-# SPECIALISE INLINE copyProtoVector :: MArray IOUArray Float IO => ProtoVector Float -> IO (ProtoVector Float) #-}
  {-# SPECIALISE INLINE copyProtoVector :: MArray IOUArray Int IO => ProtoVector Int -> IO (ProtoVector Int) #-}
  {-# SPECIALISE INLINE copyProtoVector :: MArray IOUArray a IO => ProtoVector a -> IO (ProtoVector a) #-}
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

  {-# SPECIALISE INLINE protoVectorEnsureCapacity :: MArray IOUArray Double IO => ProtoVector Double -> Int -> IO () #-}
  {-# SPECIALISE INLINE protoVectorEnsureCapacity :: MArray IOUArray Float IO => ProtoVector Float -> Int -> IO () #-}
  {-# SPECIALISE INLINE protoVectorEnsureCapacity :: MArray IOUArray Int IO => ProtoVector Int -> Int -> IO () #-}
  {-# SPECIALISE INLINE protoVectorEnsureCapacity :: MArray IOUArray a IO => ProtoVector a -> Int -> IO () #-}
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
          
  {-# SPECIALISE INLINE protoVectorCount :: MArray IOUArray Double IO => ProtoVector Double -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorCount :: MArray IOUArray Float IO => ProtoVector Float -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorCount :: MArray IOUArray Int IO => ProtoVector Int -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorCount :: MArray IOUArray a IO => ProtoVector a -> IO Int #-}
  protoVectorCount vector = readIORef (vectorCountRef vector)
          
  {-# SPECIALISE INLINE appendProtoVector :: MArray IOUArray Double IO => ProtoVector Double -> Double -> IO () #-}
  {-# SPECIALISE INLINE appendProtoVector :: MArray IOUArray Float IO => ProtoVector Float -> Float -> IO () #-}
  {-# SPECIALISE INLINE appendProtoVector :: MArray IOUArray Int IO => ProtoVector Int -> Int -> IO () #-}
  {-# SPECIALISE INLINE appendProtoVector :: MArray IOUArray a IO => ProtoVector a -> a -> IO () #-}
  appendProtoVector vector item =
    do count <- readIORef (vectorCountRef vector)
       protoVectorEnsureCapacity vector (count + 1)
       array <- readIORef (vectorArrayRef vector)
       writeArray array count item
       writeIORef (vectorCountRef vector) (count + 1)
     
  {-# SPECIALISE INLINE readProtoVector :: MArray IOUArray Double IO => ProtoVector Double -> Int -> IO Double #-}
  {-# SPECIALISE INLINE readProtoVector :: MArray IOUArray Float IO => ProtoVector Float -> Int -> IO Float #-}
  {-# SPECIALISE INLINE readProtoVector :: MArray IOUArray Int IO => ProtoVector Int -> Int -> IO Int #-}
  {-# SPECIALISE INLINE readProtoVector :: MArray IOUArray a IO => ProtoVector a -> Int -> IO a #-}
  readProtoVector vector index =
    do array <- readIORef (vectorArrayRef vector)
       readArray array index
          
  {-# SPECIALISE INLINE writeProtoVector :: MArray IOUArray Double IO => ProtoVector Double -> Int -> Double -> IO () #-}
  {-# SPECIALISE INLINE writeProtoVector :: MArray IOUArray Float IO => ProtoVector Float -> Int -> Float -> IO () #-}
  {-# SPECIALISE INLINE writeProtoVector :: MArray IOUArray Int IO => ProtoVector Int -> Int -> Int -> IO () #-}
  {-# SPECIALISE INLINE writeProtoVector :: MArray IOUArray a IO => ProtoVector a -> Int -> a -> IO () #-}
  writeProtoVector vector index item =
    do array <- readIORef (vectorArrayRef vector)
       writeArray array index item
                   
  {-# SPECIALISE INLINE protoVectorBinarySearch :: MArray IOUArray Double IO => ProtoVector Double -> Double -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorBinarySearch :: MArray IOUArray Float IO => ProtoVector Float -> Float -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorBinarySearch :: MArray IOUArray Int IO => ProtoVector Int -> Int -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorBinarySearch :: (MArray IOUArray a IO, Ord a) => ProtoVector a -> a -> IO Int #-}
  protoVectorBinarySearch vector item =
    do array <- readIORef (vectorArrayRef vector)
       count <- readIORef (vectorCountRef vector)
       vectorBinarySearch' array item 0 (count - 1)
                   
  {-# SPECIALISE INLINE protoVectorBinarySearchWithin :: MArray IOUArray Double IO => ProtoVector Double -> Double -> Int -> Int -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorBinarySearchWithin :: MArray IOUArray Float IO => ProtoVector Float -> Float -> Int -> Int -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorBinarySearchWithin :: MArray IOUArray Int IO => ProtoVector Int -> Int -> Int -> Int -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorBinarySearchWithin :: (MArray IOUArray a IO, Ord a) => ProtoVector a -> a -> Int -> Int -> IO Int #-}
  protoVectorBinarySearchWithin vector item left right =
    do array <- readIORef (vectorArrayRef vector)
       vectorBinarySearch' array item left right

  {-# SPECIALISE INLINE freezeProtoVector :: MArray IOUArray Double IO => ProtoVector Double -> IO (Array Int Double) #-}
  {-# SPECIALISE INLINE freezeProtoVector :: MArray IOUArray Float IO => ProtoVector Float -> IO (Array Int Float) #-}
  {-# SPECIALISE INLINE freezeProtoVector :: MArray IOUArray Int IO => ProtoVector Int -> IO (Array Int Int) #-}
  {-# SPECIALISE INLINE freezeProtoVector :: MArray IOUArray a IO => ProtoVector a -> IO (Array Int a) #-}
  freezeProtoVector vector = 
    do array <- readIORef (vectorArrayRef vector)
       freeze array
     
  {-# SPECIALISE INLINE protoVectorInsert :: MArray IOUArray Double IO => ProtoVector Double -> Int -> Double -> IO () #-}
  {-# SPECIALISE INLINE protoVectorInsert :: MArray IOUArray Float IO => ProtoVector Float -> Int -> Float -> IO () #-}
  {-# SPECIALISE INLINE protoVectorInsert :: MArray IOUArray Int IO => ProtoVector Int -> Int -> Int -> IO () #-}
  {-# SPECIALISE INLINE protoVectorInsert :: MArray IOUArray a IO => ProtoVector a -> Int -> a -> IO () #-}
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
     
  {-# SPECIALISE INLINE protoVectorDeleteAt :: MArray IOUArray Double IO => ProtoVector Double -> Int -> IO () #-}
  {-# SPECIALISE INLINE protoVectorDeleteAt :: MArray IOUArray Float IO => ProtoVector Float -> Int -> IO () #-}
  {-# SPECIALISE INLINE protoVectorDeleteAt :: MArray IOUArray Int IO => ProtoVector Int -> Int -> IO () #-}
  {-# SPECIALISE INLINE protoVectorDeleteAt :: MArray IOUArray a IO => ProtoVector a -> Int -> IO () #-}
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
     
  {-# SPECIALISE INLINE protoVectorIndex :: MArray IOUArray Double IO => ProtoVector Double -> Double -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorIndex :: MArray IOUArray Float IO => ProtoVector Float -> Float -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorIndex :: MArray IOUArray Int IO => ProtoVector Int -> Int -> IO Int #-}
  {-# SPECIALISE INLINE protoVectorIndex :: (MArray IOUArray a IO, Ord a) => ProtoVector a -> a -> IO Int #-}
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

vectorBinarySearch' :: (MArray IOUArray a IO, Ord a) => IOUArray Int a -> a -> Int -> Int -> IO Int
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
