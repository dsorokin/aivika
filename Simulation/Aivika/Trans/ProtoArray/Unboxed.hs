
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.ProtoArray.Unboxed
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a prototype of all mutable unboxed arrays.
--
module Simulation.Aivika.Trans.ProtoArray.Unboxed
       (ProtoArraying(..)) where

import Data.Array
import Data.Array.IO.Safe

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef

-- | A monad within which computation we can create and work with
-- the prototype of mutable unboxed arrays.
class ProtoReferring m => ProtoArraying m a where
  
  -- | A prototype of mutable unboxed array.
  data ProtoArray m :: * -> *

  -- | Return the array size.
  protoArrayCount :: ProtoArray m a -> m Int

  -- | Create a new ptototype of mutable array by the specified session,
  -- size and initial value.
  newProtoArray :: Session m -> Int -> a -> m (ProtoArray m a)

  -- | Create a new ptototype of mutable array by the specified session
  -- and size with every element initialised to an undefined value.
  newProtoArray_ :: Session m -> Int -> m (ProtoArray m a)

  -- | Read an element from the mutable array.
  readProtoArray :: ProtoArray m a -> Int -> m a

  -- | Write the element in the mutable array.
  writeProtoArray :: ProtoArray m a -> Int -> a -> m ()

  -- | Return a list of the elements.
  protoArrayToList :: ProtoArray m a -> m [a]

  -- | Create an array by the specified list of elements.
  protoArrayFromList :: [a] -> m (ProtoArray m a)

  -- | Return the elements of the mutable array in an immutable array.
  freezeProtoArray :: ProtoArray m a -> m (Array Int a)

instance MArray IOUArray a IO => ProtoArraying IO a where

  newtype ProtoArray IO a = ProtoArray (IOUArray Int a)

  {-# SPECIALISE INLINE protoArrayCount :: MArray IOUArray Double IO => ProtoArray IO Double -> IO Int #-}
  {-# SPECIALISE INLINE protoArrayCount :: MArray IOUArray Float IO => ProtoArray IO Float -> IO Int #-}
  {-# SPECIALISE INLINE protoArrayCount :: MArray IOUArray Int IO => ProtoArray IO Int -> IO Int #-}
  protoArrayCount (ProtoArray a) = do { (0, n') <- getBounds a; return $ n' + 1 }

  {-# SPECIALISE INLINE newProtoArray :: MArray IOUArray Double IO => Session IO -> Int -> Double -> IO (ProtoArray IO Double) #-}
  {-# SPECIALISE INLINE newProtoArray :: MArray IOUArray Float IO => Session IO -> Int -> Float -> IO (ProtoArray IO Float) #-}
  {-# SPECIALISE INLINE newProtoArray :: MArray IOUArray Int IO => Session IO -> Int -> Int -> IO (ProtoArray IO Int) #-}
  newProtoArray s n a = fmap ProtoArray $ newArray (0, n - 1) a

  {-# SPECIALISE INLINE newProtoArray_ :: MArray IOUArray Double IO => Session IO -> Int -> IO (ProtoArray IO Double) #-}
  {-# SPECIALISE INLINE newProtoArray_ :: MArray IOUArray Float IO => Session IO -> Int -> IO (ProtoArray IO Float) #-}
  {-# SPECIALISE INLINE newProtoArray_ :: MArray IOUArray Int IO => Session IO -> Int -> IO (ProtoArray IO Int) #-}
  newProtoArray_ s n = fmap ProtoArray $ newArray_ (0, n - 1)

  {-# SPECIALISE INLINE readProtoArray :: MArray IOUArray Double IO => ProtoArray IO Double -> Int -> IO Double #-}
  {-# SPECIALISE INLINE readProtoArray :: MArray IOUArray Float IO => ProtoArray IO Float -> Int -> IO Float #-}
  {-# SPECIALISE INLINE readProtoArray :: MArray IOUArray Int IO => ProtoArray IO Int -> Int -> IO Int #-}
  readProtoArray (ProtoArray a) = readArray a

  {-# SPECIALISE INLINE writeProtoArray :: MArray IOUArray Double IO => ProtoArray IO Double -> Int -> Double -> IO () #-}
  {-# SPECIALISE INLINE writeProtoArray :: MArray IOUArray Float IO => ProtoArray IO Float -> Int -> Float -> IO () #-}
  {-# SPECIALISE INLINE writeProtoArray :: MArray IOUArray Int IO => ProtoArray IO Int -> Int -> Int -> IO () #-}
  writeProtoArray (ProtoArray a) = writeArray a

  {-# SPECIALISE INLINE protoArrayToList :: MArray IOUArray Double IO => ProtoArray IO Double -> IO [Double] #-}
  {-# SPECIALISE INLINE protoArrayToList :: MArray IOUArray Float IO => ProtoArray IO Float -> IO [Float] #-}
  {-# SPECIALISE INLINE protoArrayToList :: MArray IOUArray Int IO => ProtoArray IO Int -> IO [Int] #-}
  protoArrayToList (ProtoArray a) = getElems a

  {-# SPECIALISE INLINE protoArrayFromList :: MArray IOUArray Double IO => [Double] -> IO (ProtoArray IO Double) #-}
  {-# SPECIALISE INLINE protoArrayFromList :: MArray IOUArray Float IO => [Float] -> IO (ProtoArray IO Float) #-}
  {-# SPECIALISE INLINE protoArrayFromList :: MArray IOUArray Int IO => [Int] -> IO (ProtoArray IO Int) #-}
  protoArrayFromList xs = fmap ProtoArray $ newListArray (0, length xs - 1) xs

  {-# SPECIALISE INLINE freezeProtoArray :: MArray IOUArray Double IO => ProtoArray IO Double -> IO (Array Int Double) #-}
  {-# SPECIALISE INLINE freezeProtoArray :: MArray IOUArray Float IO => ProtoArray IO Float -> IO (Array Int Float) #-}
  {-# SPECIALISE INLINE freezeProtoArray :: MArray IOUArray Int IO => ProtoArray IO Int -> IO (Array Int Int) #-}
  freezeProtoArray (ProtoArray a) = freeze a
