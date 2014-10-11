
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

-- | A monad within which computation we can create and work with
-- the prototype of mutable unboxed arrays.
class Monad m => ProtoArraying m e where
  
  -- | A prototype of mutable unboxed array.
  data ProtoArray m :: * -> * -> *

  -- | Return the bounds of the unboxed array.
  protoArrayBounds :: Ix i => ProtoArray m i e -> m (i, i)

  -- | Create a new ptototype of mutable unboxed array by the specified session and initial value.
  newProtoArray :: Ix i => Session m -> (i, i) -> e -> m (ProtoArray m i e)

  -- | Create a new ptototype of mutable unboxed array by the specified session
  -- with every element initialised to an undefined value.
  newProtoArray_ :: Ix i => Session m -> (i, i) -> m (ProtoArray m i e)

  -- | Read an element from the mutable unboxed array.
  readProtoArray :: Ix i => ProtoArray m i e -> i -> m e

  -- | Write the element in the mutable unboxed array.
  writeProtoArray :: Ix i => ProtoArray m i e -> i -> e -> m ()

  -- | Return a list of the elements.
  protoArrayElems :: Ix i => ProtoArray m i e -> m [e]

  -- | Return a list of the association pairs.
  protoArrayAssocs :: Ix i => ProtoArray m i e -> m [(i, e)]

  -- | Return the elements of the mutable unboxed array in an immutable array.
  freezeProtoArray :: Ix i => ProtoArray m i e -> m (Array i e)

instance MArray IOUArray e IO => ProtoArraying IO e where

  newtype ProtoArray IO i e = ProtoArray (IOUArray i e)

  {-# SPECIALISE INLINE protoArrayBounds :: (MArray IOUArray Double IO, Ix i) => ProtoArray IO i Double -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoArrayBounds :: (MArray IOUArray Float IO, Ix i) => ProtoArray IO i Float -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoArrayBounds :: (MArray IOUArray Int IO, Ix i) => ProtoArray IO i Int -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoArrayBounds :: (MArray IOUArray e IO, Ix i) => ProtoArray IO i e -> IO (i, i) #-}
  protoArrayBounds (ProtoArray a) = getBounds a

  {-# SPECIALISE INLINE newProtoArray :: (MArray IOUArray Double IO, Ix i) => Session IO -> (i, i) -> Double -> IO (ProtoArray IO i Double) #-}
  {-# SPECIALISE INLINE newProtoArray :: (MArray IOUArray Float IO, Ix i) => Session IO -> (i, i) -> Float -> IO (ProtoArray IO i Float) #-}
  {-# SPECIALISE INLINE newProtoArray :: (MArray IOUArray Int IO, Ix i) => Session IO -> (i, i) -> Int -> IO (ProtoArray IO i Int) #-}
  {-# SPECIALISE INLINE newProtoArray :: (MArray IOUArray e IO, Ix i) => Session IO -> (i, i) -> e -> IO (ProtoArray IO i e) #-}
  newProtoArray s bnds e = fmap ProtoArray $ newArray bnds e

  {-# SPECIALISE INLINE newProtoArray_ :: (MArray IOUArray Double IO, Ix i) => Session IO -> (i, i) -> IO (ProtoArray IO i Double) #-}
  {-# SPECIALISE INLINE newProtoArray_ :: (MArray IOUArray Float IO, Ix i) => Session IO -> (i, i) -> IO (ProtoArray IO i Float) #-}
  {-# SPECIALISE INLINE newProtoArray_ :: (MArray IOUArray Int IO, Ix i) => Session IO -> (i, i) -> IO (ProtoArray IO i Int) #-}
  {-# SPECIALISE INLINE newProtoArray_ :: (MArray IOUArray e IO, Ix i) => Session IO -> (i, i) -> IO (ProtoArray IO i e) #-}
  newProtoArray_ s bnds = fmap ProtoArray $ newArray_ bnds

  {-# SPECIALISE INLINE readProtoArray :: (MArray IOUArray Double IO, Ix i) => ProtoArray IO i Double -> i -> IO Double #-}
  {-# SPECIALISE INLINE readProtoArray :: (MArray IOUArray Float IO, Ix i) => ProtoArray IO i Float -> i -> IO Float #-}
  {-# SPECIALISE INLINE readProtoArray :: (MArray IOUArray Int IO, Ix i) => ProtoArray IO i Int -> i -> IO Int #-}
  {-# SPECIALISE INLINE readProtoArray :: (MArray IOUArray e IO, Ix i) => ProtoArray IO i e -> i -> IO e #-}
  readProtoArray (ProtoArray a) = readArray a

  {-# SPECIALISE INLINE writeProtoArray :: (MArray IOUArray Double IO, Ix i) => ProtoArray IO i Double -> i -> Double -> IO () #-}
  {-# SPECIALISE INLINE writeProtoArray :: (MArray IOUArray Float IO, Ix i) => ProtoArray IO i Float -> i -> Float -> IO () #-}
  {-# SPECIALISE INLINE writeProtoArray :: (MArray IOUArray Int IO, Ix i) => ProtoArray IO i Int -> i -> Int -> IO () #-}
  {-# SPECIALISE INLINE writeProtoArray :: (MArray IOUArray e IO, Ix i) => ProtoArray IO i e -> i -> e -> IO () #-}
  writeProtoArray (ProtoArray a) = writeArray a

  {-# SPECIALISE INLINE protoArrayElems :: (MArray IOUArray Double IO, Ix i) => ProtoArray IO i Double -> IO [Double] #-}
  {-# SPECIALISE INLINE protoArrayElems :: (MArray IOUArray Float IO, Ix i) => ProtoArray IO i Float -> IO [Float] #-}
  {-# SPECIALISE INLINE protoArrayElems :: (MArray IOUArray Int IO, Ix i) => ProtoArray IO i Int -> IO [Int] #-}
  {-# SPECIALISE INLINE protoArrayElems :: (MArray IOUArray e IO, Ix i) => ProtoArray IO i e -> IO [e] #-}
  protoArrayElems (ProtoArray a) = getElems a

  {-# SPECIALISE INLINE protoArrayAssocs :: (MArray IOUArray Double IO, Ix i) => ProtoArray IO i Double -> IO [(i, Double)] #-}
  {-# SPECIALISE INLINE protoArrayAssocs :: (MArray IOUArray Float IO, Ix i) => ProtoArray IO i Float -> IO [(i, Float)] #-}
  {-# SPECIALISE INLINE protoArrayAssocs :: (MArray IOUArray Int IO, Ix i) => ProtoArray IO i Int -> IO [(i, Int)] #-}
  {-# SPECIALISE INLINE protoArrayAssocs :: (MArray IOUArray e IO, Ix i) => ProtoArray IO i e -> IO [(i, e)] #-}
  protoArrayAssocs (ProtoArray a) = getAssocs a

  {-# SPECIALISE INLINE freezeProtoArray :: (MArray IOUArray Double IO, Ix i) => ProtoArray IO i Double -> IO (Array i Double) #-}
  {-# SPECIALISE INLINE freezeProtoArray :: (MArray IOUArray Float IO, Ix i) => ProtoArray IO i Float -> IO (Array i Float) #-}
  {-# SPECIALISE INLINE freezeProtoArray :: (MArray IOUArray Int IO, Ix i) => ProtoArray IO i Int -> IO (Array i Int) #-}
  {-# SPECIALISE INLINE freezeProtoArray :: (MArray IOUArray e IO, Ix i) => ProtoArray IO i e -> IO (Array i e) #-}
  freezeProtoArray (ProtoArray a) = freeze a
