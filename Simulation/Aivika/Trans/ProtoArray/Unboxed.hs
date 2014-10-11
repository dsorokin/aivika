
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
       (ProtoArraying(..),
        ProtoArray) where

import Data.Array.IO.Safe

import Simulation.Aivika.Trans.Session

-- | A monad within which computation we can create and work with
-- the prototype of mutable unboxed arrays.
class ProtoArraying m e where
  
  -- | A prototype of mutable unboxed array.
  data ProtoArrayT m :: * -> * -> *

  -- | Return the bounds of the unboxed array.
  protoArrayBounds :: Ix i => ProtoArrayT m i e -> m (i, i)

  -- | Create a new ptototype of mutable unboxed array by the specified session and initial value.
  newProtoArray :: Ix i => SessionT m -> (i, i) -> e -> m (ProtoArrayT m i e)

  -- | Create a new ptototype of mutable unboxed array by the specified session
  -- with every element initialised to an undefined value.
  newProtoArray_ :: Ix i => SessionT m -> (i, i) -> m (ProtoArrayT m i e)

  -- | Read an element from the mutable unboxed array.
  readProtoArray :: Ix i => ProtoArrayT m i e -> i -> m e

  -- | Write the element in the mutable unboxed array.
  writeProtoArray :: Ix i => ProtoArrayT m i e -> i -> e -> m ()

instance MArray IOUArray e IO => ProtoArraying IO e where

  newtype ProtoArrayT IO i e = ProtoArray (IOUArray i e)

  {-# SPECIALISE INLINE protoArrayBounds :: (MArray IOUArray Bool IO, Ix i) => ProtoArray i Bool -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoArrayBounds :: (MArray IOUArray Char IO, Ix i) => ProtoArray i Char -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoArrayBounds :: (MArray IOUArray Double IO, Ix i) => ProtoArray i Double -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoArrayBounds :: (MArray IOUArray Float IO, Ix i) => ProtoArray i Float -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoArrayBounds :: (MArray IOUArray Int IO, Ix i) => ProtoArray i Int -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoArrayBounds :: (MArray IOUArray e IO, Ix i) => ProtoArray i e -> IO (i, i) #-}
  protoArrayBounds (ProtoArray a) = getBounds a

  {-# SPECIALISE INLINE newProtoArray :: (MArray IOUArray Bool IO, Ix i) => Session -> (i, i) -> Bool -> IO (ProtoArray i Bool) #-}
  {-# SPECIALISE INLINE newProtoArray :: (MArray IOUArray Char IO, Ix i) => Session -> (i, i) -> Char -> IO (ProtoArray i Char) #-}
  {-# SPECIALISE INLINE newProtoArray :: (MArray IOUArray Double IO, Ix i) => Session -> (i, i) -> Double -> IO (ProtoArray i Double) #-}
  {-# SPECIALISE INLINE newProtoArray :: (MArray IOUArray Float IO, Ix i) => Session -> (i, i) -> Float -> IO (ProtoArray i Float) #-}
  {-# SPECIALISE INLINE newProtoArray :: (MArray IOUArray Int IO, Ix i) => Session -> (i, i) -> Int -> IO (ProtoArray i Int) #-}
  {-# SPECIALISE INLINE newProtoArray :: (MArray IOUArray e IO, Ix i) => Session -> (i, i) -> e -> IO (ProtoArray i e) #-}
  newProtoArray s bnds e = fmap ProtoArray $ newArray bnds e

  {-# SPECIALISE INLINE newProtoArray_ :: (MArray IOUArray Bool IO, Ix i) => Session -> (i, i) -> IO (ProtoArray i Bool) #-}
  {-# SPECIALISE INLINE newProtoArray_ :: (MArray IOUArray Char IO, Ix i) => Session -> (i, i) -> IO (ProtoArray i Char) #-}
  {-# SPECIALISE INLINE newProtoArray_ :: (MArray IOUArray Double IO, Ix i) => Session -> (i, i) -> IO (ProtoArray i Double) #-}
  {-# SPECIALISE INLINE newProtoArray_ :: (MArray IOUArray Float IO, Ix i) => Session -> (i, i) -> IO (ProtoArray i Float) #-}
  {-# SPECIALISE INLINE newProtoArray_ :: (MArray IOUArray Int IO, Ix i) => Session -> (i, i) -> IO (ProtoArray i Int) #-}
  {-# SPECIALISE INLINE newProtoArray_ :: (MArray IOUArray e IO, Ix i) => Session -> (i, i) -> IO (ProtoArray i e) #-}
  newProtoArray_ s bnds = fmap ProtoArray $ newArray_ bnds

  {-# SPECIALISE INLINE readProtoArray :: (MArray IOUArray Bool IO, Ix i) => ProtoArray i Bool -> i -> IO Bool #-}
  {-# SPECIALISE INLINE readProtoArray :: (MArray IOUArray Char IO, Ix i) => ProtoArray i Char -> i -> IO Char #-}
  {-# SPECIALISE INLINE readProtoArray :: (MArray IOUArray Double IO, Ix i) => ProtoArray i Double -> i -> IO Double #-}
  {-# SPECIALISE INLINE readProtoArray :: (MArray IOUArray Float IO, Ix i) => ProtoArray i Float -> i -> IO Float #-}
  {-# SPECIALISE INLINE readProtoArray :: (MArray IOUArray Int IO, Ix i) => ProtoArray i Int -> i -> IO Int #-}
  {-# SPECIALISE INLINE readProtoArray :: (MArray IOUArray e IO, Ix i) => ProtoArray i e -> i -> IO e #-}
  readProtoArray (ProtoArray a) = readArray a

  {-# SPECIALISE INLINE writeProtoArray :: (MArray IOUArray Bool IO, Ix i) => ProtoArray i Bool -> i -> Bool -> IO () #-}
  {-# SPECIALISE INLINE writeProtoArray :: (MArray IOUArray Char IO, Ix i) => ProtoArray i Char -> i -> Char -> IO () #-}
  {-# SPECIALISE INLINE writeProtoArray :: (MArray IOUArray Double IO, Ix i) => ProtoArray i Double -> i -> Double -> IO () #-}
  {-# SPECIALISE INLINE writeProtoArray :: (MArray IOUArray Float IO, Ix i) => ProtoArray i Float -> i -> Float -> IO () #-}
  {-# SPECIALISE INLINE writeProtoArray :: (MArray IOUArray Int IO, Ix i) => ProtoArray i Int -> i -> Int -> IO () #-}
  {-# SPECIALISE INLINE writeProtoArray :: (MArray IOUArray e IO, Ix i) => ProtoArray i e -> i -> e -> IO () #-}
  writeProtoArray (ProtoArray a) = writeArray a

-- | A convenient type synonym.
type ProtoArray i e = ProtoArrayT IO i e
