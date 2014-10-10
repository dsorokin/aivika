
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.ProtoArray
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a prototype of all mutable arrays.
--
module Simulation.Aivika.Trans.ProtoArray
       (ProtoArraying(..),
        ProtoArray,
        ProtoUArraying(..),
        ProtoUArray) where

import Data.Array.IO.Safe

import Simulation.Aivika.Trans.Session

-- | A monad within which computation we can create and work with
-- the prototype of mutable arrays.
class ProtoArraying m where
  
  -- | A prototype of mutable array.
  data ProtoArrayT m :: * -> * -> *

  -- | Return the bounds of the array.
  protoArrayBounds :: Ix i => ProtoArrayT m i e -> m (i, i)

  -- | Create a new ptototype of mutable array by the specified session and initial value.
  newProtoArray :: Ix i => SessionT m -> (i, i) -> e -> m (ProtoArrayT m i e)

  -- | Create a new ptototype of mutable array by the specified session
  -- with every element initialised to an undefined value.
  newProtoArray_ :: Ix i => SessionT m -> (i, i) -> m (ProtoArrayT m i e)

  -- | Read an element from the mutable array.
  readProtoArray :: Ix i => ProtoArrayT m i e -> i -> m e

  -- | Write the element in the mutable array.
  writeProtoArray :: Ix i => ProtoArrayT m i e -> i -> e -> m ()

-- | A monad within which computation we can create and work with
-- the prototype of mutable unboxed arrays.
class ProtoArraying m => ProtoUArraying m e where
  
  -- | A prototype of mutable unboxed array.
  data ProtoUArrayT m :: * -> * -> *

  -- | Return the bounds of the unboxed array.
  protoUArrayBounds :: Ix i => ProtoUArrayT m i e -> m (i, i)

  -- | Create a new ptototype of mutable unboxed array by the specified session and initial value.
  newProtoUArray :: Ix i => SessionT m -> (i, i) -> e -> m (ProtoUArrayT m i e)

  -- | Create a new ptototype of mutable unboxed array by the specified session
  -- with every element initialised to an undefined value.
  newProtoUArray_ :: Ix i => SessionT m -> (i, i) -> m (ProtoUArrayT m i e)

  -- | Read an element from the mutable unboxed array.
  readProtoUArray :: Ix i => ProtoUArrayT m i e -> i -> m e

  -- | Write the element in the mutable unboxed array.
  writeProtoUArray :: Ix i => ProtoUArrayT m i e -> i -> e -> m ()

instance ProtoArraying IO where

  newtype ProtoArrayT IO i e = ProtoArray (IOArray i e)

  {-# SPECIALISE INLINE protoArrayBounds :: Ix i => ProtoArray i e -> IO (i, i) #-}
  protoArrayBounds (ProtoArray a) = getBounds a

  {-# SPECIALISE INLINE newProtoArray :: Ix i => Session -> (i, i) -> e -> IO (ProtoArray i e) #-}
  newProtoArray s bnds e = fmap ProtoArray $ newArray bnds e

  {-# SPECIALISE INLINE newProtoArray_ :: Ix i => Session -> (i, i) -> IO (ProtoArray i e) #-}
  newProtoArray_ s bnds = fmap ProtoArray $ newArray_ bnds

  {-# SPECIALISE INLINE readProtoArray :: Ix i => ProtoArray i e -> i -> IO e #-}
  readProtoArray (ProtoArray a) = readArray a

  {-# SPECIALISE INLINE writeProtoArray :: Ix i => ProtoArray i e -> i -> e -> IO () #-}
  writeProtoArray (ProtoArray a) = writeArray a

instance MArray IOUArray e IO => ProtoUArraying IO e where

  newtype ProtoUArrayT IO i e = ProtoUArray (IOUArray i e)

  {-# SPECIALISE INLINE protoUArrayBounds :: (MArray IOUArray Bool IO, Ix i) => ProtoUArray i Bool -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoUArrayBounds :: (MArray IOUArray Char IO, Ix i) => ProtoUArray i Char -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoUArrayBounds :: (MArray IOUArray Double IO, Ix i) => ProtoUArray i Double -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoUArrayBounds :: (MArray IOUArray Float IO, Ix i) => ProtoUArray i Float -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoUArrayBounds :: (MArray IOUArray Int IO, Ix i) => ProtoUArray i Int -> IO (i, i) #-}
  {-# SPECIALISE INLINE protoUArrayBounds :: (MArray IOUArray e IO, Ix i) => ProtoUArray i e -> IO (i, i) #-}
  protoUArrayBounds (ProtoUArray a) = getBounds a

  {-# SPECIALISE INLINE newProtoUArray :: (MArray IOUArray Bool IO, Ix i) => Session -> (i, i) -> Bool -> IO (ProtoUArray i Bool) #-}
  {-# SPECIALISE INLINE newProtoUArray :: (MArray IOUArray Char IO, Ix i) => Session -> (i, i) -> Char -> IO (ProtoUArray i Char) #-}
  {-# SPECIALISE INLINE newProtoUArray :: (MArray IOUArray Double IO, Ix i) => Session -> (i, i) -> Double -> IO (ProtoUArray i Double) #-}
  {-# SPECIALISE INLINE newProtoUArray :: (MArray IOUArray Float IO, Ix i) => Session -> (i, i) -> Float -> IO (ProtoUArray i Float) #-}
  {-# SPECIALISE INLINE newProtoUArray :: (MArray IOUArray Int IO, Ix i) => Session -> (i, i) -> Int -> IO (ProtoUArray i Int) #-}
  {-# SPECIALISE INLINE newProtoUArray :: (MArray IOUArray e IO, Ix i) => Session -> (i, i) -> e -> IO (ProtoUArray i e) #-}
  newProtoUArray s bnds e = fmap ProtoUArray $ newArray bnds e

  {-# SPECIALISE INLINE newProtoUArray_ :: (MArray IOUArray Bool IO, Ix i) => Session -> (i, i) -> IO (ProtoUArray i Bool) #-}
  {-# SPECIALISE INLINE newProtoUArray_ :: (MArray IOUArray Char IO, Ix i) => Session -> (i, i) -> IO (ProtoUArray i Char) #-}
  {-# SPECIALISE INLINE newProtoUArray_ :: (MArray IOUArray Double IO, Ix i) => Session -> (i, i) -> IO (ProtoUArray i Double) #-}
  {-# SPECIALISE INLINE newProtoUArray_ :: (MArray IOUArray Float IO, Ix i) => Session -> (i, i) -> IO (ProtoUArray i Float) #-}
  {-# SPECIALISE INLINE newProtoUArray_ :: (MArray IOUArray Int IO, Ix i) => Session -> (i, i) -> IO (ProtoUArray i Int) #-}
  {-# SPECIALISE INLINE newProtoUArray_ :: (MArray IOUArray e IO, Ix i) => Session -> (i, i) -> IO (ProtoUArray i e) #-}
  newProtoUArray_ s bnds = fmap ProtoUArray $ newArray_ bnds

  {-# SPECIALISE INLINE readProtoUArray :: (MArray IOUArray Bool IO, Ix i) => ProtoUArray i Bool -> i -> IO Bool #-}
  {-# SPECIALISE INLINE readProtoUArray :: (MArray IOUArray Char IO, Ix i) => ProtoUArray i Char -> i -> IO Char #-}
  {-# SPECIALISE INLINE readProtoUArray :: (MArray IOUArray Double IO, Ix i) => ProtoUArray i Double -> i -> IO Double #-}
  {-# SPECIALISE INLINE readProtoUArray :: (MArray IOUArray Float IO, Ix i) => ProtoUArray i Float -> i -> IO Float #-}
  {-# SPECIALISE INLINE readProtoUArray :: (MArray IOUArray Int IO, Ix i) => ProtoUArray i Int -> i -> IO Int #-}
  {-# SPECIALISE INLINE readProtoUArray :: (MArray IOUArray e IO, Ix i) => ProtoUArray i e -> i -> IO e #-}
  readProtoUArray (ProtoUArray a) = readArray a

  {-# SPECIALISE INLINE writeProtoUArray :: (MArray IOUArray Bool IO, Ix i) => ProtoUArray i Bool -> i -> Bool -> IO () #-}
  {-# SPECIALISE INLINE writeProtoUArray :: (MArray IOUArray Char IO, Ix i) => ProtoUArray i Char -> i -> Char -> IO () #-}
  {-# SPECIALISE INLINE writeProtoUArray :: (MArray IOUArray Double IO, Ix i) => ProtoUArray i Double -> i -> Double -> IO () #-}
  {-# SPECIALISE INLINE writeProtoUArray :: (MArray IOUArray Float IO, Ix i) => ProtoUArray i Float -> i -> Float -> IO () #-}
  {-# SPECIALISE INLINE writeProtoUArray :: (MArray IOUArray Int IO, Ix i) => ProtoUArray i Int -> i -> Int -> IO () #-}
  {-# SPECIALISE INLINE writeProtoUArray :: (MArray IOUArray e IO, Ix i) => ProtoUArray i e -> i -> e -> IO () #-}
  writeProtoUArray (ProtoUArray a) = writeArray a

-- | A convenient type synonym.
type ProtoArray = ProtoArrayT IO 

-- | A convenient type synonym.
type ProtoUArray = ProtoUArrayT IO
