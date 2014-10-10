
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.ProtoArray
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a prototype of all mutable arrays.
--
module Simulation.Aivika.Trans.Internal.ProtoArray
       (ProtoArraying(..),
        ProtoArray,
        ProtoUArraying(..),
        ProtoUArray) where

import Data.Array.IO.Safe

import Simulation.Aivika.Trans.Internal.Session

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

  {-# SPECIALISE INLINE protoUArrayBounds :: (MArray IOUArray e IO, Ix i) => ProtoUArray i e -> IO (i, i) #-}
  protoUArrayBounds (ProtoUArray a) = getBounds a

  {-# SPECIALISE INLINE newProtoUArray :: (MArray IOUArray e IO, Ix i) => Session -> (i, i) -> e -> IO (ProtoUArray i e) #-}
  newProtoUArray s bnds e = fmap ProtoUArray $ newArray bnds e

  {-# SPECIALISE INLINE newProtoUArray_ :: (MArray IOUArray e IO, Ix i) => Session -> (i, i) -> IO (ProtoUArray i e) #-}
  newProtoUArray_ s bnds = fmap ProtoUArray $ newArray_ bnds

  {-# SPECIALISE INLINE readProtoUArray :: (MArray IOUArray e IO, Ix i) => ProtoUArray i e -> i -> IO e #-}
  readProtoUArray (ProtoUArray a) = readArray a

  {-# SPECIALISE INLINE writeProtoUArray :: (MArray IOUArray e IO, Ix i) => ProtoUArray i e -> i -> e -> IO () #-}
  writeProtoUArray (ProtoUArray a) = writeArray a

-- | A convenient type synonym.
type ProtoArray = ProtoArrayT IO 

-- | A convenient type synonym.
type ProtoUArray = ProtoUArrayT IO
