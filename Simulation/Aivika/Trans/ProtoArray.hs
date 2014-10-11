
{-# LANGUAGE TypeFamilies #-}

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
        ProtoArray) where

import Data.Array
import Data.Array.IO.Safe

import Simulation.Aivika.Trans.Session

-- | A monad within which computation we can create and work with
-- the prototype of mutable arrays.
class Monad m => ProtoArraying m where
  
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

  -- | Return the elements of the mutable array in an immutable array.
  freezeProtoArray :: Ix i => ProtoArrayT m i e -> m (Array i e)

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

  {-# SPECIALISE INLINE freezeProtoArray :: Ix i => ProtoArray i e -> IO (Array i e) #-}
  freezeProtoArray (ProtoArray a) = freeze a

-- | A convenient type synonym.
type ProtoArray i e  = ProtoArrayT IO i e
