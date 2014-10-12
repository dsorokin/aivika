
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
       (ProtoArraying(..)) where

import Data.Array
import Data.Array.IO.Safe

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef

-- | A monad within which computation we can create and work with
-- the prototype of mutable arrays.
class ProtoReferring m => ProtoArraying m where
  
  -- | A prototype of mutable array.
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

instance ProtoArraying IO where

  newtype ProtoArray IO a = ProtoArray (IOArray Int a)

  {-# SPECIALISE INLINE protoArrayCount :: ProtoArray IO a -> IO Int #-}
  protoArrayCount (ProtoArray a) = do { (0, n') <- getBounds a; return $ n' + 1 }

  {-# SPECIALISE INLINE newProtoArray :: Session IO -> Int -> a -> IO (ProtoArray IO a) #-}
  newProtoArray s n a = fmap ProtoArray $ newArray (0, n - 1) a

  {-# SPECIALISE INLINE newProtoArray_ :: Session IO -> Int -> IO (ProtoArray IO a) #-}
  newProtoArray_ s n = fmap ProtoArray $ newArray_ (0, n - 1)

  {-# SPECIALISE INLINE readProtoArray :: ProtoArray IO a -> Int -> IO a #-}
  readProtoArray (ProtoArray a) = readArray a

  {-# SPECIALISE INLINE writeProtoArray :: ProtoArray IO a -> Int -> a -> IO () #-}
  writeProtoArray (ProtoArray a) = writeArray a

  {-# SPECIALISE INLINE protoArrayToList :: ProtoArray IO a -> IO [a] #-}
  protoArrayToList (ProtoArray a) = getElems a

  {-# SPECIALISE INLINE protoArrayFromList :: [a] -> IO (ProtoArray IO a) #-}
  protoArrayFromList xs = fmap ProtoArray $ newListArray (0, length xs - 1) xs

  {-# SPECIALISE INLINE freezeProtoArray :: ProtoArray IO a -> IO (Array Int a) #-}
  freezeProtoArray (ProtoArray a) = freeze a
