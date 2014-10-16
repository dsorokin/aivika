
{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.ProtoRef
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a prototype of mutable references.
--
module Simulation.Aivika.Trans.ProtoRef
       (ProtoReferring(..),
        ProtoRef(..)) where

import Data.IORef

import Simulation.Aivika.Trans.Session

-- | A monad within which computation we can create and work with
-- the prototype of mutable reference.
class (Functor m, Monad m) => ProtoReferring m where
  
  -- | A prototype of mutable reference.
  data ProtoRef m :: * -> *

  -- | Create a new ptototype of mutable reference by the specified session and initial value.
  newProtoRef :: Session m -> a -> m (ProtoRef m a)

  -- | Read the contents of the prototype of mutable reference.
  readProtoRef :: ProtoRef m a -> m a

  -- | Write a new value in the prototype of mutable reference.
  writeProtoRef :: ProtoRef m a -> a -> m ()

  -- | Modify a value stored in the prototype of mutable reference.
  modifyProtoRef :: ProtoRef m a -> (a -> a) -> m ()

  -- | A strict version of 'modifyProtoRef'.
  modifyProtoRef' :: ProtoRef m a -> (a -> a) -> m ()

instance ProtoReferring IO where

  newtype ProtoRef IO a = ProtoRef (IORef a)

  {-# SPECIALIZE INLINE newProtoRef :: Session IO -> a -> IO (ProtoRef IO a) #-}
  newProtoRef session = fmap ProtoRef . newIORef

  {-# SPECIALIZE INLINE readProtoRef :: ProtoRef IO a -> IO a #-}
  readProtoRef (ProtoRef x) = readIORef x

  {-# SPECIALIZE INLINE writeProtoRef :: ProtoRef IO a -> a -> IO () #-}
  writeProtoRef (ProtoRef x) = writeIORef x

  {-# SPECIALIZE INLINE modifyProtoRef :: ProtoRef IO a -> (a -> a) -> IO () #-}
  modifyProtoRef (ProtoRef x) = modifyIORef x

  {-# SPECIALIZE INLINE modifyProtoRef' :: ProtoRef IO a -> (a -> a) -> IO () #-}
  modifyProtoRef' (ProtoRef x) = modifyIORef' x
