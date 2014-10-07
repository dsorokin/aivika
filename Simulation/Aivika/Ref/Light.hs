
-- |
-- Module     : Simulation.Aivika.Ref.Light
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines a light-weight and more fast version of an updatable reference
-- that depends on the event queue but that doesn't supply with the signal notification.
--
module Simulation.Aivika.Ref.Light
       (Ref,
        newRef,
        readRef,
        writeRef,
        modifyRef) where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Event

-- | The 'Ref' type represents a mutable variable similar to the 'IORef' variable 
-- but only dependent on the event queue, which allows synchronizing the reference
-- with the model explicitly through the 'Event' monad.
newtype Ref a = 
  Ref { refValue :: IORef a }

-- | Create a new reference.
newRef :: a -> Simulation (Ref a)
newRef a =
  do x <- liftIO $ newIORef a
     return Ref { refValue = x }
     
-- | Read the value of a reference.
readRef :: Ref a -> Event a
readRef r = Event $ \p -> readIORef (refValue r)

-- | Write a new value into the reference.
writeRef :: Ref a -> a -> Event ()
writeRef r a = Event $ \p -> 
  a `seq` writeIORef (refValue r) a

-- | Mutate the contents of the reference.
modifyRef :: Ref a -> (a -> a) -> Event ()
modifyRef r f = Event $ \p -> 
  do a <- readIORef (refValue r)
     let b = f a
     b `seq` writeIORef (refValue r) b
