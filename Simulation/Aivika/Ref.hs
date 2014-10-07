
-- |
-- Module     : Simulation.Aivika.Ref
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines an updatable reference that depends on the event queue.
--
module Simulation.Aivika.Ref
       (Ref,
        refChanged,
        refChanged_,
        newRef,
        readRef,
        writeRef,
        modifyRef) where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Signal
import Simulation.Aivika.Signal

-- | The 'Ref' type represents a mutable variable similar to the 'IORef' variable 
-- but only dependent on the event queue, which allows synchronizing the reference
-- with the model explicitly through the 'Event' monad.
data Ref a = 
  Ref { refValue :: IORef a, 
        refChangedSource :: SignalSource a }

-- | Create a new reference.
newRef :: a -> Simulation (Ref a)
newRef a =
  do x <- liftIO $ newIORef a
     s <- newSignalSource
     return Ref { refValue = x, 
                  refChangedSource = s }
     
-- | Read the value of a reference.
readRef :: Ref a -> Event a
readRef r = Event $ \p -> readIORef (refValue r)

-- | Write a new value into the reference.
writeRef :: Ref a -> a -> Event ()
writeRef r a = Event $ \p -> 
  do a `seq` writeIORef (refValue r) a
     invokeEvent p $ triggerSignal (refChangedSource r) a

-- | Mutate the contents of the reference.
modifyRef :: Ref a -> (a -> a) -> Event ()
modifyRef r f = Event $ \p -> 
  do a <- readIORef (refValue r)
     let b = f a
     b `seq` writeIORef (refValue r) b
     invokeEvent p $ triggerSignal (refChangedSource r) b

-- | Return a signal that notifies about every change of the reference state.
refChanged :: Ref a -> Signal a
refChanged v = publishSignal (refChangedSource v)

-- | Return a signal that notifies about every change of the reference state.
refChanged_ :: Ref a -> Signal ()
refChanged_ r = mapSignal (const ()) $ refChanged r
