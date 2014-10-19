
-- |
-- Module     : Simulation.Aivika.Trans.Ref
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines an updatable reference that depends on the event queue.
--
module Simulation.Aivika.Trans.Ref
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

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Signal
import Simulation.Aivika.Trans.Signal

-- | The 'Ref' type represents a mutable variable similar to the 'IORef' variable 
-- but only dependent on the event queue, which allows synchronizing the reference
-- with the model explicitly through the 'Event' monad.
data Ref m a = 
  Ref { refValue :: ProtoRef m a, 
        refChangedSource :: SignalSource m a }

-- | Create a new reference.
newRef :: Comp m => a -> Simulation m (Ref m a)
{-# INLINE newRef #-}
newRef a =
  Simulation $ \r ->
  do let s = runSession r
     x <- newProtoRef s a
     s <- invokeSimulation r newSignalSource
     return Ref { refValue = x, 
                  refChangedSource = s }
     
-- | Read the value of a reference.
readRef :: Comp m => Ref m a -> Event m a
{-# INLINE readRef #-}
readRef r = Event $ \p -> readProtoRef (refValue r)

-- | Write a new value into the reference.
writeRef :: Comp m => Ref m a -> a -> Event m ()
{-# INLINE writeRef #-}
writeRef r a = Event $ \p -> 
  do a `seq` writeProtoRef (refValue r) a
     invokeEvent p $ triggerSignal (refChangedSource r) a

-- | Mutate the contents of the reference.
modifyRef :: Comp m => Ref m a -> (a -> a) -> Event m ()
{-# INLINE modifyRef #-}
modifyRef r f = Event $ \p -> 
  do a <- readProtoRef (refValue r)
     let b = f a
     b `seq` writeProtoRef (refValue r) b
     invokeEvent p $ triggerSignal (refChangedSource r) b

-- | Return a signal that notifies about every change of the reference state.
refChanged :: Comp m => Ref m a -> Signal m a
{-# INLINE refChanged #-}
refChanged v = publishSignal (refChangedSource v)

-- | Return a signal that notifies about every change of the reference state.
refChanged_ :: Comp m => Ref m a -> Signal m ()
{-# INLINE refChanged_ #-}
refChanged_ r = mapSignal (const ()) $ refChanged r
