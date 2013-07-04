
-- |
-- Module     : Simulation.Aivika.Dynamics.Ref
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines an updatable reference that depends on the event queue.
--
module Simulation.Aivika.Dynamics.Ref
       (Ref,
        refQueue,
        refChanged,
        refChanged_,
        newRef,
        readRef,
        writeRef,
        modifyRef) where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Internal.Signal

-- | The 'Ref' type represents a mutable variable similar to the 'IORef' variable 
-- but only bound to some event queue, which makes the variable coordinated 
-- with that queue.
data Ref a = 
  Ref { refQueue :: EventQueue,  -- ^ Return the bound event queue.
        refRun   :: Dynamics (),
        refValue :: IORef a, 
        refChangedSource :: SignalSource a, 
        refUpdatedSource :: SignalSource a }

-- | Create a new reference bound to the specified event queue.
newRef :: EventQueue -> a -> Simulation (Ref a)
newRef q a =
  do x <- liftIO $ newIORef a
     s <- newSignalSourceUnsafe
     u <- newSignalSourceWithUpdate (runQueue q)
     return Ref { refQueue = q,
                  refRun   = runQueueSync q,
                  refValue = x, 
                  refChangedSource = s, 
                  refUpdatedSource = u }
     
-- | Read the value of a reference, forcing the bound event queue to raise 
-- the events in case of need.
readRef :: Ref a -> Dynamics a
readRef r = Dynamics $ \p -> 
  do invokeDynamics p $ refRun r
     readIORef (refValue r)

-- | Write a new value into the reference.
writeRef :: Ref a -> a -> Dynamics ()
writeRef r a = Dynamics $ \p -> 
  do a `seq` writeIORef (refValue r) a
     invokeDynamics p $ triggerSignal (refChangedSource r) a

-- | Mutate the contents of the reference, forcing the bound event queue to
-- raise all pending events in case of need.
modifyRef :: Ref a -> (a -> a) -> Dynamics ()
modifyRef r f = Dynamics $ \p -> 
  do invokeDynamics p $ refRun r
     a <- readIORef (refValue r)
     let b = f a
     b `seq` writeIORef (refValue r) b
     invokeDynamics p $ triggerSignal (refChangedSource r) b

-- | Return a signal that notifies about every change of the reference state.
refChanged :: Ref a -> Signal a
refChanged v = merge2Signals m1 m2    -- N.B. The order is important!
  where m1 = publishSignal (refUpdatedSource v)
        m2 = publishSignal (refChangedSource v)

-- | Return a signal that notifies about every change of the reference state.
refChanged_ :: Ref a -> Signal ()
refChanged_ r = mapSignal (const ()) $ refChanged r

invokeDynamics :: Point -> Dynamics a -> IO a
{-# INLINE invokeDynamics #-}
invokeDynamics p (Dynamics m) = m p
