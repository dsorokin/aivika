
-- |
-- Module     : Simulation.Aivika.Dynamics.Ref
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module defines an updatable reference that depends on the event queue.
--
module Simulation.Aivika.Dynamics.Ref
       (Ref,
        newRef,
        refQueue,
        readRef,
        writeRef,
        modifyRef) where

import Data.IORef
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.EventQueue

-- | The 'Ref' type represents a mutable variable similar to the 'IORef' variable 
-- but only bound to some event queue, which makes the variable coordinated 
-- with that queue.
data Ref a = 
  Ref { refQueue :: EventQueue,  -- ^ Return the bound event queue.
        refRun   :: Dynamics (),
        refValue :: IORef a }

-- | Create a new reference bound to the specified event queue.
newRef :: EventQueue -> a -> Dynamics (Ref a)
newRef q a =
  do x <- liftIO $ newIORef a
     return Ref { refQueue = q,
                  refRun   = queueRun q,
                  refValue = x }
     
-- | Read the value of a reference, forcing the bound event queue to raise 
-- the events in case of need.
readRef :: Ref a -> Dynamics a
readRef r = Dynamics $ \p -> 
  do let Dynamics m = refRun r
     m p
     readIORef (refValue r)

-- | Write a new value into the reference.
writeRef :: Ref a -> a -> Dynamics ()
writeRef r a = Dynamics $ \p -> 
  a `seq` writeIORef (refValue r) a

-- | Mutate the contents of the reference, forcing the bound event queue to
-- raise all pending events in case of need.
modifyRef :: Ref a -> (a -> a) -> Dynamics ()
modifyRef r f = Dynamics $ \p -> 
  do let Dynamics m = refRun r
     m p
     a <- readIORef (refValue r)
     let b = f a
     b `seq` writeIORef (refValue r) b
