
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Internal.Event
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines the 'Event' monad which is very similar to the 'Dynamics'
-- monad but only now the computation is strongly synchronized with the event queue.
--
module Simulation.Aivika.Internal.Event
       (-- * Event
        Event(..),
        EventLift(..),
        invokeEvent,
        -- * Error Handling
        catchEvent,
        finallyEvent,
        throwEvent) where

import qualified Control.Exception as C
import Control.Exception (IOException, throw, finally)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics

-- | A value in the 'Event' monad represents a polymorphic time varying function
-- which is strongly synchronized with the event queue.
newtype Event a = Event (Point -> IO a)

instance Monad Event where
  return  = returnE
  m >>= k = bindE m k

returnE :: a -> Event a
{-# INLINE returnE #-}
returnE a = Event (\p -> return a)

bindE :: Event a -> (a -> Event b) -> Event b
{-# INLINE bindE #-}
bindE (Event m) k = 
  Event $ \p -> 
  do a <- m p
     let Event m' = k a
     m' p

instance Functor Event where
  fmap = liftME

liftME :: (a -> b) -> Event a -> Event b
{-# INLINE liftME #-}
liftME f (Event x) =
  Event $ \p -> do { a <- x p; return $ f a }

instance MonadIO Event where
  liftIO m = Event $ const m

instance SimulationLift Event where
  liftSimulation = liftES
    
liftES :: Simulation a -> Event a
{-# INLINE liftES #-}
liftES (Simulation m) =
  Event $ \p -> m $ pointRun p

-- | A type class to lift the 'Event' computation to other monads.
class Monad m => EventLift m where
  
  -- | Lift the specified 'Event' computation to another monad.
  liftEvent :: Event a -> m a
  
-- | Exception handling within 'Event' computations.
catchEvent :: Event a -> (IOException -> Event a) -> Event a
catchEvent (Event m) h =
  Event $ \p -> 
  C.catch (m p) $ \e ->
  let Event m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyEvent :: Event a -> Event b -> Event a
finallyEvent (Event m) (Event m') =
  Event $ \p ->
  C.finally (m p) (m' p)

-- | Like the standard 'throw' function.
throwEvent :: IOException -> Event a
throwEvent = throw

-- | Invoke the 'Event' computation.
invokeEvent :: Event a -> Point -> IO a
{-# INLINE invokeEvent #-}
invokeEvent (Event m) p = m p

instance MonadFix Event where
  mfix f = 
    Event $ \p ->
    do { rec { a <- invokeEvent (f a) p }; return a }
