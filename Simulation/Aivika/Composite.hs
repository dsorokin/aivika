
{-# LANGUAGE MultiParamTypeClasses, RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Composite
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It defines the 'Composite' monad that allows constructing components which
-- can be then destroyed in case of need.
--
module Simulation.Aivika.Composite
       (-- * Composite Monad
        Composite,
        runComposite,
        runComposite_,
        runCompositeInStartTime_,
        runCompositeInStopTime_,
        disposableComposite) where

import Data.Monoid

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Simulation.Aivika.Parameter
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event

-- | It represents a composite which can be then destroyed in case of need.
newtype Composite a = Composite { runComposite :: DisposableEvent -> Event (a, DisposableEvent)
                                  -- ^ Run the computation returning the result
                                  -- and some 'DisposableEvent' that being applied
                                  -- destroys the composite, for example, unsubscribes
                                  -- from signals or cancels the processes.
                                  --
                                }

-- | Like 'runComposite' but retains the composite parts during the simulation.
runComposite_ :: Composite a -> Event a
runComposite_ m =
  do (a, _) <- runComposite m mempty
     return a

-- | Like 'runComposite_' but runs the computation in the start time.
runCompositeInStartTime_ :: Composite a -> Simulation a
runCompositeInStartTime_ = runEventInStartTime . runComposite_

-- | Like 'runComposite_' but runs the computation in the stop time.
runCompositeInStopTime_ :: Composite a -> Simulation a
runCompositeInStopTime_ = runEventInStopTime . runComposite_

-- | When destroying the composite, the specified action will be applied.
disposableComposite :: DisposableEvent -> Composite ()
disposableComposite h = Composite $ \h0 -> return ((), h0 <> h)

instance Functor Composite where

  fmap f (Composite m) =
    Composite $ \h0 ->
    do (a, h) <- m h0
       return (f a, h)

instance Applicative Composite where

  pure = return
  (<*>) = ap

instance Monad Composite where

  return a = Composite $ \h0 -> return (a, h0)

  (Composite m) >>= k =
    Composite $ \h0 ->
    do (a, h) <- m h0
       let Composite m' = k a
       (b, h') <- m' h
       return (b, h')

instance MonadIO Composite where

  liftIO m =
    Composite $ \h0 ->
    do a <- liftIO m
       return (a, h0)

instance MonadFix Composite where

  mfix f =
    Composite $ \h0 ->
    do rec (a, h) <- runComposite (f a) h0
       return (a, h)

instance ParameterLift Composite where

  liftParameter m =
    Composite $ \h0 ->
    do a <- liftParameter m
       return (a, h0)

instance SimulationLift Composite where

  liftSimulation m =
    Composite $ \h0 ->
    do a <- liftSimulation m
       return (a, h0)

instance DynamicsLift Composite where

  liftDynamics m =
    Composite $ \h0 ->
    do a <- liftDynamics m
       return (a, h0)

instance EventLift Composite where

  liftEvent m =
    Composite $ \h0 ->
    do a <- liftEvent m
       return (a, h0)
