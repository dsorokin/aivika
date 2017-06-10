
-- |
-- Module     : Simulation.Aivika.Signal.Random
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines random signals of events, which are useful
-- for describing the input of the model.
--

module Simulation.Aivika.Signal.Random
       (-- * Signal of Random Events
        newRandomSignal,
        newRandomUniformSignal,
        newRandomUniformIntSignal,
        newRandomTriangularSignal,
        newRandomNormalSignal,
        newRandomLogNormalSignal,
        newRandomExponentialSignal,
        newRandomErlangSignal,
        newRandomPoissonSignal,
        newRandomBinomialSignal,
        newRandomGammaSignal,
        newRandomBetaSignal,
        newRandomWeibullSignal,
        newRandomDiscreteSignal) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Generator
import Simulation.Aivika.Parameter
import Simulation.Aivika.Parameter.Random
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Composite
import Simulation.Aivika.Process
import Simulation.Aivika.Signal
import Simulation.Aivika.Statistics
import Simulation.Aivika.Arrival

-- | Return a signal of random events that arrive with the specified delay.
newRandomSignal :: Parameter (Double, a)
                   -- ^ compute a pair of the delay and event of type @a@
                   -> Composite (Signal (Arrival a))
                   -- ^ the computation that returns a signal emitting the delayed events
newRandomSignal delay =
  do source <- liftSimulation newSignalSource
     let loop t0 =
           do (delay, a) <- liftParameter delay
              when (delay > 0) $
                holdProcess delay
              t2 <- liftDynamics time
              let arrival = Arrival { arrivalValue = a,
                                      arrivalTime  = t2,
                                      arrivalDelay =
                                        case t0 of
                                          Nothing -> Nothing
                                          Just t0 -> Just delay }
              liftEvent $
                triggerSignal source arrival
              loop (Just t2)
     pid <- liftSimulation newProcessId
     liftEvent $
       runProcessUsingId pid $
       loop Nothing
     disposableComposite $
       DisposableEvent $
       cancelProcessWithId pid
     return $ publishSignal source

-- | Create a new signal with random delays distributed uniformly.
newRandomUniformSignal :: Double
                          -- ^ the minimum delay
                          -> Double
                          -- ^ the maximum delay
                          -> Composite (Signal (Arrival Double))
                          -- ^ the computation of signal emitting random events with the delays generated
newRandomUniformSignal min max =
  newRandomSignal $
  randomUniform min max >>= \x ->
  return (x, x)

-- | Create a new signal with integer random delays distributed uniformly.
newRandomUniformIntSignal :: Int
                             -- ^ the minimum delay
                             -> Int
                             -- ^ the maximum delay
                             -> Composite (Signal (Arrival Int))
                             -- ^ the computation of signal emitting random events with the delays generated
newRandomUniformIntSignal min max =
  newRandomSignal $
  randomUniformInt min max >>= \x ->
  return (fromIntegral x, x)

-- | Create a new signal with random delays having the triangular distribution.
newRandomTriangularSignal :: Double
                             -- ^ the minimum delay
                             -> Double
                             -- ^ the median of the delay
                             -> Double
                             -- ^ the maximum delay
                             -> Composite (Signal (Arrival Double))
                             -- ^ the computation of signal emitting random events with the delays generated
newRandomTriangularSignal min median max =
  newRandomSignal $
  randomTriangular min median max >>= \x ->
  return (x, x)

-- | Create a new signal with random delays distributed normally.
newRandomNormalSignal :: Double
                         -- ^ the mean delay
                         -> Double
                         -- ^ the delay deviation
                         -> Composite (Signal (Arrival Double))
                         -- ^ the computation of signal emitting random events with the delays generated
newRandomNormalSignal mu nu =
  newRandomSignal $
  randomNormal mu nu >>= \x ->
  return (x, x)

-- | Create a new signal with random delays having the lognormal distribution.
newRandomLogNormalSignal :: Double
                            -- ^ the mean of a normal distribution which
                            -- this distribution is derived from
                            -> Double
                            -- ^ the deviation of a normal distribution which
                            -- this distribution is derived from
                            -> Composite (Signal (Arrival Double))
                            -- ^ the computation of signal emitting random events with the delays generated
newRandomLogNormalSignal mu nu =
  newRandomSignal $
  randomLogNormal mu nu >>= \x ->
  return (x, x)

-- | Return a new signal with random delays distibuted exponentially with the specified mean
-- (the reciprocal of the rate).
newRandomExponentialSignal :: Double
                              -- ^ the mean delay (the reciprocal of the rate)
                              -> Composite (Signal (Arrival Double))
                              -- ^ the computation of signal emitting random events with the delays generated
newRandomExponentialSignal mu =
  newRandomSignal $
  randomExponential mu >>= \x ->
  return (x, x)
         
-- | Return a new signal with random delays having the Erlang distribution with the specified
-- scale (the reciprocal of the rate) and shape parameters.
newRandomErlangSignal :: Double
                         -- ^ the scale (the reciprocal of the rate)
                         -> Int
                         -- ^ the shape
                         -> Composite (Signal (Arrival Double))
                         -- ^ the computation of signal emitting random events with the delays generated
newRandomErlangSignal beta m =
  newRandomSignal $
  randomErlang beta m >>= \x ->
  return (x, x)

-- | Return a new signal with random delays having the Poisson distribution with
-- the specified mean.
newRandomPoissonSignal :: Double
                          -- ^ the mean delay
                          -> Composite (Signal (Arrival Int))
                          -- ^ the computation of signal emitting random events with the delays generated
newRandomPoissonSignal mu =
  newRandomSignal $
  randomPoisson mu >>= \x ->
  return (fromIntegral x, x)

-- | Return a new signal with random delays having the binomial distribution with the specified
-- probability and trials.
newRandomBinomialSignal :: Double
                           -- ^ the probability
                           -> Int
                           -- ^ the number of trials
                           -> Composite (Signal (Arrival Int))
                           -- ^ the computation of signal emitting random events with the delays generated
newRandomBinomialSignal prob trials =
  newRandomSignal $
  randomBinomial prob trials >>= \x ->
  return (fromIntegral x, x)

-- | Return a new signal with random delays having the Gamma distribution by the specified
-- shape and scale.
newRandomGammaSignal :: Double
                        -- ^ the shape
                        -> Double
                        -- ^ the scale (a reciprocal of the rate)
                        -> Composite (Signal (Arrival Double))
                        -- ^ the computation of signal emitting random events with the delays generated
newRandomGammaSignal kappa theta =
  newRandomSignal $
  randomGamma kappa theta >>= \x ->
  return (x, x)

-- | Return a new signal with random delays having the Beta distribution by the specified
-- shape parameters (alpha and beta).
newRandomBetaSignal :: Double
                       -- ^ the shape (alpha)
                       -> Double
                       -- ^ the shape (beta)
                       -> Composite (Signal (Arrival Double))
                       -- ^ the computation of signal emitting random events with the delays generated
newRandomBetaSignal alpha beta =
  newRandomSignal $
  randomBeta alpha beta >>= \x ->
  return (x, x)

-- | Return a new signal with random delays having the Weibull distribution by the specified
-- shape and scale.
newRandomWeibullSignal :: Double
                          -- ^ shape
                          -> Double
                          -- ^ scale
                          -> Composite (Signal (Arrival Double))
                          -- ^ the computation of signal emitting random events with the delays generated
newRandomWeibullSignal alpha beta =
  newRandomSignal $
  randomWeibull alpha beta >>= \x ->
  return (x, x)

-- | Return a new signal with random delays having the specified discrete distribution.
newRandomDiscreteSignal :: DiscretePDF Double
                           -- ^ the discrete probability density function
                           -> Composite (Signal (Arrival Double))
                           -- ^ the computation of signal emitting random events with the delays generated
newRandomDiscreteSignal dpdf =
  newRandomSignal $
  randomDiscrete dpdf >>= \x ->
  return (x, x)
