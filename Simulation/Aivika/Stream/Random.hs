
-- |
-- Module     : Simulation.Aivika.Stream.Random
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines random streams of events, which are useful
-- for describing the input of the model.
--

module Simulation.Aivika.Stream.Random
       (-- * Stream of Random Events
        randomStream,
        randomUniformStream,
        randomUniformIntStream,
        randomTriangularStream,
        randomNormalStream,
        randomLogNormalStream,
        randomExponentialStream,
        randomErlangStream,
        randomPoissonStream,
        randomBinomialStream,
        randomGammaStream,
        randomBetaStream,
        randomWeibullStream,
        randomDiscreteStream) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Generator
import Simulation.Aivika.Parameter
import Simulation.Aivika.Parameter.Random
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Process
import Simulation.Aivika.Processor
import Simulation.Aivika.Stream
import Simulation.Aivika.Statistics
import Simulation.Aivika.Ref
import Simulation.Aivika.Arrival

-- | Return a sream of random events that arrive with the specified delay.
randomStream :: Parameter (Double, a)
                -- ^ compute a pair of the delay and event of type @a@
                -> Stream (Arrival a)
                -- ^ a stream of delayed events
randomStream delay = Cons $ loop Nothing where
  loop t0 =
    do t1 <- liftDynamics time
       case t0 of
         Nothing -> return ()
         Just t0 ->
           when (t1 /= t0) $
           error $
           "The time of requesting for a new random event is different from " ++
           "the time when the previous event has arrived. Probably, your model " ++
           "contains a logical error. The random events should be requested permanently. " ++
           "At least, they can be lost, for example, when trying to enqueue them, but " ++
           "the random stream itself must always work: randomStream."
       (delay, a) <- liftParameter delay
       when (delay > 0) $
         holdProcess delay
       t2 <- liftDynamics time
       let arrival = Arrival { arrivalValue = a,
                               arrivalTime  = t2,
                               arrivalDelay =
                                 case t0 of
                                   Nothing -> Nothing
                                   Just t0 -> Just delay }
       return (arrival, Cons $ loop (Just t2))

-- | Create a new stream with random delays distributed uniformly.
randomUniformStream :: Double
                       -- ^ the minimum delay
                       -> Double
                       -- ^ the maximum delay
                       -> Stream (Arrival Double)
                       -- ^ the stream of random events with the delays generated
randomUniformStream min max =
  randomStream $
  randomUniform min max >>= \x ->
  return (x, x)

-- | Create a new stream with integer random delays distributed uniformly.
randomUniformIntStream :: Int
                          -- ^ the minimum delay
                          -> Int
                          -- ^ the maximum delay
                          -> Stream (Arrival Int)
                          -- ^ the stream of random events with the delays generated
randomUniformIntStream min max =
  randomStream $
  randomUniformInt min max >>= \x ->
  return (fromIntegral x, x)

-- | Create a new stream with random delays having the triangular distribution.
randomTriangularStream :: Double
                          -- ^ the minimum delay
                          -> Double
                          -- ^ the median of the delay
                          -> Double
                          -- ^ the maximum delay
                          -> Stream (Arrival Double)
                          -- ^ the stream of random events with the delays generated
randomTriangularStream min median max =
  randomStream $
  randomTriangular min median max >>= \x ->
  return (x, x)

-- | Create a new stream with random delays distributed normally.
randomNormalStream :: Double
                      -- ^ the mean delay
                      -> Double
                      -- ^ the delay deviation
                      -> Stream (Arrival Double)
                      -- ^ the stream of random events with the delays generated
randomNormalStream mu nu =
  randomStream $
  randomNormal mu nu >>= \x ->
  return (x, x)

-- | Create a new stream with random delays having the lognormal distribution.
randomLogNormalStream :: Double
                         -- ^ the mean of a normal distribution which
                         -- this distribution is derived from
                         -> Double
                         -- ^ the deviation of a normal distribution which
                         -- this distribution is derived from
                         -> Stream (Arrival Double)
                         -- ^ the stream of random events with the delays generated
randomLogNormalStream mu nu =
  randomStream $
  randomLogNormal mu nu >>= \x ->
  return (x, x)

-- | Return a new stream with random delays distibuted exponentially with the specified mean
-- (the reciprocal of the rate).
randomExponentialStream :: Double
                           -- ^ the mean delay (the reciprocal of the rate)
                           -> Stream (Arrival Double)
                           -- ^ the stream of random events with the delays generated
randomExponentialStream mu =
  randomStream $
  randomExponential mu >>= \x ->
  return (x, x)
         
-- | Return a new stream with random delays having the Erlang distribution with the specified
-- scale (the reciprocal of the rate) and shape parameters.
randomErlangStream :: Double
                      -- ^ the scale (the reciprocal of the rate)
                      -> Int
                      -- ^ the shape
                      -> Stream (Arrival Double)
                      -- ^ the stream of random events with the delays generated
randomErlangStream beta m =
  randomStream $
  randomErlang beta m >>= \x ->
  return (x, x)

-- | Return a new stream with random delays having the Poisson distribution with
-- the specified mean.
randomPoissonStream :: Double
                       -- ^ the mean delay
                       -> Stream (Arrival Int)
                       -- ^ the stream of random events with the delays generated
randomPoissonStream mu =
  randomStream $
  randomPoisson mu >>= \x ->
  return (fromIntegral x, x)

-- | Return a new stream with random delays having the binomial distribution with the specified
-- probability and trials.
randomBinomialStream :: Double
                        -- ^ the probability
                        -> Int
                        -- ^ the number of trials
                        -> Stream (Arrival Int)
                        -- ^ the stream of random events with the delays generated
randomBinomialStream prob trials =
  randomStream $
  randomBinomial prob trials >>= \x ->
  return (fromIntegral x, x)

-- | Return a new stream with random delays having the Gamma distribution by the specified
-- shape and scale.
randomGammaStream :: Double
                     -- ^ the shape
                     -> Double
                     -- ^ the scale (a reciprocal of the rate)
                     -> Stream (Arrival Double)
                     -- ^ the stream of random events with the delays generated
randomGammaStream kappa theta =
  randomStream $
  randomGamma kappa theta >>= \x ->
  return (x, x)

-- | Return a new stream with random delays having the Beta distribution by the specified
-- shape parameters (alpha and beta).
randomBetaStream :: Double
                    -- ^ the shape (alpha)
                    -> Double
                    -- ^ the shape (beta)
                    -> Stream (Arrival Double)
                    -- ^ the stream of random events with the delays generated
randomBetaStream alpha beta =
  randomStream $
  randomBeta alpha beta >>= \x ->
  return (x, x)

-- | Return a new stream with random delays having the Weibull distribution by the specified
-- shape and scale.
randomWeibullStream :: Double
                       -- ^ shape
                       -> Double
                       -- ^ scale
                       -> Stream (Arrival Double)
                       -- ^ the stream of random events with the delays generated
randomWeibullStream alpha beta =
  randomStream $
  randomWeibull alpha beta >>= \x ->
  return (x, x)

-- | Return a new stream with random delays having the specified discrete distribution.
randomDiscreteStream :: DiscretePDF Double
                        -- ^ the discrete probability density function
                        -> Stream (Arrival Double)
                        -- ^ the stream of random events with the delays generated
randomDiscreteStream dpdf =
  randomStream $
  randomDiscrete dpdf >>= \x ->
  return (x, x)
