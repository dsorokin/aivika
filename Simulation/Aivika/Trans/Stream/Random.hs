
-- |
-- Module     : Simulation.Aivika.Trans.Stream.Random
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines random streams of events, which are useful
-- for describing the input of the model.
--

module Simulation.Aivika.Trans.Stream.Random
       (-- * Stream of Random Events
        randomStream,
        randomUniformStream,
        randomUniformIntStream,
        randomNormalStream,
        randomExponentialStream,
        randomErlangStream,
        randomPoissonStream,
        randomBinomialStream) where

import System.Random

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Parameter.Random
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Processor
import Simulation.Aivika.Trans.Stream
import Simulation.Aivika.Trans.Statistics
import Simulation.Aivika.Trans.Ref
import Simulation.Aivika.Trans.Arrival

-- | Return a sream of random events that arrive with the specified delay.
randomStream :: Comp m
                => Parameter m (Double, a)
                -- ^ compute a pair of the delay and event of type @a@
                -> Stream m (Arrival a)
                -- ^ a stream of delayed events
{-# INLINABLE randomStream #-}
{-# SPECIALISE randomStream :: Parameter IO (Double, a) -> Stream IO (Arrival a) #-}
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
           "the random stream itself must always execute: randomStream."
       (delay, a) <- liftParameter delay
       holdProcess delay
       t2 <- liftDynamics time
       let arrival = Arrival { arrivalValue = a,
                               arrivalTime  = t2,
                               arrivalDelay =
                                 case t0 of
                                   Nothing -> Nothing
                                   Just t0 -> Just delay }
       return (arrival, Cons $ loop (Just t2))

-- | Create a new stream with delays distributed uniformly.
randomUniformStream :: Comp m
                       => Double
                       -- ^ the minimum delay
                       -> Double
                       -- ^ the maximum delay
                       -> Stream m (Arrival Double)
                       -- ^ the stream of random events with the delays generated
{-# INLINE randomUniformStream #-}
randomUniformStream min max =
  randomStream $
  randomUniform min max >>= \x ->
  return (x, x)

-- | Create a new stream with integer delays distributed uniformly.
randomUniformIntStream :: Comp m
                          => Int
                          -- ^ the minimum delay
                          -> Int
                          -- ^ the maximum delay
                          -> Stream m (Arrival Int)
                          -- ^ the stream of random events with the delays generated
{-# INLINE randomUniformIntStream #-}
randomUniformIntStream min max =
  randomStream $
  randomUniformInt min max >>= \x ->
  return (fromIntegral x, x)

-- | Create a new stream with delays distributed normally.
randomNormalStream :: Comp m
                      => Double
                      -- ^ the mean delay
                      -> Double
                      -- ^ the delay deviation
                      -> Stream m (Arrival Double)
                      -- ^ the stream of random events with the delays generated
{-# INLINE randomNormalStream #-}
randomNormalStream mu nu =
  randomStream $
  randomNormal mu nu >>= \x ->
  return (x, x)
         
-- | Return a new stream with delays distibuted exponentially with the specified mean
-- (the reciprocal of the rate).
randomExponentialStream :: Comp m
                           => Double
                           -- ^ the mean delay (the reciprocal of the rate)
                           -> Stream m (Arrival Double)
                           -- ^ the stream of random events with the delays generated
{-# INLINE randomExponentialStream #-}
randomExponentialStream mu =
  randomStream $
  randomExponential mu >>= \x ->
  return (x, x)
         
-- | Return a new stream with delays having the Erlang distribution with the specified
-- scale (the reciprocal of the rate) and shape parameters.
randomErlangStream :: Comp m
                      => Double
                      -- ^ the scale (the reciprocal of the rate)
                      -> Int
                      -- ^ the shape
                      -> Stream m (Arrival Double)
                      -- ^ the stream of random events with the delays generated
{-# INLINE randomErlangStream #-}
randomErlangStream beta m =
  randomStream $
  randomErlang beta m >>= \x ->
  return (x, x)

-- | Return a new stream with delays having the Poisson distribution with
-- the specified mean.
randomPoissonStream :: Comp m
                       => Double
                       -- ^ the mean delay
                       -> Stream m (Arrival Int)
                       -- ^ the stream of random events with the delays generated
{-# INLINE randomPoissonStream #-}
randomPoissonStream mu =
  randomStream $
  randomPoisson mu >>= \x ->
  return (fromIntegral x, x)

-- | Return a new stream with delays having the binomial distribution with the specified
-- probability and trials.
randomBinomialStream :: Comp m
                        => Double
                        -- ^ the probability
                        -> Int
                        -- ^ the number of trials
                        -> Stream m (Arrival Int)
                        -- ^ the stream of random events with the delays generated
{-# INLINE randomBinomialStream #-}
randomBinomialStream prob trials =
  randomStream $
  randomBinomial prob trials >>= \x ->
  return (fromIntegral x, x)
