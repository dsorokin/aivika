
-- |
-- Module     : Simulation.Aivika.Stream.Random
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines random streams of data, which are useful
-- for describing the input of the model.
--

module Simulation.Aivika.Stream.Random
       (randomUniformStream,
        randomNormalStream,
        randomExponentialStream,
        randomErlangStream,
        randomPoissonStream,
        randomBinomialStream) where

import System.Random

import Control.Monad.Trans

import Simulation.Aivika.Parameter
import Simulation.Aivika.Parameter.Random
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Process
import Simulation.Aivika.Stream

-- | Create a new stream with delays distributed uniformly.
randomUniformStream :: Double              -- ^ the minimum delay
                       -> Double           -- ^ the maximum delay
                       -> Stream Double    -- ^ the stream of delays
randomUniformStream min max = Cons z where
  z = do delay <- liftParameter $ randomUniform min max
         holdProcess delay
         return (delay, randomUniformStream min max)

-- | Create a new stream with delays distributed normally.
randomNormalStream :: Double              -- ^ the mean delay
                      -> Double           -- ^ the delay deviation
                      -> Stream Double    -- ^ the stream of delays
randomNormalStream mu nu = Cons z where
  z = do delay <- liftParameter $ randomNormal mu nu
         holdProcess delay
         return (delay, randomNormalStream mu nu)
         
-- | Return a new stream with delays distibuted exponentially with the specified mean
-- (the reciprocal of the rate).
randomExponentialStream :: Double
                           -- ^ the mean delay (the reciprocal of the rate)
                           -> Stream Double
                           -- ^ the stream of delays
randomExponentialStream mu = Cons z where
  z = do delay <- liftParameter $ randomExponential mu
         holdProcess delay
         return (delay, randomExponentialStream mu)
         
-- | Return a new stream with delays having the Erlang distribution with the specified
-- scale (the reciprocal of the rate) and shape parameters.
randomErlangStream :: Double            -- ^ the scale (the reciprocal of the rate)
                      -> Int            -- ^ the shape
                      -> Stream Double  -- ^ the stream of delays
randomErlangStream beta m = Cons z where
  z = do delay <- liftParameter $ randomErlang beta m
         holdProcess delay
         return (delay, randomErlangStream beta m)

-- | Return a new stream with delays having the Poisson distribution with
-- the specified mean.
randomPoissonStream :: Double           -- ^ the mean delay
                       -> Stream Int    -- ^ the stream of delays
randomPoissonStream mu = Cons z where
  z = do delay <- liftParameter $ randomPoisson mu
         holdProcess $ fromIntegral delay
         return (delay, randomPoissonStream mu)

-- | Return a new stream with delays having the binomial distribution with the specified
-- probability and trials.
randomBinomialStream :: Double           -- ^ the probability
                        -> Int           -- ^ the number of trials
                        -> Stream Int    -- ^ the stream of delays
randomBinomialStream prob trials = Cons z where
  z = do delay <- liftParameter $ randomBinomial prob trials
         holdProcess $ fromIntegral delay
         return (delay, randomBinomialStream prob trials)
