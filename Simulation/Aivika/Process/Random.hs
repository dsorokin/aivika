
-- |
-- Module     : Simulation.Aivika.Process.Random
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines helper functions, which are useful to hold 
-- the 'Process' computation for a time interval according to some
-- random distribution.
--

module Simulation.Aivika.Process.Random
       (randomUniformProcess,
        randomUniformProcess_,
        randomUniformIntProcess,
        randomUniformIntProcess_,
        randomNormalProcess,
        randomNormalProcess_,
        randomExponentialProcess,
        randomExponentialProcess_,
        randomErlangProcess,
        randomErlangProcess_,
        randomPoissonProcess,
        randomPoissonProcess_,
        randomBinomialProcess,
        randomBinomialProcess_) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Parameter
import Simulation.Aivika.Parameter.Random
import Simulation.Aivika.Process

-- | Hold the process for a random time interval distributed uniformly.
randomUniformProcess :: Double
                        -- ^ the minimum time interval
                        -> Double
                        -- ^ the maximum time interval
                        -> Process Double
                        -- ^ a computation of the time interval
                        -- for which the process was actually held
randomUniformProcess min max =
  do t <- liftParameter $ randomUniform min max
     holdProcess t
     return t

-- | Hold the process for a random time interval distributed uniformly.
randomUniformProcess_ :: Double
                         -- ^ the minimum time interval
                         -> Double
                         -- ^ the maximum time interval
                         -> Process ()
randomUniformProcess_ min max =
  do t <- liftParameter $ randomUniform min max
     holdProcess t

-- | Hold the process for a random time interval distributed uniformly.
randomUniformIntProcess :: Int
                           -- ^ the minimum time interval
                           -> Int
                           -- ^ the maximum time interval
                           -> Process Int
                           -- ^ a computation of the time interval
                           -- for which the process was actually held
randomUniformIntProcess min max =
  do t <- liftParameter $ randomUniformInt min max
     holdProcess $ fromIntegral t
     return t

-- | Hold the process for a random time interval distributed uniformly.
randomUniformIntProcess_ :: Int
                            -- ^ the minimum time interval
                            -> Int
                            -- ^ the maximum time interval
                            -> Process ()
randomUniformIntProcess_ min max =
  do t <- liftParameter $ randomUniformInt min max
     holdProcess $ fromIntegral t

-- | Hold the process for a random time interval distributed normally.
randomNormalProcess :: Double
                       -- ^ the mean time interval
                       -> Double
                       -- ^ the time interval deviation
                       -> Process Double
                       -- ^ a computation of the time interval
                       -- for which the process was actually held
randomNormalProcess mu nu =
  do t <- liftParameter $ randomNormal mu nu
     holdProcess t
     return t
         
-- | Hold the process for a random time interval distributed normally.
randomNormalProcess_ :: Double
                        -- ^ the mean time interval
                        -> Double
                        -- ^ the time interval deviation
                        -> Process ()
randomNormalProcess_ mu nu =
  do t <- liftParameter $ randomNormal mu nu
     holdProcess t
         
-- | Hold the process for a random time interval distributed exponentially
-- with the specified mean (the reciprocal of the rate).
randomExponentialProcess :: Double
                            -- ^ the mean time interval (the reciprocal of the rate)
                            -> Process Double
                            -- ^ a computation of the time interval
                            -- for which the process was actually held
randomExponentialProcess mu =
  do t <- liftParameter $ randomExponential mu
     holdProcess t
     return t
         
-- | Hold the process for a random time interval distributed exponentially
-- with the specified mean (the reciprocal of the rate).
randomExponentialProcess_ :: Double
                             -- ^ the mean time interval (the reciprocal of the rate)
                             -> Process ()
randomExponentialProcess_ mu =
  do t <- liftParameter $ randomExponential mu
     holdProcess t
         
-- | Hold the process for a random time interval having the Erlang distribution with
-- the specified scale (the reciprocal of the rate) and shape parameters.
randomErlangProcess :: Double
                       -- ^ the scale (the reciprocal of the rate)
                       -> Int
                       -- ^ the shape
                       -> Process Double
                       -- ^ a computation of the time interval
                       -- for which the process was actually held
randomErlangProcess beta m =
  do t <- liftParameter $ randomErlang beta m
     holdProcess t
     return t

-- | Hold the process for a random time interval having the Erlang distribution with
-- the specified scale (the reciprocal of the rate) and shape parameters.
randomErlangProcess_ :: Double
                        -- ^ the scale (the reciprocal of the rate)
                        -> Int
                        -- ^ the shape
                        -> Process ()
randomErlangProcess_ beta m =
  do t <- liftParameter $ randomErlang beta m
     holdProcess t

-- | Hold the process for a random time interval having the Poisson distribution with
-- the specified mean.
randomPoissonProcess :: Double
                        -- ^ the mean time interval
                        -> Process Int
                        -- ^ a computation of the time interval
                        -- for which the process was actually held
randomPoissonProcess mu =
  do t <- liftParameter $ randomPoisson mu
     holdProcess $ fromIntegral t
     return t

-- | Hold the process for a random time interval having the Poisson distribution with
-- the specified mean.
randomPoissonProcess_ :: Double
                         -- ^ the mean time interval
                         -> Process ()
randomPoissonProcess_ mu =
  do t <- liftParameter $ randomPoisson mu
     holdProcess $ fromIntegral t

-- | Hold the process for a random time interval having the binomial distribution
-- with the specified probability and trials.
randomBinomialProcess :: Double
                         -- ^ the probability
                         -> Int
                         -- ^ the number of trials
                         -> Process Int
                         -- ^ a computation of the time interval
                         -- for which the process was actually held
randomBinomialProcess prob trials =
  do t <- liftParameter $ randomBinomial prob trials
     holdProcess $ fromIntegral t
     return t

-- | Hold the process for a random time interval having the binomial distribution
-- with the specified probability and trials.
randomBinomialProcess_ :: Double
                         -- ^ the probability
                         -> Int
                         -- ^ the number of trials
                         -> Process ()
randomBinomialProcess_ prob trials =
  do t <- liftParameter $ randomBinomial prob trials
     holdProcess $ fromIntegral t
