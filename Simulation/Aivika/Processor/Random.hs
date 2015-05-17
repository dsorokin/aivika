
-- |
-- Module     : Simulation.Aivika.Processor.Random
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines some useful random processors that
-- hold the current process for the corresponding time interval,
-- when processing every input element.
--

module Simulation.Aivika.Processor.Random
       (randomUniformProcessor,
        randomUniformIntProcessor,
        randomNormalProcessor,
        randomExponentialProcessor,
        randomErlangProcessor,
        randomPoissonProcessor,
        randomBinomialProcessor) where

import Simulation.Aivika.Process
import Simulation.Aivika.Process.Random
import Simulation.Aivika.Processor

-- | When processing every input element, hold the process
-- for a random time interval distributed uniformly.
randomUniformProcessor :: Double
                          -- ^ the minimum time interval
                          -> Double
                          -- ^ the maximum time interval
                          -> Processor a a
randomUniformProcessor min max =
  withinProcessor $
  randomUniformProcess_ min max

-- | When processing every input element, hold the process
-- for a random time interval distributed uniformly.
randomUniformIntProcessor :: Int
                             -- ^ the minimum time interval
                             -> Int
                             -- ^ the maximum time interval
                             -> Processor a a
randomUniformIntProcessor min max =
  withinProcessor $
  randomUniformIntProcess_ min max

-- | When processing every input element, hold the process
-- for a random time interval distributed normally.
randomNormalProcessor :: Double
                         -- ^ the mean time interval
                         -> Double
                         -- ^ the time interval deviation
                         -> Processor a a
randomNormalProcessor mu nu =
  withinProcessor $
  randomNormalProcess_ mu nu
         
-- | When processing every input element, hold the process
-- for a random time interval distributed exponentially
-- with the specified mean (the reciprocal of the rate).
randomExponentialProcessor :: Double
                              -- ^ the mean time interval (the reciprocal of the rate)
                              -> Processor a a
randomExponentialProcessor mu =
  withinProcessor $
  randomExponentialProcess_ mu
         
-- | When processing every input element, hold the process
-- for a random time interval having the Erlang distribution with
-- the specified scale (the reciprocal of the rate) and shape parameters.
randomErlangProcessor :: Double
                         -- ^ the scale (the reciprocal of the rate)
                         -> Int
                         -- ^ the shape
                         -> Processor a a
randomErlangProcessor beta m =
  withinProcessor $
  randomErlangProcess_ beta m

-- | When processing every input element, hold the process
-- for a random time interval having the Poisson distribution
-- with the specified mean.
randomPoissonProcessor :: Double
                          -- ^ the mean time interval
                          -> Processor a a
randomPoissonProcessor mu =
  withinProcessor $
  randomPoissonProcess_ mu

-- | When processing every input element, hold the process
-- for a random time interval having the binomial distribution
-- with the specified probability and trials.
randomBinomialProcessor :: Double
                           -- ^ the probability
                           -> Int
                           -- ^ the number of trials
                           -> Processor a a
randomBinomialProcessor prob trials =
  withinProcessor $
  randomBinomialProcess_ prob trials
