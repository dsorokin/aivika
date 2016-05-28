
-- |
-- Module     : Simulation.Aivika.Processor.Random
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines some useful random processors that
-- hold the current process for the corresponding time interval,
-- when processing every input element.
--

module Simulation.Aivika.Processor.Random
       (randomUniformProcessor,
        randomUniformIntProcessor,
        randomTriangularProcessor,
        randomNormalProcessor,
        randomLogNormalProcessor,
        randomExponentialProcessor,
        randomErlangProcessor,
        randomPoissonProcessor,
        randomBinomialProcessor,
        randomGammaProcessor,
        randomBetaProcessor,
        randomWeibullProcessor,
        randomDiscreteProcessor) where

import Simulation.Aivika.Generator
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
-- for a random time interval having the triangular distribution.
randomTriangularProcessor :: Double
                             -- ^ the minimum time interval
                             -> Double
                             -- ^ the median of the time interval
                             -> Double
                             -- ^ the maximum time interval
                             -> Processor a a
randomTriangularProcessor min median max =
  withinProcessor $
  randomTriangularProcess_ min median max

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
-- for a random time interval having the lognormal distribution.
randomLogNormalProcessor :: Double
                            -- ^ the mean for a normal distribution
                            -- which this distribution is derived from
                            -> Double
                            -- ^ the deviation for a normal distribution
                            -- which this distribution is derived from
                            -> Processor a a
randomLogNormalProcessor mu nu =
  withinProcessor $
  randomLogNormalProcess_ mu nu

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

-- | When processing every input element, hold the process
-- for a random time interval having the Gamma distribution
-- with the specified shape and scale.
randomGammaProcessor :: Double
                        -- ^ the shape
                        -> Double
                        -- ^ the scale (a reciprocal of the rate)
                        -> Processor a a
randomGammaProcessor kappa theta =
  withinProcessor $
  randomGammaProcess_ kappa theta

-- | When processing every input element, hold the process
-- for a random time interval having the Beta distribution
-- with the specified shape parameters (alpha and beta).
randomBetaProcessor :: Double
                       -- ^ shape (alpha)
                       -> Double
                       -- ^ shape (beta)
                       -> Processor a a
randomBetaProcessor alpha beta =
  withinProcessor $
  randomBetaProcess_ alpha beta

-- | When processing every input element, hold the process
-- for a random time interval having the Weibull distribution
-- with the specified shape and scale.
randomWeibullProcessor :: Double
                          -- ^ shape
                          -> Double
                          -- ^ scale
                          -> Processor a a
randomWeibullProcessor alpha beta =
  withinProcessor $
  randomWeibullProcess_ alpha beta

-- | When processing every input element, hold the process
-- for a random time interval having the specified discrete distribution.
randomDiscreteProcessor :: DiscretePDF Double
                           -- ^ the discrete probability density function
                           -> Processor a a
randomDiscreteProcessor dpdf =
  withinProcessor $
  randomDiscreteProcess_ dpdf
