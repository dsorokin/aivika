
-- |
-- Module     : Simulation.Aivika.Net.Random
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines some useful random network computations that
-- hold the current process for the corresponding time interval,
-- when processing every input element.
--

module Simulation.Aivika.Net.Random
       (randomUniformNet,
        randomUniformIntNet,
        randomTriangularNet,
        randomNormalNet,
        randomLogNormalNet,
        randomExponentialNet,
        randomErlangNet,
        randomPoissonNet,
        randomBinomialNet,
        randomGammaNet,
        randomBetaNet,
        randomWeibullNet,
        randomDiscreteNet) where

import Simulation.Aivika.Generator
import Simulation.Aivika.Process
import Simulation.Aivika.Process.Random
import Simulation.Aivika.Net

-- | When processing every input element, hold the process
-- for a random time interval distributed uniformly.
randomUniformNet :: Double
                    -- ^ the minimum time interval
                    -> Double
                    -- ^ the maximum time interval
                    -> Net a a
randomUniformNet min max =
  withinNet $
  randomUniformProcess_ min max

-- | When processing every input element, hold the process
-- for a random time interval distributed uniformly.
randomUniformIntNet :: Int
                       -- ^ the minimum time interval
                       -> Int
                       -- ^ the maximum time interval
                       -> Net a a
randomUniformIntNet min max =
  withinNet $
  randomUniformIntProcess_ min max

-- | When processing every input element, hold the process
-- for a random time interval having the triangular distribution.
randomTriangularNet :: Double
                       -- ^ the minimum time interval
                       -> Double
                       -- ^ the median of the time interval
                       -> Double
                       -- ^ the maximum time interval
                       -> Net a a
randomTriangularNet min median max =
  withinNet $
  randomTriangularProcess_ min median max

-- | When processing every input element, hold the process
-- for a random time interval distributed normally.
randomNormalNet :: Double
                   -- ^ the mean time interval
                   -> Double
                   -- ^ the time interval deviation
                   -> Net a a
randomNormalNet mu nu =
  withinNet $
  randomNormalProcess_ mu nu
         
-- | When processing every input element, hold the process
-- for a random time interval having the lognormal distribution.
randomLogNormalNet :: Double
                      -- ^ the mean of a normal distribution which
                      -- this distribution is derived from
                      -> Double
                      -- ^ the deviation of a normal distribution which
                      -- this distribution is derived from
                      -> Net a a
randomLogNormalNet mu nu =
  withinNet $
  randomLogNormalProcess_ mu nu

-- | When processing every input element, hold the process
-- for a random time interval distributed exponentially
-- with the specified mean (the reciprocal of the rate).
randomExponentialNet :: Double
                        -- ^ the mean time interval (the reciprocal of the rate)
                        -> Net a a
randomExponentialNet mu =
  withinNet $
  randomExponentialProcess_ mu
         
-- | When processing every input element, hold the process
-- for a random time interval having the Erlang distribution with
-- the specified scale (the reciprocal of the rate) and shape parameters.
randomErlangNet :: Double
                   -- ^ the scale (the reciprocal of the rate)
                   -> Int
                   -- ^ the shape
                   -> Net a a
randomErlangNet beta m =
  withinNet $
  randomErlangProcess_ beta m

-- | When processing every input element, hold the process
-- for a random time interval having the Poisson distribution
-- with the specified mean.
randomPoissonNet :: Double
                    -- ^ the mean time interval
                    -> Net a a
randomPoissonNet mu =
  withinNet $
  randomPoissonProcess_ mu

-- | When processing every input element, hold the process
-- for a random time interval having the binomial distribution
-- with the specified probability and trials.
randomBinomialNet :: Double
                     -- ^ the probability
                     -> Int
                     -- ^ the number of trials
                     -> Net a a
randomBinomialNet prob trials =
  withinNet $
  randomBinomialProcess_ prob trials

-- | When processing every input element, hold the process
-- for a random time interval having the Gamma distribution
-- with the specified shape and scale.
randomGammaNet :: Double
                  -- ^ the shape
                  -> Double
                  -- ^ the scale (a reciprocal of the rate)
                  -> Net a a
randomGammaNet kappa theta =
  withinNet $
  randomGammaProcess_ kappa theta

-- | When processing every input element, hold the process
-- for a random time interval having the Beta distribution
-- with the specified shape parameters (alpha and beta).
randomBetaNet :: Double
                 -- ^ shape (alpha)
                 -> Double
                 -- ^ shape (beta)
                 -> Net a a
randomBetaNet alpha beta =
  withinNet $
  randomBetaProcess_ alpha beta

-- | When processing every input element, hold the process
-- for a random time interval having the Weibull distribution
-- with the specified shape and scale.
randomWeibullNet :: Double
                    -- ^ shape
                    -> Double
                    -- ^ scale
                    -> Net a a
randomWeibullNet alpha beta =
  withinNet $
  randomWeibullProcess_ alpha beta

-- | When processing every input element, hold the process
-- for a random time interval having the specified discrete distribution.
randomDiscreteNet :: DiscretePDF Double
                     -- ^ the discrete probability density function
                     -> Net a a
randomDiscreteNet dpdf =
  withinNet $
  randomDiscreteProcess_ dpdf
