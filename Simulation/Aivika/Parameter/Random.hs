
-- |
-- Module     : Simulation.Aivika.Parameter.Random
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines the random parameters of simulation experiments.
--
-- To create a parameter that would return the same value within the simulation run,
-- you should memoize the computation with help of 'memoParameter', which is important
-- for the Monte-Carlo simulation.
--
-- To create a random function that would return the same values in the integration
-- time points within the simulation run, you should either lift the computation to
-- the 'Dynamics' computation and then memoize it too but using the 'memo0Dynamics'
-- function for that computation, or just take the predefined function that does
-- namely this.

module Simulation.Aivika.Parameter.Random
       (randomUniform,
        randomUniformInt,
        randomNormal,
        randomExponential,
        randomErlang,
        randomPoisson,
        randomBinomial,
        randomTrue,
        randomFalse) where

import System.Random

import Control.Monad.Trans

import Simulation.Aivika.Generator
import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Memo.Unboxed

-- | Computation that generates a new random number distributed uniformly.
randomUniform :: Double     -- ^ minimum
                 -> Double  -- ^ maximum
                 -> Parameter Double
randomUniform min max =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorUniform g min max

-- | Computation that generates a new random integer number distributed uniformly.
randomUniformInt :: Int     -- ^ minimum
                    -> Int  -- ^ maximum
                    -> Parameter Int
randomUniformInt min max =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorUniformInt g min max

-- | Computation that generates a new random number distributed normally.
randomNormal :: Double     -- ^ mean
                -> Double  -- ^ deviation
                -> Parameter Double
randomNormal mu nu =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorNormal g mu nu

-- | Computation that returns a new exponential random number with the specified mean
-- (the reciprocal of the rate).
randomExponential :: Double
                     -- ^ the mean (the reciprocal of the rate)
                     -> Parameter Double
randomExponential mu =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorExponential g mu

-- | Computation that returns a new Erlang random number with the specified scale
-- (the reciprocal of the rate) and integer shape.
randomErlang :: Double
                -- ^ the scale (the reciprocal of the rate)
                -> Int
                -- ^ the shape
                -> Parameter Double
randomErlang beta m =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorErlang g beta m

-- | Computation that returns a new Poisson random number with the specified mean.
randomPoisson :: Double
                 -- ^ the mean
                 -> Parameter Int
randomPoisson mu =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorPoisson g mu

-- | Computation that returns a new binomial random number with the specified
-- probability and trials.
randomBinomial :: Double  -- ^ the probability
                  -> Int  -- ^ the number of trials
                  -> Parameter Int
randomBinomial prob trials =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorBinomial g prob trials

-- | Computation that returns 'True' in case of success.
randomTrue :: Double      -- ^ the probability of the success
              -> Parameter Bool
randomTrue p =
  do x <- randomUniform 0 1
     return (x <= p)

-- | Computation that returns 'False' in case of success.
randomFalse :: Double      -- ^ the probability of the success
              -> Parameter Bool
randomFalse p =
  do x <- randomUniform 0 1
     return (x > p)     
