
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

module Simulation.Aivika.Parameter.Random
       (randomUniform,
        randomNormal,
        randomExponential,
        randomErlang,
        randomPoisson,
        randomBinomial) where

import System.Random

import Control.Monad.Trans

import Simulation.Aivika.Generator
import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter

-- | Computation that generates a new random number distributed uniformly.
--
-- To create a parameter that would return the same value within the simulation run,
-- you should memoize the computation, which is important for the Monte-Carlo simulation.
--
-- To create a random function that would return the same values in the integration
-- time points within the simulation run, you should either lift the computation to
-- the @Dynamics@ computation and then memoize it too but using the corresponded
-- function for that computation, or just take the predefined function that does
-- namely this.
randomUniform :: Double     -- ^ minimum
                 -> Double  -- ^ maximum
                 -> Parameter Double
randomUniform min max =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorUniform g min max

-- | Computation that generates a new random number distributed normally.
--
-- To create a parameter that would return the same value within the simulation run,
-- you should memoize the computation, which is important for the Monte-Carlo simulation.
--
-- To create a random function that would return the same values in the integration
-- time points within the simulation run, you should either lift the computation to
-- the @Dynamics@ computation and then memoize it too but using the corresponded
-- function for that computation, or just take the predefined function that does
-- namely this.
randomNormal :: Double     -- ^ mean
                -> Double  -- ^ deviation
                -> Parameter Double
randomNormal mu nu =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorNormal g mu nu

-- | Computation that returns a new exponential random number with the specified mean
-- (the reciprocal of the rate).
--
-- To create a parameter that would return the same value within the simulation run,
-- you should memoize the computation, which is important for the Monte-Carlo simulation.
--
-- To create a random function that would return the same values in the integration
-- time points within the simulation run, you should either lift the computation to
-- the @Dynamics@ computation and then memoize it too but using the corresponded
-- function for that computation, or just take the predefined function that does
-- namely this.
randomExponential :: Double
                     -- ^ the mean (the reciprocal of the rate)
                     -> Parameter Double
randomExponential mu =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorExponential g mu

-- | Computation that returns a new Erlang random number with the specified scale
-- (the reciprocal of the rate) and integer shape.
--
-- To create a parameter that would return the same value within the simulation run,
-- you should memoize the computation, which is important for the Monte-Carlo simulation.
--
-- To create a random function that would return the same values in the integration
-- time points within the simulation run, you should either lift the computation to
-- the @Dynamics@ computation and then memoize it too but using the corresponded
-- function for that computation, or just take the predefined function that does
-- namely this.
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
--
-- To create a parameter that would return the same value within the simulation run,
-- you should memoize the computation, which is important for the Monte-Carlo simulation.
--
-- To create a random function that would return the same values in the integration
-- time points within the simulation run, you should either lift the computation to
-- the @Dynamics@ computation and then memoize it too but using the corresponded
-- function for that computation, or just take the predefined function that does
-- namely this.
randomPoisson :: Double
                 -- ^ the mean
                 -> Parameter Int
randomPoisson mu =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorPoisson g mu

-- | Computation that returns a new binomial random number with the specified
-- probability and trials.
--
-- To create a parameter that would return the same value within the simulation run,
-- you should memoize the computation, which is important for the Monte-Carlo simulation.
--
-- To create a random function that would return the same values in the integration
-- time points within the simulation run, you should either lift the computation to
-- the @Dynamics@ computation and then memoize it too but using the corresponded
-- function for that computation, or just take the predefined function that does
-- namely this.
randomBinomial :: Double  -- ^ the probability
                  -> Int  -- ^ the number of trials
                  -> Parameter Int
randomBinomial prob trials =
  Parameter $ \r ->
  let g = runGenerator r
  in generatorBinomial g prob trials
