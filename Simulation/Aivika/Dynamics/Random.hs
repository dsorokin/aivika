
-- |
-- Module     : Simulation.Aivika.Dynamics.Random
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines the random parameters of simulation experiments.
--

module Simulation.Aivika.Dynamics.Random
       (memoRandomUniformDynamics,
        memoRandomNormalDynamics,
        memoRandomExponentialDynamics,
        memoRandomErlangDynamics,
        memoRandomPoissonDynamics,
        memoRandomBinomialDynamics) where

import System.Random

import Control.Monad.Trans

import Simulation.Aivika.Generator
import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Dynamics.Memo.Unboxed

-- | Computation that generates random numbers distributed uniformly and
-- memoizes them in the integration time points.
memoRandomUniformDynamics :: Dynamics Double     -- ^ minimum
                             -> Dynamics Double  -- ^ maximum
                             -> Simulation (Dynamics Double)
memoRandomUniformDynamics min max =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     min' <- invokeDynamics p min
     max' <- invokeDynamics p max
     generatorUniform g min' max'

-- | Computation that generates random numbers distributed normally and
-- memoizes them in the integration time points.
memoRandomNormalDynamics :: Dynamics Double     -- ^ mean
                            -> Dynamics Double  -- ^ deviation
                            -> Simulation (Dynamics Double)
memoRandomNormalDynamics mu nu =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     mu' <- invokeDynamics p mu
     nu' <- invokeDynamics p nu
     generatorNormal g mu' nu'

-- | Computation that generates exponential random numbers with the specified mean
-- (the reciprocal of the rate) and memoizes them in the integration time points.
memoRandomExponentialDynamics :: Dynamics Double
                                 -- ^ the mean (the reciprocal of the rate)
                                 -> Simulation (Dynamics Double)
memoRandomExponentialDynamics mu =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     mu' <- invokeDynamics p mu
     generatorExponential g mu'

-- | Computation that generates the Erlang random numbers with the specified scale
-- (the reciprocal of the rate) and integer shape but memoizes them in the integration
-- time points.
memoRandomErlangDynamics :: Dynamics Double
                            -- ^ the scale (the reciprocal of the rate)
                            -> Dynamics Int
                            -- ^ the shape
                            -> Simulation (Dynamics Double)
memoRandomErlangDynamics beta m =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     beta' <- invokeDynamics p beta
     m' <- invokeDynamics p m
     generatorErlang g beta' m'

-- | Computation that generats the Poisson random numbers with the specified mean
-- and memoizes them in the integration time points.
memoRandomPoissonDynamics :: Dynamics Double
                             -- ^ the mean
                             -> Simulation (Dynamics Int)
memoRandomPoissonDynamics mu =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     mu' <- invokeDynamics p mu
     generatorPoisson g mu'

-- | Computation that generates binomial random numbers with the specified
-- probability and trials but memoizes them in the integration time points.
memoRandomBinomialDynamics :: Dynamics Double  -- ^ the probability
                              -> Dynamics Int  -- ^ the number of trials
                              -> Simulation (Dynamics Int)
memoRandomBinomialDynamics prob trials =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     prob' <- invokeDynamics p prob
     trials' <- invokeDynamics p trials
     generatorBinomial g prob' trials'
