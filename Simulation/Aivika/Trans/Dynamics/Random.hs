
-- |
-- Module     : Simulation.Aivika.Trans.Dynamics.Random
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the random functions that always return the same values
-- in the integration time points within a single simulation run. The values
-- for another simulation run will be regenerated anew.
--
-- For example, the computations returned by these functions can be used in
-- the equations of System Dynamics.
--
-- Also it is worth noting that the values are generated in a strong order starting
-- from 'starttime' with step 'dt'. This is how the 'memo0Dynamics' function
-- actually works.
--

module Simulation.Aivika.Trans.Dynamics.Random
       (memoRandomUniformDynamics,
        memoRandomUniformIntDynamics,
        memoRandomNormalDynamics,
        memoRandomExponentialDynamics,
        memoRandomErlangDynamics,
        memoRandomPoissonDynamics,
        memoRandomBinomialDynamics) where

import System.Random

import Control.Monad.Trans

import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Dynamics.Memo.Unboxed

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

-- | Computation that generates random integer numbers distributed uniformly and
-- memoizes them in the integration time points.
memoRandomUniformIntDynamics :: Dynamics Int     -- ^ minimum
                                -> Dynamics Int  -- ^ maximum
                                -> Simulation (Dynamics Int)
memoRandomUniformIntDynamics min max =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     min' <- invokeDynamics p min
     max' <- invokeDynamics p max
     generatorUniformInt g min' max'

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
