
-- |
-- Module     : Simulation.Aivika.Dynamics.Random
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
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

module Simulation.Aivika.Dynamics.Random
       (memoRandomUniformDynamics,
        memoRandomUniformIntDynamics,
        memoRandomTriangularDynamics,
        memoRandomNormalDynamics,
        memoRandomLogNormalDynamics,
        memoRandomExponentialDynamics,
        memoRandomErlangDynamics,
        memoRandomPoissonDynamics,
        memoRandomBinomialDynamics,
        memoRandomGammaDynamics,
        memoRandomBetaDynamics,
        memoRandomWeibullDynamics,
        memoRandomDiscreteDynamics) where

import System.Random

import Control.Monad.Trans

import Simulation.Aivika.Generator
import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Dynamics.Memo.Unboxed
import Simulation.Aivika.Unboxed

-- | Computation that generates random numbers distributed uniformly and
-- memoizes the numbers in the integration time points.
memoRandomUniformDynamics :: Dynamics Double     -- ^ minimum
                             -> Dynamics Double  -- ^ maximum
                             -> Simulation (Dynamics Double)
memoRandomUniformDynamics min max =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     min' <- invokeDynamics p min
     max' <- invokeDynamics p max
     generateUniform g min' max'

-- | Computation that generates random integer numbers distributed uniformly and
-- memoizes the numbers in the integration time points.
memoRandomUniformIntDynamics :: Dynamics Int     -- ^ minimum
                                -> Dynamics Int  -- ^ maximum
                                -> Simulation (Dynamics Int)
memoRandomUniformIntDynamics min max =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     min' <- invokeDynamics p min
     max' <- invokeDynamics p max
     generateUniformInt g min' max'

-- | Computation that generates random numbers from the triangular distribution
-- and memoizes the numbers in the integration time points.
memoRandomTriangularDynamics :: Dynamics Double     -- ^ minimum
                                -> Dynamics Double  -- ^ median
                                -> Dynamics Double  -- ^ maximum
                                -> Simulation (Dynamics Double)
memoRandomTriangularDynamics min median max =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     min' <- invokeDynamics p min
     median' <- invokeDynamics p median
     max' <- invokeDynamics p max
     generateTriangular g min' median' max'

-- | Computation that generates random numbers distributed normally and
-- memoizes the numbers in the integration time points.
memoRandomNormalDynamics :: Dynamics Double     -- ^ mean
                            -> Dynamics Double  -- ^ deviation
                            -> Simulation (Dynamics Double)
memoRandomNormalDynamics mu nu =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     mu' <- invokeDynamics p mu
     nu' <- invokeDynamics p nu
     generateNormal g mu' nu'

-- | Computation that generates random numbers from the lognormal distribution
-- and memoizes the numbers in the integration time points.
memoRandomLogNormalDynamics :: Dynamics Double
                               -- ^ the mean of a normal distribution which
                               -- this distribution is derived from
                               -> Dynamics Double
                               -- ^ the deviation of a normal distribution which
                               -- this distribution is derived from
                               -> Simulation (Dynamics Double)
memoRandomLogNormalDynamics mu nu =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     mu' <- invokeDynamics p mu
     nu' <- invokeDynamics p nu
     generateLogNormal g mu' nu'

-- | Computation that generates exponential random numbers with the specified mean
-- (the reciprocal of the rate) and memoizes the numbers in the integration time points.
memoRandomExponentialDynamics :: Dynamics Double
                                 -- ^ the mean (a reciprocal of the rate)
                                 -> Simulation (Dynamics Double)
memoRandomExponentialDynamics mu =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     mu' <- invokeDynamics p mu
     generateExponential g mu'

-- | Computation that generates the Erlang random numbers with the specified scale
-- (the reciprocal of the rate) and integer shape but memoizes the numbers in
-- the integration time points.
memoRandomErlangDynamics :: Dynamics Double
                            -- ^ the scale (a reciprocal of the rate)
                            -> Dynamics Int
                            -- ^ the shape
                            -> Simulation (Dynamics Double)
memoRandomErlangDynamics beta m =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     beta' <- invokeDynamics p beta
     m' <- invokeDynamics p m
     generateErlang g beta' m'

-- | Computation that generats the Poisson random numbers with the specified mean
-- and memoizes the numbers in the integration time points.
memoRandomPoissonDynamics :: Dynamics Double
                             -- ^ the mean
                             -> Simulation (Dynamics Int)
memoRandomPoissonDynamics mu =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     mu' <- invokeDynamics p mu
     generatePoisson g mu'

-- | Computation that generates binomial random numbers with the specified
-- probability and trials but memoizes the numbers in the integration time points.
memoRandomBinomialDynamics :: Dynamics Double  -- ^ the probability
                              -> Dynamics Int  -- ^ the number of trials
                              -> Simulation (Dynamics Int)
memoRandomBinomialDynamics prob trials =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     prob' <- invokeDynamics p prob
     trials' <- invokeDynamics p trials
     generateBinomial g prob' trials'

-- | Computation that generates random numbers from the Gamma distribution
-- with the specified shape and scale but memoizes the numbers in
-- the integration time points.
memoRandomGammaDynamics :: Dynamics Double     -- ^ shape
                           -> Dynamics Double  -- ^ scale (a reciprocal of the rate)
                           -> Simulation (Dynamics Double)
memoRandomGammaDynamics kappa theta =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     kappa' <- invokeDynamics p kappa
     theta' <- invokeDynamics p theta
     generateGamma g kappa' theta'

-- | Computation that generates random numbers from the Beta distribution
-- by the specified shape parameters and memoizes the numbers in
-- the integration time points.
memoRandomBetaDynamics :: Dynamics Double     -- ^ shape (alpha)
                          -> Dynamics Double  -- ^ shape (beta)
                          -> Simulation (Dynamics Double)
memoRandomBetaDynamics alpha beta =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     alpha' <- invokeDynamics p alpha
     beta'  <- invokeDynamics p beta
     generateBeta g alpha' beta'

-- | Computation that generates random numbers from the Weibull distribution
-- with the specified shape and scale but memoizes the numbers in
-- the integration time points.
memoRandomWeibullDynamics :: Dynamics Double     -- ^ shape
                             -> Dynamics Double  -- ^ scale
                             -> Simulation (Dynamics Double)
memoRandomWeibullDynamics alpha beta =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     alpha' <- invokeDynamics p alpha
     beta'  <- invokeDynamics p beta
     generateWeibull g alpha' beta'

-- | Computation that generates random values from the specified discrete
-- distribution and memoizes the values in the integration time points.
memoRandomDiscreteDynamics :: Unboxed a => Dynamics (DiscretePDF a) -> Simulation (Dynamics a)
memoRandomDiscreteDynamics dpdf =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     dpdf' <- invokeDynamics p dpdf
     generateDiscrete g dpdf'
