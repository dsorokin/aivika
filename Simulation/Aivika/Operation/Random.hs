
-- |
-- Module     : Simulation.Aivika.Operation.Random
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines some useful predefined operations that
-- hold the current process for the corresponding random time
-- interval, when processing every input element.
--

module Simulation.Aivika.Operation.Random
       (newRandomUniformOperation,
        newRandomUniformIntOperation,
        newRandomTriangularOperation,
        newRandomNormalOperation,
        newRandomLogNormalOperation,
        newRandomExponentialOperation,
        newRandomErlangOperation,
        newRandomPoissonOperation,
        newRandomBinomialOperation,
        newRandomGammaOperation,
        newRandomBetaOperation,
        newRandomWeibullOperation,
        newRandomDiscreteOperation,
        newPreemptibleRandomUniformOperation,
        newPreemptibleRandomUniformIntOperation,
        newPreemptibleRandomTriangularOperation,
        newPreemptibleRandomNormalOperation,
        newPreemptibleRandomLogNormalOperation,
        newPreemptibleRandomExponentialOperation,
        newPreemptibleRandomErlangOperation,
        newPreemptibleRandomPoissonOperation,
        newPreemptibleRandomBinomialOperation,
        newPreemptibleRandomGammaOperation,
        newPreemptibleRandomBetaOperation,
        newPreemptibleRandomWeibullOperation,
        newPreemptibleRandomDiscreteOperation) where

import Simulation.Aivika.Generator
import Simulation.Aivika.Event
import Simulation.Aivika.Process
import Simulation.Aivika.Process.Random
import Simulation.Aivika.Operation

-- | Create a new operation that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomUniformOperation :: Double
                             -- ^ the minimum time interval
                             -> Double
                             -- ^ the maximum time interval
                             -> Event (Operation a a)
newRandomUniformOperation =
  newPreemptibleRandomUniformOperation False

-- | Create a new operation that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomUniformIntOperation :: Int
                                -- ^ the minimum time interval
                                -> Int
                                -- ^ the maximum time interval
                                -> Event (Operation a a)
newRandomUniformIntOperation =
  newPreemptibleRandomUniformIntOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the triangular distribution, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomTriangularOperation :: Double
                                -- ^ the minimum time interval
                                -> Double
                                -- ^ the median of the time interval
                                -> Double
                                -- ^ the maximum time interval
                                -> Event (Operation a a)
newRandomTriangularOperation =
  newPreemptibleRandomTriangularOperation False

-- | Create a new operation that holds the process for a random time interval
-- distributed normally, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomNormalOperation :: Double
                            -- ^ the mean time interval
                            -> Double
                            -- ^ the time interval deviation
                            -> Event (Operation a a)
newRandomNormalOperation =
  newPreemptibleRandomNormalOperation False
         
-- | Create a new operation that holds the process for a random time interval
-- having the lognormal distribution, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomLogNormalOperation :: Double
                               -- ^ the mean of a normal distribution which
                               -- this distribution is derived from
                               -> Double
                               -- ^ the deviation of a normal distribution which
                               -- this distribution is derived from
                               -> Event (Operation a a)
newRandomLogNormalOperation =
  newPreemptibleRandomLogNormalOperation False

-- | Create a new operation that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomExponentialOperation :: Double
                                 -- ^ the mean time interval (the reciprocal of the rate)
                                 -> Event (Operation a a)
newRandomExponentialOperation =
  newPreemptibleRandomExponentialOperation False
         
-- | Create a new operation that holds the process for a random time interval
-- having the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomErlangOperation :: Double
                            -- ^ the scale (the reciprocal of the rate)
                            -> Int
                            -- ^ the shape
                            -> Event (Operation a a)
newRandomErlangOperation =
  newPreemptibleRandomErlangOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the Poisson distribution with the specified mean, when processing
-- every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomPoissonOperation :: Double
                             -- ^ the mean time interval
                             -> Event (Operation a a)
newRandomPoissonOperation =
  newPreemptibleRandomPoissonOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the binomial distribution with the specified probability and trials,
-- when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomBinomialOperation :: Double
                              -- ^ the probability
                              -> Int
                              -- ^ the number of trials
                              -> Event (Operation a a)
newRandomBinomialOperation =
  newPreemptibleRandomBinomialOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the Gamma distribution with the specified shape and scale,
-- when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomGammaOperation :: Double
                           -- ^ the shape
                           -> Double
                           -- ^ the scale (a reciprocal of the rate)
                           -> Event (Operation a a)
newRandomGammaOperation =
  newPreemptibleRandomGammaOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the Beta distribution with the specified shape parameters (alpha and beta),
-- when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomBetaOperation :: Double
                          -- ^ shape (alpha)
                          -> Double
                          -- ^ shape (beta)
                          -> Event (Operation a a)
newRandomBetaOperation =
  newPreemptibleRandomBetaOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the Weibull distribution with the specified shape and scale,
-- when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomWeibullOperation :: Double
                             -- ^ shape
                             -> Double
                             -- ^ scale
                             -> Event (Operation a a)
newRandomWeibullOperation =
  newPreemptibleRandomWeibullOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the specified discrete distribution, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomDiscreteOperation :: DiscretePDF Double
                              -- ^ the discrete probability density function
                              -> Event (Operation a a)
newRandomDiscreteOperation =
  newPreemptibleRandomDiscreteOperation False

-- | Create a new operation that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformOperation :: Bool
                                        -- ^ whether the operation process can be preempted
                                        -> Double
                                        -- ^ the minimum time interval
                                        -> Double
                                        -- ^ the maximum time interval
                                        -> Event (Operation a a)
newPreemptibleRandomUniformOperation preemptible min max =
  newPreemptibleOperation preemptible $ \a ->
  do randomUniformProcess_ min max
     return a

-- | Create a new operation that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformIntOperation :: Bool
                                           -- ^ whether the operation process can be preempted
                                           -> Int
                                           -- ^ the minimum time interval
                                           -> Int
                                           -- ^ the maximum time interval
                                           -> Event (Operation a a)
newPreemptibleRandomUniformIntOperation preemptible min max =
  newPreemptibleOperation preemptible $ \a ->
  do randomUniformIntProcess_ min max
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the triangular distribution, when processing every input element.
newPreemptibleRandomTriangularOperation :: Bool
                                           -- ^ whether the operation process can be preempted
                                           -> Double
                                           -- ^ the minimum time interval
                                           -> Double
                                           -- ^ the median of the time interval
                                           -> Double
                                           -- ^ the maximum time interval
                                           -> Event (Operation a a)
newPreemptibleRandomTriangularOperation preemptible min median max =
  newPreemptibleOperation preemptible $ \a ->
  do randomTriangularProcess_ min median max
     return a

-- | Create a new operation that holds the process for a random time interval
-- distributed normally, when processing every input element.
newPreemptibleRandomNormalOperation :: Bool
                                       -- ^ whether the operation process can be preempted
                                       -> Double
                                       -- ^ the mean time interval
                                       -> Double
                                       -- ^ the time interval deviation
                                       -> Event (Operation a a)
newPreemptibleRandomNormalOperation preemptible mu nu =
  newPreemptibleOperation preemptible $ \a ->
  do randomNormalProcess_ mu nu
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the lognormal distribution, when processing every input element.
newPreemptibleRandomLogNormalOperation :: Bool
                                          -- ^ whether the operation process can be preempted
                                          -> Double
                                          -- ^ the mean of a normal distribution which
                                          -- this distribution is derived from
                                          -> Double
                                          -- ^ the deviation of a normal distribution which
                                          -- this distribution is derived from
                                          -> Event (Operation a a)
newPreemptibleRandomLogNormalOperation preemptible mu nu =
  newPreemptibleOperation preemptible $ \a ->
  do randomLogNormalProcess_ mu nu
     return a

-- | Create a new operation that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
newPreemptibleRandomExponentialOperation :: Bool
                                            -- ^ whether the operation process can be preempted
                                            -> Double
                                            -- ^ the mean time interval (the reciprocal of the rate)
                                            -> Event (Operation a a)
newPreemptibleRandomExponentialOperation preemptible mu =
  newPreemptibleOperation preemptible $ \a ->
  do randomExponentialProcess_ mu
     return a
         
-- | Create a new operation that holds the process for a random time interval
-- having the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
newPreemptibleRandomErlangOperation :: Bool
                                       -- ^ whether the operation process can be preempted
                                       -> Double
                                       -- ^ the scale (the reciprocal of the rate)
                                       -> Int
                                       -- ^ the shape
                                       -> Event (Operation a a)
newPreemptibleRandomErlangOperation preemptible beta m =
  newPreemptibleOperation preemptible $ \a ->
  do randomErlangProcess_ beta m
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the Poisson distribution with the specified mean, when processing
-- every input element.
newPreemptibleRandomPoissonOperation :: Bool
                                        -- ^ whether the operation process can be preempted
                                        -> Double
                                        -- ^ the mean time interval
                                        -> Event (Operation a a)
newPreemptibleRandomPoissonOperation preemptible mu =
  newPreemptibleOperation preemptible $ \a ->
  do randomPoissonProcess_ mu
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the binomial distribution with the specified probability and trials,
-- when processing every input element.
newPreemptibleRandomBinomialOperation :: Bool
                                         -- ^ whether the operation process can be preempted
                                         -> Double
                                         -- ^ the probability
                                         -> Int
                                         -- ^ the number of trials
                                         -> Event (Operation a a)
newPreemptibleRandomBinomialOperation preemptible prob trials =
  newPreemptibleOperation preemptible $ \a ->
  do randomBinomialProcess_ prob trials
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the Gamma distribution with the specified shape and scale,
-- when processing every input element.
newPreemptibleRandomGammaOperation :: Bool
                                      -- ^ whether the operation process can be preempted
                                      -> Double
                                      -- ^ the shape
                                      -> Double
                                      -- ^ the scale
                                      -> Event (Operation a a)
newPreemptibleRandomGammaOperation preemptible kappa theta =
  newPreemptibleOperation preemptible $ \a ->
  do randomGammaProcess_ kappa theta
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the Beta distribution with the specified shape parameters (alpha and beta),
-- when processing every input element.
newPreemptibleRandomBetaOperation :: Bool
                                     -- ^ whether the operation process can be preempted
                                     -> Double
                                     -- ^ shape (alpha)
                                     -> Double
                                     -- ^ shape (beta)
                                     -> Event (Operation a a)
newPreemptibleRandomBetaOperation preemptible alpha beta =
  newPreemptibleOperation preemptible $ \a ->
  do randomBetaProcess_ alpha beta
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the Weibull distribution with the specified shape and scale,
-- when processing every input element.
newPreemptibleRandomWeibullOperation :: Bool
                                        -- ^ whether the operation process can be preempted
                                        -> Double
                                        -- ^ shape
                                        -> Double
                                        -- ^ scale
                                        -> Event (Operation a a)
newPreemptibleRandomWeibullOperation preemptible alpha beta =
  newPreemptibleOperation preemptible $ \a ->
  do randomWeibullProcess_ alpha beta
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the specified discrete distribution, when processing every input element.
newPreemptibleRandomDiscreteOperation :: Bool
                                         -- ^ whether the operation process can be preempted
                                         -> DiscretePDF Double
                                         -- ^ the discrete probability density function
                                         -> Event (Operation a a)
newPreemptibleRandomDiscreteOperation preemptible dpdf =
  newPreemptibleOperation preemptible $ \a ->
  do randomDiscreteProcess_ dpdf
     return a
