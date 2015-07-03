
-- |
-- Module     : Simulation.Aivika.Activity.Random
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines some useful predefined activities that
-- hold the current process for the corresponding random time
-- interval, when processing every input element.
--

module Simulation.Aivika.Activity.Random
       (newRandomUniformActivity,
        newRandomUniformIntActivity,
        newRandomTriangularActivity,
        newRandomNormalActivity,
        newRandomLogNormalActivity,
        newRandomExponentialActivity,
        newRandomErlangActivity,
        newRandomPoissonActivity,
        newRandomBinomialActivity,
        newPreemptibleRandomUniformActivity,
        newPreemptibleRandomUniformIntActivity,
        newPreemptibleRandomTriangularActivity,
        newPreemptibleRandomNormalActivity,
        newPreemptibleRandomLogNormalActivity,
        newPreemptibleRandomExponentialActivity,
        newPreemptibleRandomErlangActivity,
        newPreemptibleRandomPoissonActivity,
        newPreemptibleRandomBinomialActivity) where

import Simulation.Aivika.Simulation
import Simulation.Aivika.Process
import Simulation.Aivika.Process.Random
import Simulation.Aivika.Activity

-- | Create a new activity that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomUniformActivity :: Double
                            -- ^ the minimum time interval
                            -> Double
                            -- ^ the maximum time interval
                            -> Simulation (Activity () a a)
newRandomUniformActivity =
  newPreemptibleRandomUniformActivity False

-- | Create a new activity that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomUniformIntActivity :: Int
                               -- ^ the minimum time interval
                               -> Int
                               -- ^ the maximum time interval
                               -> Simulation (Activity () a a)
newRandomUniformIntActivity =
  newPreemptibleRandomUniformIntActivity False

-- | Create a new activity that holds the process for a random time interval
-- from the triangular distribution, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomTriangularActivity :: Double
                               -- ^ the minimum time interval
                               -> Double
                               -- ^ the median of the time interval
                               -> Double
                               -- ^ the maximum time interval
                               -> Simulation (Activity () a a)
newRandomTriangularActivity =
  newPreemptibleRandomTriangularActivity False

-- | Create a new activity that holds the process for a random time interval
-- distributed normally, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomNormalActivity :: Double
                           -- ^ the mean time interval
                           -> Double
                           -- ^ the time interval deviation
                           -> Simulation (Activity () a a)
newRandomNormalActivity =
  newPreemptibleRandomNormalActivity False
         
-- | Create a new activity that holds the process for a random time interval
-- from the lognormal distribution, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomLogNormalActivity :: Double
                              -- ^ the mean of a normal distribution which
                              -- this distribution is derived from
                              -> Double
                              -- ^ the deviation of a normal distribution which
                              -- this distribution is derived from
                              -> Simulation (Activity () a a)
newRandomLogNormalActivity =
  newPreemptibleRandomLogNormalActivity False

-- | Create a new activity that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomExponentialActivity :: Double
                                -- ^ the mean time interval (the reciprocal of the rate)
                                -> Simulation (Activity () a a)
newRandomExponentialActivity =
  newPreemptibleRandomExponentialActivity False
         
-- | Create a new activity that holds the process for a random time interval
-- from the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomErlangActivity :: Double
                           -- ^ the scale (the reciprocal of the rate)
                           -> Int
                           -- ^ the shape
                           -> Simulation (Activity () a a)
newRandomErlangActivity =
  newPreemptibleRandomErlangActivity False

-- | Create a new activity that holds the process for a random time interval
-- from the Poisson distribution with the specified mean, when processing
-- every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomPoissonActivity :: Double
                            -- ^ the mean time interval
                            -> Simulation (Activity () a a)
newRandomPoissonActivity =
  newPreemptibleRandomPoissonActivity False

-- | Create a new activity that holds the process for a random time interval
-- from the binomial distribution with the specified probability and trials,
-- when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomBinomialActivity :: Double
                             -- ^ the probability
                             -> Int
                             -- ^ the number of trials
                             -> Simulation (Activity () a a)
newRandomBinomialActivity =
  newPreemptibleRandomBinomialActivity False

-- | Create a new activity that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformActivity :: Bool
                                       -- ^ whether the activity process can be preempted
                                       -> Double
                                       -- ^ the minimum time interval
                                       -> Double
                                       -- ^ the maximum time interval
                                       -> Simulation (Activity () a a)
newPreemptibleRandomUniformActivity preemptible min max =
  newPreemptibleActivity preemptible $ \a ->
  do randomUniformProcess_ min max
     return a

-- | Create a new activity that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformIntActivity :: Bool
                                          -- ^ whether the activity process can be preempted
                                          -> Int
                                          -- ^ the minimum time interval
                                          -> Int
                                          -- ^ the maximum time interval
                                          -> Simulation (Activity () a a)
newPreemptibleRandomUniformIntActivity preemptible min max =
  newPreemptibleActivity preemptible $ \a ->
  do randomUniformIntProcess_ min max
     return a

-- | Create a new activity that holds the process for a random time interval
-- from the triangular distribution, when processing every input element.
newPreemptibleRandomTriangularActivity :: Bool
                                          -- ^ whether the activity process can be preempted
                                          -> Double
                                          -- ^ the minimum time interval
                                          -> Double
                                          -- ^ the median of the time interval
                                          -> Double
                                          -- ^ the maximum time interval
                                          -> Simulation (Activity () a a)
newPreemptibleRandomTriangularActivity preemptible min median max =
  newPreemptibleActivity preemptible $ \a ->
  do randomTriangularProcess_ min median max
     return a

-- | Create a new activity that holds the process for a random time interval
-- distributed normally, when processing every input element.
newPreemptibleRandomNormalActivity :: Bool
                                      -- ^ whether the activity process can be preempted
                                      -> Double
                                      -- ^ the mean time interval
                                      -> Double
                                      -- ^ the time interval deviation
                                      -> Simulation (Activity () a a)
newPreemptibleRandomNormalActivity preemptible mu nu =
  newPreemptibleActivity preemptible $ \a ->
  do randomNormalProcess_ mu nu
     return a

-- | Create a new activity that holds the process for a random time interval
-- from the lognormal distribution, when processing every input element.
newPreemptibleRandomLogNormalActivity :: Bool
                                         -- ^ whether the activity process can be preempted
                                         -> Double
                                         -- ^ the mean of a normal distribution which
                                         -- this distribution is derived from
                                         -> Double
                                         -- ^ the deviation of a normal distribution which
                                         -- this distribution is derived from
                                         -> Simulation (Activity () a a)
newPreemptibleRandomLogNormalActivity preemptible mu nu =
  newPreemptibleActivity preemptible $ \a ->
  do randomLogNormalProcess_ mu nu
     return a

-- | Create a new activity that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
newPreemptibleRandomExponentialActivity :: Bool
                                           -- ^ whether the activity process can be preempted
                                           -> Double
                                           -- ^ the mean time interval (the reciprocal of the rate)
                                           -> Simulation (Activity () a a)
newPreemptibleRandomExponentialActivity preemptible mu =
  newPreemptibleActivity preemptible $ \a ->
  do randomExponentialProcess_ mu
     return a
         
-- | Create a new activity that holds the process for a random time interval
-- from the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
newPreemptibleRandomErlangActivity :: Bool
                                      -- ^ whether the activity process can be preempted
                                      -> Double
                                      -- ^ the scale (the reciprocal of the rate)
                                      -> Int
                                      -- ^ the shape
                                      -> Simulation (Activity () a a)
newPreemptibleRandomErlangActivity preemptible beta m =
  newPreemptibleActivity preemptible $ \a ->
  do randomErlangProcess_ beta m
     return a

-- | Create a new activity that holds the process for a random time interval
-- from the Poisson distribution with the specified mean, when processing
-- every input element.
newPreemptibleRandomPoissonActivity :: Bool
                                       -- ^ whether the activity process can be preempted
                                       -> Double
                                       -- ^ the mean time interval
                                       -> Simulation (Activity () a a)
newPreemptibleRandomPoissonActivity preemptible mu =
  newPreemptibleActivity preemptible $ \a ->
  do randomPoissonProcess_ mu
     return a

-- | Create a new activity that holds the process for a random time interval
-- from the binomial distribution with the specified probability and trials,
-- when processing every input element.
newPreemptibleRandomBinomialActivity :: Bool
                                        -- ^ whether the activity process can be preempted
                                        -> Double
                                        -- ^ the probability
                                        -> Int
                                        -- ^ the number of trials
                                        -> Simulation (Activity () a a)
newPreemptibleRandomBinomialActivity preemptible prob trials =
  newPreemptibleActivity preemptible $ \a ->
  do randomBinomialProcess_ prob trials
     return a
