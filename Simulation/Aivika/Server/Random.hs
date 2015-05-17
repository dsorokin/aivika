
-- |
-- Module     : Simulation.Aivika.Server.Random
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines some useful predefined servers that
-- hold the current process for the corresponding random time
-- interval, when processing every input element.
--

module Simulation.Aivika.Server.Random
       (newRandomUniformServer,
        newRandomUniformIntServer,
        newRandomNormalServer,
        newRandomExponentialServer,
        newRandomErlangServer,
        newRandomPoissonServer,
        newRandomBinomialServer,
        newPreemptibleRandomUniformServer,
        newPreemptibleRandomUniformIntServer,
        newPreemptibleRandomNormalServer,
        newPreemptibleRandomExponentialServer,
        newPreemptibleRandomErlangServer,
        newPreemptibleRandomPoissonServer,
        newPreemptibleRandomBinomialServer) where

import Simulation.Aivika.Simulation
import Simulation.Aivika.Process
import Simulation.Aivika.Process.Random
import Simulation.Aivika.Server

-- | Create a new server that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomUniformServer :: Double
                          -- ^ the minimum time interval
                          -> Double
                          -- ^ the maximum time interval
                          -> Simulation (Server () a a)
newRandomUniformServer =
  newPreemptibleRandomUniformServer False

-- | Create a new server that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomUniformIntServer :: Int
                             -- ^ the minimum time interval
                             -> Int
                             -- ^ the maximum time interval
                             -> Simulation (Server () a a)
newRandomUniformIntServer =
  newPreemptibleRandomUniformIntServer False

-- | Create a new server that holds the process for a random time interval
-- distributed normally, when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomNormalServer :: Double
                         -- ^ the mean time interval
                         -> Double
                         -- ^ the time interval deviation
                         -> Simulation (Server () a a)
newRandomNormalServer =
  newPreemptibleRandomNormalServer False
         
-- | Create a new server that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomExponentialServer :: Double
                              -- ^ the mean time interval (the reciprocal of the rate)
                              -> Simulation (Server () a a)
newRandomExponentialServer =
  newPreemptibleRandomExponentialServer False
         
-- | Create a new server that holds the process for a random time interval
-- having the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomErlangServer :: Double
                         -- ^ the scale (the reciprocal of the rate)
                         -> Int
                         -- ^ the shape
                         -> Simulation (Server () a a)
newRandomErlangServer =
  newPreemptibleRandomErlangServer False

-- | Create a new server that holds the process for a random time interval
-- having the Poisson distribution with the specified mean, when processing
-- every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomPoissonServer :: Double
                          -- ^ the mean time interval
                          -> Simulation (Server () a a)
newRandomPoissonServer =
  newPreemptibleRandomPoissonServer False

-- | Create a new server that holds the process for a random time interval
-- having the binomial distribution with the specified probability and trials,
-- when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomBinomialServer :: Double
                           -- ^ the probability
                           -> Int
                           -- ^ the number of trials
                           -> Simulation (Server () a a)
newRandomBinomialServer =
  newPreemptibleRandomBinomialServer False

-- | Create a new server that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformServer :: Bool
                                     -- ^ whether the server process can be preempted
                                     -> Double
                                     -- ^ the minimum time interval
                                     -> Double
                                     -- ^ the maximum time interval
                                     -> Simulation (Server () a a)
newPreemptibleRandomUniformServer preemptible min max =
  newPreemptibleServer preemptible $ \a ->
  do randomUniformProcess_ min max
     return a

-- | Create a new server that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformIntServer :: Bool
                                        -- ^ whether the server process can be preempted
                                        -> Int
                                        -- ^ the minimum time interval
                                        -> Int
                                        -- ^ the maximum time interval
                                        -> Simulation (Server () a a)
newPreemptibleRandomUniformIntServer preemptible min max =
  newPreemptibleServer preemptible $ \a ->
  do randomUniformIntProcess_ min max
     return a

-- | Create a new server that holds the process for a random time interval
-- distributed normally, when processing every input element.
newPreemptibleRandomNormalServer :: Bool
                                    -- ^ whether the server process can be preempted
                                    -> Double
                                    -- ^ the mean time interval
                                    -> Double
                                    -- ^ the time interval deviation
                                    -> Simulation (Server () a a)
newPreemptibleRandomNormalServer preemptible mu nu =
  newPreemptibleServer preemptible $ \a ->
  do randomNormalProcess_ mu nu
     return a
         
-- | Create a new server that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
newPreemptibleRandomExponentialServer :: Bool
                                         -- ^ whether the server process can be preempted
                                         -> Double
                                         -- ^ the mean time interval (the reciprocal of the rate)
                                         -> Simulation (Server () a a)
newPreemptibleRandomExponentialServer preemptible mu =
  newPreemptibleServer preemptible $ \a ->
  do randomExponentialProcess_ mu
     return a
         
-- | Create a new server that holds the process for a random time interval
-- having the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
newPreemptibleRandomErlangServer :: Bool
                                    -- ^ whether the server process can be preempted
                                    -> Double
                                    -- ^ the scale (the reciprocal of the rate)
                                    -> Int
                                    -- ^ the shape
                                    -> Simulation (Server () a a)
newPreemptibleRandomErlangServer preemptible beta m =
  newPreemptibleServer preemptible $ \a ->
  do randomErlangProcess_ beta m
     return a

-- | Create a new server that holds the process for a random time interval
-- having the Poisson distribution with the specified mean, when processing
-- every input element.
newPreemptibleRandomPoissonServer :: Bool
                                     -- ^ whether the server process can be preempted
                                     -> Double
                                     -- ^ the mean time interval
                                  -> Simulation (Server () a a)
newPreemptibleRandomPoissonServer preemptible mu =
  newPreemptibleServer preemptible $ \a ->
  do randomPoissonProcess_ mu
     return a

-- | Create a new server that holds the process for a random time interval
-- having the binomial distribution with the specified probability and trials,
-- when processing every input element.
newPreemptibleRandomBinomialServer :: Bool
                                     -- ^ whether the server process can be preempted
                                     -> Double
                                     -- ^ the probability
                                     -> Int
                                     -- ^ the number of trials
                                     -> Simulation (Server () a a)
newPreemptibleRandomBinomialServer preemptible prob trials =
  newPreemptibleServer preemptible $ \a ->
  do randomBinomialProcess_ prob trials
     return a
