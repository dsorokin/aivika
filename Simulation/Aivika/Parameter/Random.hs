
-- |
-- Module     : Simulation.Aivika.Parameter.Random
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines the random parameters of simulation experiments.
--

module Simulation.Aivika.Parameter.Random
       (newRandomParameter,
        newNormalParameter,
        newExponentialParameter,
        newPoissonParameter,
        newBinomialParameter) where

import System.Random

import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Random
import Simulation.Aivika.Parameter

-- | Create a new random parameter distributed uniformly.
-- The value doesn't change within the simulation run but
-- then the value is recalculated for each new run.
newRandomParameter :: Simulation Double     -- ^ minimum
                      -> Simulation Double  -- ^ maximum
                      -> IO (Simulation Double)
newRandomParameter min max =
  memoSimulation $
  do x <- liftIO $ getStdRandom random
     min + return x * (max - min)

-- | Create a new random parameter distributed normally.
-- The value doesn't change within the simulation run but
-- then the value is recalculated for each new run.
newNormalParameter :: Simulation Double     -- ^ mean
                      -> Simulation Double  -- ^ variance
                      -> IO (Simulation Double)
newNormalParameter mu nu =
  do g <- newNormalGen
     memoSimulation $
       do x <- liftIO g
          mu + return x * nu

-- | Return the exponential random parameter with the specified mean.
newExponentialParameter :: Simulation Double -> IO (Simulation Double)
newExponentialParameter mu =
  memoSimulation $
  do x <- mu
     liftIO $ exponentialGen x

-- | Return the Poisson random parameter with the specified mean.
newPoissonParameter :: Simulation Double -> IO (Simulation Int)
newPoissonParameter mu =
  memoSimulation $
  do x <- mu
     liftIO $ poissonGen x

-- | Return the binomial random parameter with the specified probability and trials.
newBinomialParameter :: Simulation Double -> Simulation Int -> IO (Simulation Int)
newBinomialParameter prob trials =
  memoSimulation $
  do x <- prob
     y <- trials
     liftIO $ binomialGen x y