
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

import Simulation.Aivika.Random
import Simulation.Aivika.Parameter

-- | Create a new random parameter distributed uniformly.
-- The value doesn't change within the simulation run but
-- then the value is recalculated for each new run.
newRandomParameter :: Parameter Double     -- ^ minimum
                      -> Parameter Double  -- ^ maximum
                      -> IO (Parameter Double)
newRandomParameter min max =
  memoParameter $
  do x <- liftIO $ getStdRandom random
     min + return x * (max - min)

-- | Create a new random parameter distributed normally.
-- The value doesn't change within the simulation run but
-- then the value is recalculated for each new run.
newNormalParameter :: Parameter Double     -- ^ mean
                      -> Parameter Double  -- ^ variance
                      -> IO (Parameter Double)
newNormalParameter mu nu =
  do g <- newNormalGen
     memoParameter $
       do x <- liftIO g
          mu + return x * nu

-- | Return the exponential random parameter with the specified mean.
newExponentialParameter :: Parameter Double -> IO (Parameter Double)
newExponentialParameter mu =
  memoParameter $
  do x <- mu
     liftIO $ exponentialGen x

-- | Return the Poisson random parameter with the specified mean.
newPoissonParameter :: Parameter Double -> IO (Parameter Int)
newPoissonParameter mu =
  memoParameter $
  do x <- mu
     liftIO $ poissonGen x

-- | Return the binomial random parameter with the specified probability and trials.
newBinomialParameter :: Parameter Double  -- ^ the probability
                        -> Parameter Int  -- ^ the number of trials
                        -> IO (Parameter Int)
newBinomialParameter prob trials =
  memoParameter $
  do x <- prob
     y <- trials
     liftIO $ binomialGen x y