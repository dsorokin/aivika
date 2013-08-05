
-- |
-- Module     : Simulation.Aivika.Dynamics.Random
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- Below are defined random functions that return the 'Dynamics' computations. 
-- The values are initially defined in the integration time points and then
-- they are passed in to the 'memo0Dynamics' function to memoize and then interpolate.
--

module Simulation.Aivika.Dynamics.Random 
       (newUniformDynamics,
        newNormalDynamics,
        newExponentialDynamics,
        newPoissonDynamics,
        newBinomialDynamics) where

import System.Random
import Data.IORef
import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Random
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Memo.Unboxed

-- | Return the uniform random numbers in the integration time points.
newUniformDynamics :: Dynamics Double     -- ^ minimum
                   -> Dynamics Double     -- ^ maximum
                   -> Simulation (Dynamics Double)
newUniformDynamics min max =
  memo0Dynamics $ do
    x <- liftIO $ getStdRandom random
    min + return x * (max - min)
     
-- | Return the normal random numbers in the integration time points.
newNormalDynamics :: Dynamics Double     -- ^ mean
                  -> Dynamics Double     -- ^ variance
                  -> Simulation (Dynamics Double)
newNormalDynamics mu nu =
  do g <- liftIO newNormalGen
     memo0Dynamics $ do
       x <- liftIO g
       mu + return x * nu

-- | Return the exponential random numbers in the integration time points
-- with the specified mean.
newExponentialDynamics :: Dynamics Double -> Simulation (Dynamics Double)
newExponentialDynamics mu =
  memo0Dynamics $ do
    x <- mu
    liftIO $ exponentialGen x

-- | Return the Poisson random numbers in the integration time points
-- with the specified mean.
newPoissonDynamics :: Dynamics Double -> Simulation (Dynamics Int)
newPoissonDynamics mu =
  memo0Dynamics $ do
    x <- mu
    liftIO $ poissonGen x

-- | Return in the integration time points the binomial random numbers
-- with the specified probability and trials.
newBinomialDynamics :: Dynamics Double  -- ^ the probability
                       -> Dynamics Int  -- ^ the number of trials
                       -> Simulation (Dynamics Int)
newBinomialDynamics prob trials =
  memo0Dynamics $ do
    x <- prob
    y <- trials
    liftIO $ binomialGen x y
