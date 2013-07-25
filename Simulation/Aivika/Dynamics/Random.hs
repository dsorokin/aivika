
-- |
-- Module     : Simulation.Aivika.Dynamics.Random
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- Below are defined random functions that return the 'Dynamics' computations. 
-- The values are initially defined in the integration time points and then
-- they are passed in to the 'memo0Dynamics' function to memoize and then interpolate.
--

module Simulation.Aivika.Dynamics.Random 
       (newRandomDynamics, newNormalDynamics) where

import System.Random
import Data.IORef
import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Random
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Memo.Unboxed

-- | Return the uniform random numbers in the integration time points.
newRandomDynamics :: Dynamics Double     -- ^ minimum
                  -> Dynamics Double  -- ^ maximum
                  -> Simulation (Dynamics Double)
newRandomDynamics min max =
  memo0Dynamics $ do
    x <- liftIO $ getStdRandom random
    min + return x * (max - min)
     
-- | Return the normal random numbers in the integration time points.
newNormalDynamics :: Dynamics Double     -- ^ mean
                  -> Dynamics Double  -- ^ variance
                  -> Simulation (Dynamics Double)
newNormalDynamics mu nu =
  do g <- liftIO newNormalGen
     memo0Dynamics $ do
       x <- liftIO g
       mu + return x * nu
