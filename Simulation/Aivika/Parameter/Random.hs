
-- |
-- Module     : Simulation.Aivika.Parameter.Random
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines the random parameters of simulation experiments.
--

module Simulation.Aivika.Parameter.Random
       (newRandomParameter,
        newNormalParameter) where

import System.Random

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
  do x <- newParameter $ getStdRandom random
     return $ min + x * (max - min)

-- | Create a new random parameter distributed normally.
-- The value doesn't change within the simulation run but
-- then the value is recalculated for each new run.
newNormalParameter :: Simulation Double     -- ^ mean
                      -> Simulation Double  -- ^ variance
                      -> IO (Simulation Double)
newNormalParameter mu nu =
  do x <- newNormalGen >>= newParameter
     return $ mu + x * nu
