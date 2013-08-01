
-- |
-- Module     : Simulation.Aivika.Parameter
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines the parameters of simulation experiments.
--

module Simulation.Aivika.Parameter
       (newTableParameter,
        newIndexedParameter) where

import Data.Array

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation

-- | Create a thread-safe parameter that returns always the same value within the simulation run,
-- where the value is taken consequently from the specified table based on the number of the 
-- current run starting from zero. After all values from the table are used, it takes the first 
-- value of the table, then the second one and so on.
--
-- It uses the 'memoSimulation' function.
newTableParameter :: Array Int a -> IO (Simulation a)
newTableParameter t = newIndexedParameter (\i -> return $ t ! (((i - i1) `mod` n) + i1))
  where (i1, i2) = bounds t
        n = i2 - i1 + 1

-- | Create a thread-safe parameter that returns always the same value within the simulation run, 
-- where the value depends on the number of this run starting from zero.
--
-- It uses the 'memoSimulation' function.
newIndexedParameter :: (Int -> Simulation a) -> IO (Simulation a)
newIndexedParameter f =
  memoSimulation $ simulationIndex >>= f
