
-- |
-- Module     : Simulation.Aivika.Trans.Simulation
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'SimulationT' monad transformer that represents a simulation run.
-- 
module Simulation.Aivika.Trans.Simulation
       (-- * Simulation
        Simulation,
        SimulationLift(..),
        runSimulation,
        runSimulations,
        -- * Error Handling
        catchSimulation,
        finallySimulation,
        throwSimulation,
        -- * Memoization
        memoSimulation) where

import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
