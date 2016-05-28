
-- |
-- Module     : Simulation.Aivika.Simulation
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- The module defines the 'Simulation' monad that represents a computation within
-- the simulation run.
-- 
module Simulation.Aivika.Simulation
       (-- * Simulation
        Simulation,
        SimulationLift(..),
        runSimulation,
        runSimulations,
        runSimulationByIndex,
        -- * Error Handling
        catchSimulation,
        finallySimulation,
        throwSimulation,
        -- * Memoization
        memoSimulation,
        -- * Exceptions
        SimulationException(..),
        SimulationAbort(..),
        SimulationRetry(..)) where

import Simulation.Aivika.Internal.Simulation
