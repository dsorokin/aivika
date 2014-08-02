
-- |
-- Module     : Simulation.Aivika.Simulation
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'Simulation' monad that represents a simulation run.
-- 
module Simulation.Aivika.Simulation
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

import Simulation.Aivika.Internal.Simulation
