
-- |
-- Module     : Simulation.Aivika.Dynamics.Simulation
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines the 'Simulation' monad representing a simulation run.
--
module Simulation.Aivika.Dynamics.Simulation
       (Simulation,
        SimulationLift(..),
        Specs(..),
        Method(..),
        runSimulation,
        runSimulations,
        simulationIndex,
        simulationCount,
        simulationSpecs) where

import Simulation.Aivika.Dynamics.Internal.Simulation
