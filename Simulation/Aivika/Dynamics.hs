
-- |
-- Module     : Simulation.Aivika.Dynamics
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- The module defines the 'Dynamics' monad representing a time varying polymorphic function. 
--
module Simulation.Aivika.Dynamics
       (-- * Dynamics Monad
        Dynamics,
        DynamicsLift(..),
        runDynamicsInStartTime,
        runDynamicsInStopTime,
        runDynamicsInIntegTimes,
        runDynamicsInTime,
        runDynamicsInTimes,
        -- * Error Handling
        catchDynamics,
        finallyDynamics,
        throwDynamics,
        -- * Simulation Time
        time,
        isTimeInteg,
        integIteration,
        integPhase,
        -- * Debugging
        traceDynamics) where

import Simulation.Aivika.Internal.Dynamics
