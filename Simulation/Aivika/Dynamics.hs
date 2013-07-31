
-- |
-- Module     : Simulation.Aivika.Dynamics
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
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
        -- * Time parameters
        starttime,
        stoptime,
        dt,
        time,
        isTimeInteg,
        integIteration,
        integPhase) where

import Simulation.Aivika.Internal.Dynamics
