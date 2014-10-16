
-- |
-- Module     : Simulation.Aivika.Trans.Dynamics
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'DynamicsT' monad tranformer representing a time varying polymorphic function. 
--
module Simulation.Aivika.Trans.Dynamics
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
        integPhase) where

import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Dynamics
