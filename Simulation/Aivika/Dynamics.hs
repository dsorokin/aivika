
-- |
-- Module     : Simulation.Aivika.Dynamics
-- Copyright  : Copyright (c) 2009-2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines the 'Dynamics' monad representing an abstract dynamic 
-- process, i.e. a time varying polymorphic function. 
--
module Simulation.Aivika.Dynamics 
       (Dynamics,
        DynamicsLift(..),
        runDynamicsInStartTime,
        runDynamicsInStopTime,
        runDynamicsInIntegTimes) where

import Simulation.Aivika.Dynamics.Internal.Dynamics
