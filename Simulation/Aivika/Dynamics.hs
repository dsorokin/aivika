
-- |
-- Module     : Simulation.Aivika.Dynamics
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- The module defines the 'Dynamics' monad representing an abstract dynamic 
-- process, i.e. a time varying polymorphic function. 
-- 
-- This is a key point of the Aivika simulation library. With help of this monad 
-- we can simulate the system of ordinary differential equations (ODEs) of 
-- System Dynamics, define the tasks of Discrete Event Simulation (DES) supporting 
-- different paradigms. Also we can use the Agent-based Modeling. Thus, 
-- we can create hybrid simulation models.
--
module Simulation.Aivika.Dynamics 
       (Dynamics,
        DynamicsLift(..),
        runDynamicsInStart,
        runDynamicsInFinal,
        runDynamics) where

import Simulation.Aivika.Dynamics.Internal.Dynamics
