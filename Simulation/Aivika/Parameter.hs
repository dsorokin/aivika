-- |
-- Module     : Simulation.Aivika.Parameter
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines the 'Parameter' monad that allows representing the model
-- parameters. For example, they can be used when running the Monte-Carlo simulation.
--
-- In general, this monad is very useful for representing a computation which is external
-- relative to the model itself.
-- 
module Simulation.Aivika.Parameter
       (-- * Parameter
        Parameter,
        ParameterLift(..),
        runParameter,
        runParameters,
        -- * Error Handling
        catchParameter,
        finallyParameter,
        throwParameter,
        -- * Predefined Parameters
        simulationIndex,
        simulationCount,
        simulationSpecs,
        generatorParameter,
        starttime,
        stoptime,
        dt,
        -- * Memoization
        memoParameter,
        -- * Utilities
        tableParameter) where

import Simulation.Aivika.Internal.Parameter
