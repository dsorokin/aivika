-- |
-- Module     : Simulation.Aivika.Parameter
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines the 'Parameter' monad that allows representing the model
-- parameters. For example, they can be used when running the Monte-Carlo simulation.
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
        parameterIndex,
        parameterCount,
        parameterSpecs,
        -- * Memoization
        memoParameter,
        -- * Utilities
        newTableParameter,
        newIndexedParameter) where

import Simulation.Aivika.Internal.Parameter