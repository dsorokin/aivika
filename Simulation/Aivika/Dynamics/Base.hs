
-- |
-- Module     : Simulation.Aivika.Dynamics.Base
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module defines basic functions for the 'Dynamics' monad.
--

module Simulation.Aivika.Dynamics.Base
       (-- * Time Parameters
        starttime, 
        stoptime,
        dt,
        time,
        integTimes,
        isTimeInteg,
        integIteration,
        integIterationBnds,
        integIterationLoBnd,
        integIterationHiBnd,
        -- * Interpolation and Initial Value
        initDynamics,
        discrete,
        interpolate,
        -- * Memoization
        memo,
        umemo,
        memo0,
        umemo0,
        -- * Iterating
        iterateDynamics,
        -- * Fold
        foldDynamics1,
        foldDynamics,
        -- * Norming
        divideDynamics) where

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Time
import Simulation.Aivika.Dynamics.Internal.Interpolate
import Simulation.Aivika.Dynamics.Internal.Memo
import Simulation.Aivika.Dynamics.Internal.Fold
