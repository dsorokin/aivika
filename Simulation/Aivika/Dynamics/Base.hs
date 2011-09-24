
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
        -- * Interpolation and Initial Value
        initD,
        discrete,
        interpolate,
        -- * Memoization
        memo,
        umemo,
        memo0,
        umemo0,
        -- * Iterating
        iterateD,
        -- * Fold
        foldD1,
        foldD,
        -- * Norming
        divideD) where

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Time
import Simulation.Aivika.Dynamics.Internal.Interpolate
import Simulation.Aivika.Dynamics.Internal.Memo
import Simulation.Aivika.Dynamics.Internal.Fold
