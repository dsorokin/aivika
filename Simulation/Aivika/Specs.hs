
-- |
-- Module     : Simulation.Aivika.Specs
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It defines the simulation specs and functions for this data type.
module Simulation.Aivika.Specs
       (-- * Simulation Specs
        Specs(..),
        Method(..),
        -- * Auxiliary Functions
        basicTime,
        integIterationBnds,
        integIterationHiBnd,
        integIterationLoBnd,
        integPhaseBnds,
        integPhaseHiBnd,
        integPhaseLoBnd,
        integTimes,
        gridTimes) where

import Simulation.Aivika.Internal.Specs
