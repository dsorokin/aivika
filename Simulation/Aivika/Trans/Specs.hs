
-- |
-- Module     : Simulation.Aivika.Trans.Specs
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines the simulation specs and functions for this data type.
module Simulation.Aivika.Trans.Specs
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
        integTimes) where

import Simulation.Aivika.Trans.Internal.Specs
