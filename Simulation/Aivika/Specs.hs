
-- |
-- Module     : Simulation.Aivika.Specs
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- It defines the simulation specs and functions for this data type.
module Simulation.Aivika.Specs
       (Specs(..),
        Method(..),
        basicTime,
        integIterationBnds,
        integIterationHiBnd,
        integIterationLoBnd,
        integPhaseBnds,
        integPhaseHiBnd,
        integPhaseLoBnd,
        integTimes) where

import Simulation.Aivika.Internal.Specs
