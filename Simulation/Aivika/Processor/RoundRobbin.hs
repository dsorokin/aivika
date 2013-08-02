
-- |
-- Module     : Simulation.Aivika.Processor.RoundRobbin
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines the Round-Robbin processor.
--
module Simulation.Aivika.Processor.RoundRobbin
       (newRoundRobbinProcessor,
        newRoundRobbinProcessorUsingIds) where

import Simulation.Aivika.Simulation
import Simulation.Aivika.Process
import Simulation.Aivika.Processor

-- | Create a new Round-Robbin processor
-- (not implemented yet).
newRoundRobbinProcessor :: (a -> (Process Double, Process b))
                           -- ^ A function of input that returns the timeout
                           -- and process which should be performed within
                           -- the specified timeout.
                           -> Simulation (Processor a b)
newRoundRobbinProcessor = undefined

-- | Create a new Round-Robbin processor that uses the specified processor identifiers.
-- (not implemented yet).
newRoundRobbinProcessorUsingIds :: (a -> (ProcessId, Process Double, Process b))
                                   -- ^ A function of input that returns a new
                                   -- process identifier, the timeout
                                   -- and the process itself which will be launched
                                   -- with the specified identifier and which should
                                   -- be performed within the specified timeout.
                                   -> Simulation (Processor a b)
newRoundRobbinProcessorUsingIds = undefined