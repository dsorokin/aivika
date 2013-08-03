
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
       (roundRobbinProcessor,
        roundRobbinProcessorUsingIds) where

import Simulation.Aivika.Simulation
import Simulation.Aivika.Process
import Simulation.Aivika.Processor
import Simulation.Aivika.Stream

-- | Represents the Round-Robbin processor that tries to perform the task within
-- the specified timeout. If the task times out, then it is canceled and returned
-- to the processor again; otherwise, the successful result is redirected to output.
-- (not implemented yet).
roundRobbinProcessor :: Processor (Double, Process a) a
roundRobbinProcessor =
  Processor $
  runProcessor roundRobbinProcessorUsingIds . mapStreamM f where
    f (timeout, p) =
      do pid <- liftSimulation newProcessId
         return (pid, timeout, p)

-- | Like 'roundRobbinProcessor' but allows specifying the process identifiers.
-- (not implemented yet).
roundRobbinProcessorUsingIds :: Processor (ProcessId, Double, Process a) a
roundRobbinProcessorUsingIds = undefined