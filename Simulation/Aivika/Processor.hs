
-- |
-- Module     : Simulation.Aivika.Processor
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The processor of simulation data.
--
module Simulation.Aivika.Processor
       (Processor(..),
        processorUsingId,
        processorParallel,
        processorParallelUsingIds,
        newRoundRobbinProcessor,
        newRoundRobbinProcessorUsingIds) where

import Simulation.Aivika.Simulation
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.Stream

-- | Represents a processor of simulation data (it seems to be an Arrow).
newtype Processor a b =
  Processor { runProcessor :: Stream a -> Stream b
              -- ^ Run the processor.
            }

-- | Create a processor that will use the specified process identifier.
-- It is useful to refer to the underlying 'Process' computation which
-- can be passivated, interrupted, canceled and so on.
processorUsingId :: ProcessId -> Processor a b -> Processor a b
processorUsingId = undefined

-- | Launches the specified processors in parallel.
processorParallel :: [Processor a b] -> Processor a b
processorParallel = undefined

-- | Launches the specified processors in parallel using the provided identifiers.
-- It is useful to refer to the underlying 'Process' computations which can be
-- passivated, interrupted, canceled and so on.
processorParallelUsingIds :: [(ProcessId, Processor a b)] -> Processor a b
processorParallelUsingIds = undefined

-- | Create a new Round-Robbin processor.
newRoundRobbinProcessor :: (a -> (Process Double, Process b))
                           -- ^ A function of input that returns the timeout
                           -- and process which should be performed within
                           -- the specified timeout.
                           -> Simulation (Processor a b)
newRoundRobbinProcessor = undefined

-- | Create a new Round-Robbin processor that uses the specified processor identifiers.
newRoundRobbinProcessorUsingIds :: (a -> (ProcessId, Process Double, Process b))
                                   -- ^ A function of input that returns a new
                                   -- process identifier, the timeout
                                   -- and the process itself which will be launched
                                   -- with the specified identifier and which should
                                   -- be performed within the specified timeout.
                                   -> Simulation (Processor a b)
newRoundRobbinProcessorUsingIds = undefined