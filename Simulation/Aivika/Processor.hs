
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
       (Processor,
        runProcessor) where

import Simulation.Aivika.Internal.Process
import Simulation.Aivika.Stream

-- | Represents a processor of simulation data (it seems to be an Arrow).
newtype Processor a b =
  Processor { runProcessor :: Stream a -> Stream b
              -- ^ Run the processor.
            }
