
-- |
-- Module     : Simulation.Aivika.Statistics.Accumulator
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This small utility module allows accumulating the timing statistics based on 'Signalable' data
-- such as the queue size or the number of lost items in the queue.
--

module Simulation.Aivika.Statistics.Accumulator
       (-- * Timing Statistics Accumulator
        TimingStatsAccumulator,
        newTimingStatsAccumulator,
        timingStatsAccumulated) where

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Ref
import Simulation.Aivika.Statistics
import Simulation.Aivika.Signal

-- | Represents an accumulator for the timing statistics.
newtype TimingStatsAccumulator a =
  TimingStatsAccumulator { timingStatsAccumulatedRef :: Ref (TimingStats a) }

-- | Return the accumulated statistics.
timingStatsAccumulated :: TimingStatsAccumulator a -> Event (TimingStats a)
timingStatsAccumulated = readRef . timingStatsAccumulatedRef

-- | Start gathering the timing statistics from the current simulation time. 
newTimingStatsAccumulator :: TimingData a => Signalable a -> Event (TimingStatsAccumulator a)
newTimingStatsAccumulator x =
  do t0 <- liftDynamics time
     a0 <- readSignalable x
     r  <- liftSimulation $ newRef (returnTimingStats t0 a0)
     handleSignal_ (signalableChanged x) $ \a ->
       do t <- liftDynamics time
          modifyRef r $ addTimingStats t a
     return TimingStatsAccumulator { timingStatsAccumulatedRef = r }
