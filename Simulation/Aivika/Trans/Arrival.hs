
-- |
-- Module     : Simulation.Aivika.Trans.Arrival
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the types and functions for working with the events
-- that can represent something that arrive from outside the model, or
-- represent other things which computation is delayed and hence is not synchronized.
--
-- Therefore, the additional information is provided about the time and delay of arrival.

module Simulation.Aivika.Trans.Arrival
       (Arrival(..),
        ArrivalTimer,
        newArrivalTimer,
        arrivalTimerProcessor,
        arrivalProcessingTime,
        arrivalProcessingTimeChanged,
        arrivalProcessingTimeChanged_) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Processor
import Simulation.Aivika.Trans.Stream
import Simulation.Aivika.Trans.Statistics
import Simulation.Aivika.Trans.Ref
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Internal.Arrival

-- | Accumulates the statistics about that how long the arrived events are processed.
data ArrivalTimer =
  ArrivalTimer { arrivalProcessingTimeRef :: Ref (SamplingStats Double),
                 arrivalProcessingTimeChangedSource :: SignalSource () }

-- | Create a new timer that measures how long the arrived events are processed.
newArrivalTimer :: Simulation ArrivalTimer
newArrivalTimer =
  do r <- newRef emptySamplingStats
     s <- newSignalSource
     return ArrivalTimer { arrivalProcessingTimeRef = r,
                           arrivalProcessingTimeChangedSource = s }

-- | Return the statistics about that how long the arrived events were processed.
arrivalProcessingTime :: ArrivalTimer -> Event (SamplingStats Double)
arrivalProcessingTime = readRef . arrivalProcessingTimeRef

-- | Return a signal raised when the the processing time statistics changes.
arrivalProcessingTimeChanged :: ArrivalTimer -> Signal (SamplingStats Double)
arrivalProcessingTimeChanged timer =
  mapSignalM (const $ arrivalProcessingTime timer) (arrivalProcessingTimeChanged_ timer)

-- | Return a signal raised when the the processing time statistics changes.
arrivalProcessingTimeChanged_ :: ArrivalTimer -> Signal ()
arrivalProcessingTimeChanged_ timer =
  publishSignal (arrivalProcessingTimeChangedSource timer)

-- | Return a processor that actually measures how much time has passed from
-- the time of arriving the events.
arrivalTimerProcessor :: ArrivalTimer -> Processor (Arrival a) (Arrival a)
arrivalTimerProcessor timer =
  Processor $ \xs -> Cons $ loop xs where
    loop xs =
      do (a, xs) <- runStream xs
         liftEvent $
           do t <- liftDynamics time
              modifyRef (arrivalProcessingTimeRef timer) $
                addSamplingStats (t - arrivalTime a)
              triggerSignal (arrivalProcessingTimeChangedSource timer) ()
         return (a, Cons $ loop xs)
