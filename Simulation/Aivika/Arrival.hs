
-- |
-- Module     : Simulation.Aivika.Arrival
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines the types and functions for working with the events
-- that can represent something that arrive from outside the model, or
-- represent other things which computation is delayed and hence is not synchronized.
--
-- Therefore, the additional information is provided about the time and delay of arrival.

module Simulation.Aivika.Arrival
       (Arrival(..),
        ArrivalTimer,
        newArrivalTimer,
        arrivalTimerProcessor,
        arrivalTimerSignal,
        arrivalTimerChannel,
        arrivalProcessingTime,
        arrivalProcessingTimeChanged,
        arrivalProcessingTimeChanged_,
        resetArrivalTimer) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Composite
import Simulation.Aivika.Processor
import Simulation.Aivika.Stream
import Simulation.Aivika.Statistics
import Simulation.Aivika.Ref
import Simulation.Aivika.Signal
import Simulation.Aivika.Channel
import Simulation.Aivika.Internal.Arrival

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

-- | Return a signal that actually measures how much time has passed from
-- the time of arriving the events.
--
-- Note that the statistics is counted each time you subscribe to the output signal.
-- For example, if you subscribe twice then the statistics counting is duplicated.
-- Ideally, you should subscribe to the output signal only once.
arrivalTimerSignal :: ArrivalTimer -> Signal (Arrival a) -> Signal (Arrival a)
arrivalTimerSignal timer sa =
  Signal { handleSignal = \h ->
            handleSignal sa $ \a ->
            do t <- liftDynamics time
               modifyRef (arrivalProcessingTimeRef timer) $
                 addSamplingStats (t - arrivalTime a)
               h a
         }

-- | Like 'arrivalTimerSignal' but measures how much time has passed from
-- the time of arriving the events in the channel.
arrivalTimerChannel :: ArrivalTimer -> Channel (Arrival a) (Arrival a)
arrivalTimerChannel timer =
  Channel $ \sa ->
  return $ arrivalTimerSignal timer sa

-- | Reset the statistics.
resetArrivalTimer :: ArrivalTimer -> Event ()
resetArrivalTimer timer =
  do writeRef (arrivalProcessingTimeRef timer) emptySamplingStats
     triggerSignal (arrivalProcessingTimeChangedSource timer) ()
