
-- |
-- Module     : Simulation.Aivika.Arrival
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
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
        arrivalProcessingTime) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Processor
import Simulation.Aivika.Stream
import Simulation.Aivika.Statistics
import Simulation.Aivika.Ref
import Simulation.Aivika.Internal.Arrival

-- | Accumulates the statistics about that how long the arrived events are processed.
data ArrivalTimer =
  ArrivalTimer { arrivalProcessingTimeRef :: Ref (SamplingStats Double) }

-- | Create a new timer that measures how long the arrived events are processed.
newArrivalTimer :: Simulation ArrivalTimer
newArrivalTimer =
  do r <- newRef emptySamplingStats
     return ArrivalTimer { arrivalProcessingTimeRef = r }

-- | Return the statistics about that how long the arrived events were processed.
arrivalProcessingTime :: ArrivalTimer -> Event (SamplingStats Double)
arrivalProcessingTime = readRef . arrivalProcessingTimeRef

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
         return (a, Cons $ loop xs)
