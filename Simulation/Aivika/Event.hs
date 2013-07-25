
-- |
-- Module     : Simulation.Aivika.Event
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines the 'Event' monad which is very similar to the 'Dynamics'
-- monad but only now the computation is strongly synchronized with the event queue.
--
module Simulation.Aivika.Event
       (-- * Event Monad
        Event,
        EventLift(..),
        EventProcessing(..),
        EventCancellation(..),
        runEvent,
        runEventInStartTime,
        runEventInStopTime,
        -- * Event Queue
        enqueueEvent,
        enqueueEventWithCancellation,
        enqueueEventWithTimes,
        enqueueEventWithIntegTimes,
        enqueueEventWithStartTime,
        enqueueEventWithStopTime,
        enqueueEventWithCurrentTime,
        eventQueueCount,
        -- * Error Handling
        catchEvent,
        finallyEvent,
        throwEvent) where

import Simulation.Aivika.Internal.Event
