
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
        runEvent,
        runEventWith,
        runEventInStartTime,
        runEventInStopTime,
        -- * Event Queue
        enqueueEvent,
        enqueueEventWithCancellation,
        enqueueEventWithTimes,
        enqueueEventWithIntegTimes,
        yieldEvent,
        eventQueueCount,
        -- * Cancelling Event
        EventCancellation,
        cancelEvent,
        eventCancelled,
        eventFinished,
        -- * Error Handling
        catchEvent,
        finallyEvent,
        throwEvent,
        -- * Memoization
        memoEvent) where

import Simulation.Aivika.Internal.Event
