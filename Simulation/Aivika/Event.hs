
-- |
-- Module     : Simulation.Aivika.Event
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines the 'Event' monad which is very similar to the 'Dynamics'
-- monad but only now the computation is strongly synchronized with the event queue.
--
-- The @Dynamics@ computation is defined in all time points simultaneously, while
-- the @Event@ computation can be described in every time point differently and can change
-- in discrete steps. Therefore, the former is destined for differential and difference
-- equations of System Dynamics, while the latter is destined for discrete event simulation,
-- being its core actually.
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
        enqueueEventWithStartTime,
        enqueueEventWithStopTime,
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
        memoEvent,
        memoEventInTime,
        -- * Disposable
        DisposableEvent(..),
        -- * Retrying Computation
        retryEvent,
        -- * Debugging
        traceEvent) where

import Simulation.Aivika.Internal.Event
