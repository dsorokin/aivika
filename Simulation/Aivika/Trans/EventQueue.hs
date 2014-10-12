
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.EventQueue
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the event queue.
--
module Simulation.Aivika.Trans.EventQueue
       (-- * Event Queue Processing
        EventQueueable(..),
        EventQueueing(..)) where

import Data.IORef

import Control.Monad

import qualified Simulation.Aivika.PriorityQueue as PQ

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event

instance EventQueueable IO where

  data EventQueue IO =
    EventQueue { queuePQ :: PQ.PriorityQueue (Point IO -> IO ()),
                 -- ^ the underlying priority queue
                 queueBusy :: IORef Bool,
                 -- ^ whether the queue is currently processing events
                 queueTime :: IORef Double
                 -- ^ the actual time of the event queue
               }
  
  newEventQueue session specs = 
    do f <- newIORef False
       t <- newIORef $ spcStartTime specs
       pq <- PQ.newQueue
       return EventQueue { queuePQ   = pq,
                           queueBusy = f,
                           queueTime = t }

instance EventQueueing IO where
  
  {-# INLINE enqueueEvent #-}
  enqueueEvent t (Event m) =
    Event $ \p ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in PQ.enqueue pq t m

  {-# INLINE runEventWith #-}
  runEventWith processing (Event e) =
    Dynamics $ \p ->
    do invokeDynamics p $ processEvents processing
       e p

  {-# INLINE eventQueueCount #-}
  eventQueueCount =
    Event $ PQ.queueCount . queuePQ . runEventQueue . pointRun

instance Comp IO
instance Enq IO

-- | Process the pending events.
processPendingEventsCore :: Bool -> Dynamics IO ()
processPendingEventsCore includingCurrentEvents = Dynamics r where
  r p =
    do let q = runEventQueue $ pointRun p
           f = queueBusy q
       f' <- readIORef f
       unless f' $
         do writeIORef f True
            call q p
            writeIORef f False
  call q p =
    do let pq = queuePQ q
           r  = pointRun p
       f <- PQ.queueNull pq
       unless f $
         do (t2, c2) <- PQ.queueFront pq
            let t = queueTime q
            t' <- readIORef t
            when (t2 < t') $ 
              error "The time value is too small: processPendingEventsCore"
            when ((t2 < pointTime p) ||
                  (includingCurrentEvents && (t2 == pointTime p))) $
              do writeIORef t t2
                 PQ.dequeue pq
                 let sc = pointSpecs p
                     t0 = spcStartTime sc
                     dt = spcDT sc
                     n2 = fromIntegral $ floor ((t2 - t0) / dt)
                 c2 $ p { pointTime = t2,
                          pointIteration = n2,
                          pointPhase = -1 }
                 call q p

-- | Process the pending events synchronously, i.e. without past.
processPendingEvents :: Bool -> Dynamics IO ()
processPendingEvents includingCurrentEvents = Dynamics r where
  r p =
    do let q = runEventQueue $ pointRun p
           t = queueTime q
       t' <- readIORef t
       if pointTime p < t'
         then error $
              "The current time is less than " ++
              "the time in the queue: processPendingEvents"
         else invokeDynamics p m
  m = processPendingEventsCore includingCurrentEvents

-- | A memoized value.
processEventsIncludingCurrent = processPendingEvents True

-- | A memoized value.
processEventsIncludingEarlier = processPendingEvents False

-- | A memoized value.
processEventsIncludingCurrentCore = processPendingEventsCore True

-- | A memoized value.
processEventsIncludingEarlierCore = processPendingEventsCore True

-- | Process the events.
processEvents :: EventProcessing -> Dynamics IO ()
processEvents CurrentEvents = processEventsIncludingCurrent
processEvents EarlierEvents = processEventsIncludingEarlier
processEvents CurrentEventsOrFromPast = processEventsIncludingCurrentCore
processEvents EarlierEventsOrFromPast = processEventsIncludingEarlierCore
