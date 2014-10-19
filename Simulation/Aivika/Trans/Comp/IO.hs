
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.Comp.IO
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the event queue within monad 'IO'.
--
module Simulation.Aivika.Trans.Comp.IO() where

import Control.Monad

import qualified Simulation.Aivika.Trans.PriorityQueue as PQ

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs

instance EventQueueing IO where

  data EventQueue IO =
    EventQueue { queuePQ :: PQ.PriorityQueue IO (Point IO -> IO ()),
                 -- ^ the underlying priority queue
                 queueBusy :: ProtoRef IO Bool,
                 -- ^ whether the queue is currently processing events
                 queueTime :: ProtoRef IO Double
                 -- ^ the actual time of the event queue
               }
  
  {-# INLINABLE newEventQueue #-}
  newEventQueue session specs = 
    do f <- newProtoRef session False
       t <- newProtoRef session $ spcStartTime specs
       pq <- PQ.newQueue session
       return EventQueue { queuePQ   = pq,
                           queueBusy = f,
                           queueTime = t }

  {-# INLINABLE enqueueEvent #-}
  enqueueEvent t (Event m) =
    Event $ \p ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in PQ.enqueue pq t m

  {-# INLINABLE runEventWith #-}
  runEventWith processing (Event e) =
    Dynamics $ \p ->
    do invokeDynamics p $ processEvents processing
       e p

  {-# INLINABLE eventQueueCount #-}
  eventQueueCount =
    Event $ PQ.queueCount . queuePQ . runEventQueue . pointRun

instance Comp IO

-- | Process the pending events.
processPendingEventsCore :: Bool -> Dynamics IO ()
{-# INLINABLE processPendingEventsCore #-}
processPendingEventsCore includingCurrentEvents = Dynamics r where
  r p =
    do let q = runEventQueue $ pointRun p
           f = queueBusy q
       f' <- readProtoRef f
       unless f' $
         do writeProtoRef f True
            call q p
            writeProtoRef f False
  call q p =
    do let pq = queuePQ q
           r  = pointRun p
       f <- PQ.queueNull pq
       unless f $
         do (t2, c2) <- PQ.queueFront pq
            let t = queueTime q
            t' <- readProtoRef t
            when (t2 < t') $ 
              error "The time value is too small: processPendingEventsCore"
            when ((t2 < pointTime p) ||
                  (includingCurrentEvents && (t2 == pointTime p))) $
              do writeProtoRef t t2
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
{-# INLINABLE processPendingEvents #-}
processPendingEvents includingCurrentEvents = Dynamics r where
  r p =
    do let q = runEventQueue $ pointRun p
           t = queueTime q
       t' <- readProtoRef t
       if pointTime p < t'
         then error $
              "The current time is less than " ++
              "the time in the queue: processPendingEvents"
         else invokeDynamics p m
  m = processPendingEventsCore includingCurrentEvents

-- | A memoized value.
processEventsIncludingCurrent :: Dynamics IO ()
{-# INLINABLE processEventsIncludingCurrent #-}
processEventsIncludingCurrent = processPendingEvents True

-- | A memoized value.
processEventsIncludingEarlier :: Dynamics IO ()
{-# INLINABLE processEventsIncludingEarlier #-}
processEventsIncludingEarlier = processPendingEvents False

-- | A memoized value.
processEventsIncludingCurrentCore :: Dynamics IO ()
{-# INLINABLE processEventsIncludingCurrentCore #-}
processEventsIncludingCurrentCore = processPendingEventsCore True

-- | A memoized value.
processEventsIncludingEarlierCore :: Dynamics IO ()
{-# INLINABLE processEventsIncludingEarlierCore #-}
processEventsIncludingEarlierCore = processPendingEventsCore True

-- | Process the events.
processEvents :: EventProcessing -> Dynamics IO ()
{-# INLINABLE processEvents #-}
processEvents CurrentEvents = processEventsIncludingCurrent
processEvents EarlierEvents = processEventsIncludingEarlier
processEvents CurrentEventsOrFromPast = processEventsIncludingCurrentCore
processEvents EarlierEventsOrFromPast = processEventsIncludingEarlierCore
