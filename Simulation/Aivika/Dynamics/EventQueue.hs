
-- |
-- Module     : Simulation.Aivika.Dynamics.EventQueue
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module introduces the event queue. An event handler is
-- the Dynamics computation that has a single purpose to perform
-- some side effect at the desired time. To pass in any message
-- to the event, you can use a closure.
--
module Simulation.Aivika.Dynamics.EventQueue
       (EventQueue,
        newQueue,
        enqueue,
        enqueueWithTimes,
        enqueueWithIntegTimes,
        enqueueWithStartTime,
        enqueueWithStopTime,
        enqueueWithCurrentTime,
        runQueue,
        runQueueSync,
        runQueueBefore,
        runQueueSyncBefore,
        queueCount) where

import Data.IORef
import Control.Monad

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics
import qualified Simulation.Aivika.PriorityQueue as PQ

-- | The 'EventQueue' type represents the event queue.
data EventQueue = EventQueue { 
  queuePQ   :: PQ.PriorityQueue (Dynamics ()),
  queueBusy :: IORef Bool,
  queueTime :: IORef Double, 
  -- Optimization
  runQueue  :: Dynamics (),
  -- ^ Run the event queue processing its events.
  -- There is no restiction on the time of the queue itself. It this time
  -- is greater than the current simulation time then nothing happens.
  runQueueSync :: Dynamics (),
  -- ^ Run the event queue synchronously, i.e. the current time cannot be
  -- less than the actual time of the queue itself.
  --
  -- You will rarely need to run the event queue explicitly, but
  -- if you do want then this function is probably that one you should use.
  runQueueBefore :: Dynamics (),
  -- ^ Run the event queue processing only those events
  -- which time is less than the current simulation time.
  -- There is no restiction on the time of the queue itself. It this time
  -- is greater than the current simulation time then nothing happens.
  runQueueSyncBefore :: Dynamics ()
  -- ^ Run the event queue synchronously processing only those events
  -- which time is less than the current simulation time. But the current
  -- time cannot be less than the actual time of the queue itself.
  --
  -- This function is usually called before a handler is subscribed
  -- to the signal. Earlier 'runQueueSync' was called instead, which could
  -- lead to the lost of the signal by the handler at time of direct
  -- subscribing. Changed in version 0.6.1.
  }

-- | Create a new event queue.
newQueue :: Simulation EventQueue
newQueue = 
  Simulation $ \r ->
  do let sc = runSpecs r
     f <- newIORef False
     t <- newIORef $ spcStartTime sc
     pq <- PQ.newQueue
     let q = EventQueue { queuePQ   = pq,
                          queueBusy = f,
                          queueTime = t, 
                          runQueue  = runQueueCore True q,
                          runQueueSync = runQueueSyncCore True q,
                          runQueueBefore = runQueueCore False q,
                          runQueueSyncBefore = runQueueSyncCore False q }
     return q
             
-- | Enqueue the event which must be actuated at the specified time.
enqueue :: EventQueue -> Double -> Dynamics () -> Dynamics ()
enqueue q t c = Dynamics r where
  r p = let pq = queuePQ q in PQ.enqueue pq t c
    
-- | Run the event queue processing its events.
runQueueCore :: Bool -> EventQueue -> Dynamics ()
runQueueCore includingCurrentTime q = Dynamics r where
  r p =
    do let f = queueBusy q
       f' <- readIORef f
       unless f' $
         do writeIORef f True
            call q p
            writeIORef f False
  call q p =
    do let pq = queuePQ q
       f <- PQ.queueNull pq
       unless f $
         do (t2, c2) <- PQ.queueFront pq
            let t = queueTime q
            t' <- readIORef t
            when (t2 < t') $ 
              error "The time value is too small: runQueueCore"
            when ((t2 < pointTime p) ||
                  (includingCurrentTime && (t2 == pointTime p))) $
              do writeIORef t t2
                 PQ.dequeue pq
                 let sc  = pointSpecs p
                     t0  = spcStartTime sc
                     dt  = spcDT sc
                     n2  = fromIntegral $ floor ((t2 - t0) / dt)
                     Dynamics k = c2
                 k $ p { pointTime = t2,
                         pointIteration = n2,
                         pointPhase = -1 }
                 call q p

-- | Run the event queue synchronously, i.e. without past.
runQueueSyncCore :: Bool -> EventQueue -> Dynamics ()
runQueueSyncCore includingCurrentTime q = Dynamics r where
  r p =
    do let t = queueTime q
       t' <- readIORef t
       if pointTime p < t'
         then error $
              "The current time is less than " ++
              "the time in the queue: runQueueSyncCore"
         else m p
  Dynamics m = if includingCurrentTime
               then runQueue q
               else runQueueBefore q
  
-- | Return the number of pending events that should
-- be yet actuated.
queueCount :: EventQueue -> Dynamics Int
queueCount q = Dynamics r where
  r p = 
    do let Dynamics m = runQueueSync q
       m p
       PQ.queueCount $ queuePQ q
       
-- | Actuate the event handler in the specified time points.
enqueueWithTimes :: EventQueue -> [Double] -> Dynamics () -> Dynamics ()
enqueueWithTimes q ts m = loop ts
  where loop []       = return ()
        loop (t : ts) = enqueue q t $ m >> loop ts
       
-- | Actuate the event handler in the specified time points.
enqueueWithPoints :: EventQueue -> [Point] -> Dynamics () -> Dynamics ()
enqueueWithPoints q xs (Dynamics m) = loop xs
  where loop []       = return ()
        loop (x : xs) = enqueue q (pointTime x) $ 
                        Dynamics $ \p ->
                        do m x    -- N.B. we substitute the time point!
                           let Dynamics m' = loop xs
                           m' p

-- | Actuate the event handler in the integration time points.
enqueueWithIntegTimes :: EventQueue -> Dynamics () -> Dynamics ()
enqueueWithIntegTimes q m =
  Dynamics $ \p ->
  do let sc  = pointSpecs p
         (nl, nu) = integIterationBnds sc
         points  = map point [nl .. nu]
         point n = Point { pointSpecs = sc,
                           pointRun = pointRun p,
                           pointTime = basicTime sc n 0,
                           pointIteration = n,
                           pointPhase = 0 }
         Dynamics m' = enqueueWithPoints q points m
     m' p

-- | Actuate the event handler in the start time.
enqueueWithStartTime :: EventQueue -> Dynamics () -> Dynamics ()
enqueueWithStartTime q m =
  Dynamics $ \p ->
  do let sc  = pointSpecs p
         (nl, nu) = integIterationBnds sc
         point n = Point { pointSpecs = sc,
                           pointRun = pointRun p,
                           pointTime = basicTime sc n 0,
                           pointIteration = n,
                           pointPhase = 0 }
         Dynamics m' = enqueueWithPoints q [point nl] m
     m' p

-- | Actuate the event handler in the stop time.
enqueueWithStopTime :: EventQueue -> Dynamics () -> Dynamics ()
enqueueWithStopTime q m =
  Dynamics $ \p ->
  do let sc  = pointSpecs p
         (nl, nu) = integIterationBnds sc
         point n = Point { pointSpecs = sc,
                           pointRun = pointRun p,
                           pointTime = basicTime sc n 0,
                           pointIteration = n,
                           pointPhase = 0 }
         Dynamics m' = enqueueWithPoints q [point nu] m
     m' p

-- | Actuate the event handler in the current time but 
-- through the event queue, which allows continuing the 
-- current tasks and then calling the handler after the 
-- tasks are finished. The simulation time will be the same.
enqueueWithCurrentTime :: EventQueue -> Dynamics () -> Dynamics ()
enqueueWithCurrentTime q m =
  Dynamics $ \p ->
  do let Dynamics m' = enqueue q (pointTime p) m
     m' p
