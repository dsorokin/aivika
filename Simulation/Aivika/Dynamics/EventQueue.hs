
-- |
-- Module     : Simulation.Aivika.Dynamics.EventQueue
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- The module introduces the event queue. Any event is the Dynamics computation,
-- or, saying differently, a dynamic process that has a single purpose 
-- to perform some side effect at the desired time. To pass the message, 
-- we actually use a closure.
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
  runQueueSync :: Dynamics ()
  -- ^ Run the event queue synchronously, i.e. without past.
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
                          runQueue  = runQueueCore q,
                          runQueueSync = runQueueSyncCore q }
     return q
             
-- | Enqueue the event which must be actuated at the specified time.
enqueue :: EventQueue -> Double -> Dynamics () -> Dynamics ()
enqueue q t c = Dynamics r where
  r p = let pq = queuePQ q in PQ.enqueue pq t c
    
-- | Run the event queue processing its events.
runQueueCore :: EventQueue -> Dynamics ()
runQueueCore q = Dynamics r where
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
              error "The time value is too small: runQueue"
            when (t2 <= pointTime p) $
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
runQueueSyncCore :: EventQueue -> Dynamics ()
runQueueSyncCore q = Dynamics r where
  r p =
    do let t = queueTime q
       t' <- readIORef t
       if pointTime p < t'
         then error $
              "The current time is less than " ++
              "the time in the queue: runQueueSync"
         else let Dynamics m = runQueue q
              in m p
  
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