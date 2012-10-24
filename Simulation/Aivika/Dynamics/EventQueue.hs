
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
        queueRun) where

import Data.IORef
import Control.Monad

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics
import qualified Simulation.Aivika.PriorityQueue as PQ

-- | The 'EventQueue' type represents the event queue.
data EventQueue = EventQueue { 
  queuePQ   :: PQ.PriorityQueue (Dynamics ()),
  queueRun  :: Dynamics (),   -- ^ Run the event queue processing its events
  queueBusy :: IORef Bool,
  queueTime :: IORef Double }

-- | Create a new event queue.
newQueue :: Simulation EventQueue
newQueue = 
  Simulation $ \r ->
  do let sc = runSpecs r
     f <- newIORef False
     t <- newIORef $ spcStartTime sc
     pq <- PQ.newQueue
     let q = EventQueue { queuePQ   = pq,
                          queueRun  = runQueue q,
                          queueBusy = f,
                          queueTime = t }
     return q
             
-- | Enqueue the event which must be actuated at the specified time.
enqueue :: EventQueue -> Double -> Dynamics () -> Dynamics ()
enqueue q t c = Dynamics r where
  r p = let pq = queuePQ q in PQ.enqueue pq t c
    
-- | Run the event queue processing its events.
runQueue :: EventQueue -> Dynamics ()
runQueue q = Dynamics r where
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
              error "The time value is too small: subrunQueue"
            when (t2 <= pointTime p) $
              do writeIORef t t2
                 PQ.dequeue pq
                 let sc  = pointSpecs p
                     t0  = spcStartTime sc
                     dt  = spcDT sc
                     n2  = fromInteger $ toInteger $ floor ((t2 - t0) / dt)
                     Dynamics k = c2
                 k $ p { pointTime = t2,
                         pointIteration = n2,
                         pointPhase = -1 }
                 call q p
