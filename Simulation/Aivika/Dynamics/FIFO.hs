
-- |
-- Module     : Simulation.Aivika.Dynamics.FIFO
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines the FIFO queue.
--
module Simulation.Aivika.Dynamics.FIFO
       (FIFO,
        fifoQueue,
        fifoNull,
        fifoFull,
        fifoMaxCount,
        fifoCount,
        fifoLostCount,
        fifoEnqueue,
        fifoDequeue,
        fifoEnqueueLost,
        newFIFO,
        dequeueFIFO,
        tryDequeueFIFO,
        enqueueFIFO,
        tryEnqueueFIFO,
        enqueueFIFOOrLost) where

import Data.IORef
import Data.Array
import Data.Array.IO.Safe

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Process
import Simulation.Aivika.Dynamics.Resource
import Simulation.Aivika.Dynamics.Internal.Signal
import Simulation.Aivika.Dynamics.Signal

-- | Represents the FIFO queue with rule: first input - first output.
data FIFO a =
  FIFO { fifoQueue :: EventQueue,  -- ^ Return the event queue.
         fifoMaxCount :: Int,      -- ^ The maximum available number of items.
         fifoReadRes  :: Resource,
         fifoWriteRes :: Resource,
         fifoCountRef :: IORef Int,
         fifoLostCountRef :: IORef Int,
         fifoStartRef :: IORef Int,
         fifoEndRef   :: IORef Int,
         fifoArray :: IOArray Int a, 
         fifoEnqueueSource :: SignalSource a,
         fifoEnqueueLostSource :: SignalSource a,
         fifoDequeueSource :: SignalSource a,
         fifoUpdatedSource :: SignalSource a }
  
-- | Create a new FIFO queue with the specified maximum available number of items.  
newFIFO :: EventQueue -> Int -> Simulation (FIFO a)  
newFIFO q count =
  do i <- liftIO $ newIORef 0
     l <- liftIO $ newIORef 0
     s <- liftIO $ newIORef 0
     e <- liftIO $ newIORef 0
     a <- liftIO $ newArray_ (0, count - 1)
     r <- newResourceWithCount q count 0
     w <- newResourceWithCount q count count
     s1 <- newSignalSourceUnsafe
     s2 <- newSignalSourceUnsafe
     s3 <- newSignalSourceUnsafe
     s4 <- newSignalSource q
     return FIFO { fifoQueue = q,
                   fifoMaxCount = count,
                   fifoReadRes  = r,
                   fifoWriteRes = w,
                   fifoCountRef = i,
                   fifoLostCountRef = l,
                   fifoStartRef = s,
                   fifoEndRef   = e,
                   fifoArray = a, 
                   fifoEnqueueSource = s1,
                   fifoEnqueueLostSource = s2,
                   fifoDequeueSource = s3,
                   fifoUpdatedSource = s4 }
  
-- | Test whether the FIFO queue is empty.
fifoNull :: FIFO a -> Dynamics Bool
fifoNull fifo =
  do a <- fifoCount fifo
     return (a == 0)

-- | Test whether the FIFO queue is full.
fifoFull :: FIFO a -> Dynamics Bool
fifoFull fifo =
  do a <- fifoCount fifo
     return (a == fifoMaxCount fifo)

-- | Return the queue size.
fifoCount :: FIFO a -> Dynamics Int
fifoCount fifo =
  liftIO $ readIORef (fifoCountRef fifo)
  
-- | Return the number of lost items.
fifoLostCount :: FIFO a -> Dynamics Int
fifoLostCount fifo =
  liftIO $ readIORef (fifoLostCountRef fifo)
  
-- | Dequeue from the FIFO queue suspending the process if
-- the queue is empty.
dequeueFIFO :: FIFO a -> Process a  
dequeueFIFO fifo =
  do requestResource (fifoReadRes fifo)
     a <- liftIO $ dequeueImpl fifo
     releaseResource (fifoWriteRes fifo)
     liftDynamics $ triggerSignal (fifoDequeueSource fifo) a
     return a
  
-- | Try to dequeue from the FIFO queue immediately.  
tryDequeueFIFO :: FIFO a -> Dynamics (Maybe a)
tryDequeueFIFO fifo =
  do x <- tryRequestResourceInDynamics (fifoReadRes fifo)
     if x 
       then do a <- liftIO $ dequeueImpl fifo
               releaseResourceInDynamics (fifoWriteRes fifo)
               triggerSignal (fifoDequeueSource fifo) a
               return $ Just a
       else return Nothing

-- | Enqueue the item in the FIFO queue suspending the process
-- if the queue is full.  
enqueueFIFO :: FIFO a -> a -> Process ()
enqueueFIFO fifo a =
  do requestResource (fifoWriteRes fifo)
     liftIO $ enqueueImpl fifo a
     releaseResource (fifoReadRes fifo)
     liftDynamics $ triggerSignal (fifoEnqueueSource fifo) a
     
-- | Try to enqueue the item in the FIFO queue. Return 'False' in
-- the monad if the queue is full.
tryEnqueueFIFO :: FIFO a -> a -> Dynamics Bool
tryEnqueueFIFO fifo a =
  do x <- tryRequestResourceInDynamics (fifoWriteRes fifo)
     if x 
       then do liftIO $ enqueueImpl fifo a
               releaseResourceInDynamics (fifoReadRes fifo)
               triggerSignal (fifoEnqueueSource fifo) a
               return True
       else return False

-- | Try to enqueue the item in the FIFO queue. If the queue is full
-- then the item will be lost.
enqueueFIFOOrLost :: FIFO a -> a -> Dynamics ()
enqueueFIFOOrLost fifo a =
  do x <- tryRequestResourceInDynamics (fifoWriteRes fifo)
     if x
       then do liftIO $ enqueueImpl fifo a
               releaseResourceInDynamics (fifoReadRes fifo)
               triggerSignal (fifoEnqueueSource fifo) a
       else do liftIO $ modifyIORef (fifoLostCountRef fifo) $ (+) 1
               triggerSignal (fifoEnqueueLostSource fifo) a

-- | Return a signal that notifies when any item is enqueued.
fifoEnqueue :: FIFO a -> Signal a
fifoEnqueue fifo = merge2Signals m1 m2    -- N.B. The order is important!
  where m1 = publishSignal (fifoUpdatedSource fifo)
        m2 = publishSignal (fifoEnqueueSource fifo)

-- | Return a signal which notifies that the item was lost when 
-- attempting to add it to the full queue with help of
-- 'enqueueFIFOOrLost'.
fifoEnqueueLost :: FIFO a -> Signal a
fifoEnqueueLost fifo = merge2Signals m1 m2    -- N.B. The order is important!
  where m1 = publishSignal (fifoUpdatedSource fifo)
        m2 = publishSignal (fifoEnqueueLostSource fifo)

-- | Return a signal that notifies when any item is dequeued.
fifoDequeue :: FIFO a -> Signal a
fifoDequeue fifo = merge2Signals m1 m2    -- N.B. The order is important!
  where m1 = publishSignal (fifoUpdatedSource fifo)
        m2 = publishSignal (fifoDequeueSource fifo)

-- | An implementation method.
dequeueImpl :: FIFO a -> IO a
dequeueImpl fifo =
  do i <- readIORef (fifoCountRef fifo)
     s <- readIORef (fifoStartRef fifo)
     let i' = i - 1
         s' = (s + 1) `mod` fifoMaxCount fifo
     a <- readArray (fifoArray fifo) s
     writeArray (fifoArray fifo) s undefined
     i' `seq` writeIORef (fifoCountRef fifo) i'
     s' `seq` writeIORef (fifoStartRef fifo) s'
     return a

-- | An implementation method.
enqueueImpl :: FIFO a -> a -> IO ()
enqueueImpl fifo a =
  do i <- readIORef (fifoCountRef fifo)
     e <- readIORef (fifoEndRef fifo)
     let i' = i + 1
         e' = (e + 1) `mod` fifoMaxCount fifo
     a `seq` writeArray (fifoArray fifo) e a
     i' `seq` writeIORef (fifoCountRef fifo) i'
     e' `seq` writeIORef (fifoEndRef fifo) e'
