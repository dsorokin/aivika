
-- |
-- Module     : Simulation.Aivika.Queue
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines a queue that can use the specified strategies. So, having only
-- the 'FCFS', 'LCFS', 'SIRO' and 'StaticPriorities' strategies, you can build
-- 4 x 3 x 4 = 48 different types of the queue, each of them will have its own
-- behavior (below @StaticPriorities@ can be used for input and output only).
--
module Simulation.Aivika.Queue
       (Queue,
        queueNull,
        queueFull,
        queueMaxCount,
        queueCount,
        queueLostCount,
        enqueued,
        dequeued,
        enqueuedButLost,
        newQueue,
        dequeue,
        dequeueWithPriority,
        dequeueWithDynamicPriority,
        tryDequeue,
        enqueue,
        enqueueWithPriority,
        enqueueWithDynamicPriority,
        tryEnqueue,
        enqueueOrLost,
        enqueueOrLost_) where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.Internal.Signal
import Simulation.Aivika.Signal
import Simulation.Aivika.Resource
import Simulation.Aivika.QueueStrategy

-- | Represents the queue using the specified strategies for input @si@,
-- internal storing (in memory) @sm@ and output @so@, where @a@ denotes
-- the type of items stored in the queue. Types @qi@, @qm@ and @qo@ are
-- determined automatically and you should not care about them - they
-- are dependent types.
data Queue si qi sm qm so qo a =
  Queue { queueMaxCount :: Int,
          -- ^ The maximum available number of items.
          queueInputStrategy :: si,
          queueMemoryStrategy :: sm,
          queueOutputStrategy :: so,
          queueInputRes :: Resource si qi,
          queueMemory :: qm a,
          queueOutputRes :: Resource so qo,
          queueCountRef :: IORef Int,
          queueLostCountRef :: IORef Int,
          enqueuedSource :: SignalSource a,
          enqueuedButLostSource :: SignalSource a,
          dequeuedSource :: SignalSource a }
  
-- | Create a new queue with the specified strategies and maximum available number of items.  
newQueue :: (QueueStrategy si qi,
             QueueStrategy sm qm,
             QueueStrategy so qo) =>
            si
            -- ^ the strategy applied to the input (enqueuing) process
            -> sm
            -- ^ the strategy applied when storing items in the queue
            -> so
            -- ^ the strategy applied to the output (dequeuing) process
            -> Int
            -- ^ the maximum available number of items
            -> Simulation (Queue si qi sm qm so qo a)  
newQueue si sm so count =
  do i  <- liftIO $ newIORef 0
     l  <- liftIO $ newIORef 0
     ri <- newResourceWithCount si count count
     qm <- newStrategyQueue sm
     ro <- newResourceWithCount so count 0
     s1 <- newSignalSource
     s2 <- newSignalSource
     s3 <- newSignalSource
     return Queue { queueMaxCount = count,
                    queueInputStrategy = si,
                    queueMemoryStrategy = sm,
                    queueOutputStrategy = so,
                    queueInputRes = ri,
                    queueMemory = qm,
                    queueOutputRes = ro,
                    queueCountRef = i,
                    queueLostCountRef = l,
                    enqueuedSource = s1,
                    enqueuedButLostSource = s2,
                    dequeuedSource = s3 }
  
-- | Test whether the queue is empty.
queueNull :: Queue si qi sm qm so qo a -> Event Bool
queueNull q =
  Event $ \p ->
  do n <- readIORef (queueCountRef q)
     return (n == 0)

-- | Test whether the queue is full.
queueFull :: Queue si qi sm qm so qo a -> Event Bool
queueFull q =
  Event $ \p ->
  do n <- readIORef (queueCountRef q)
     return (n == queueMaxCount q)

-- | Return the queue size.
queueCount :: Queue si qi sm qm so qo a -> Event Int
queueCount q =
  Event $ \p -> readIORef (queueCountRef q)
  
-- | Return the number of lost items.
queueLostCount :: Queue si qi sm qm so qo a -> Event Int
queueLostCount q =
  Event $ \p -> readIORef (queueLostCountRef q)
  
-- | Dequeue suspending the process if the queue is empty.
dequeue :: (DequeueStrategy si qi,
            DequeueStrategy sm qm,
            EnqueueStrategy so qo)
           => Queue si qi sm qm so qo a
           -- ^ the queue
           -> Process a
           -- ^ the dequeued value
dequeue q =
  do requestResource (queueOutputRes q)
     a <- liftEvent $
          strategyDequeue (queueMemoryStrategy q) (queueMemory q)
     releaseResource (queueInputRes q)
     liftEvent $
       triggerSignal (dequeuedSource q) a
     return a
  
-- | Dequeue with the priority suspending the process if the queue is empty.
dequeueWithPriority :: (DequeueStrategy si qi,
                        DequeueStrategy sm qm,
                        PriorityQueueStrategy so qo)
                       => Queue si qi sm qm so qo a
                       -- ^ the queue
                       -> Double
                       -- ^ the priority
                       -> Process a
                       -- ^ the dequeued value
dequeueWithPriority q priority =
  do requestResourceWithPriority (queueOutputRes q) priority
     a <- liftEvent $
          strategyDequeue (queueMemoryStrategy q) (queueMemory q)
     releaseResource (queueInputRes q)
     liftEvent $
       triggerSignal (dequeuedSource q) a
     return a
  
-- | Dequeue with the dynamic priority suspending the process if the queue is empty.
dequeueWithDynamicPriority :: (DequeueStrategy si qi,
                               DequeueStrategy sm qm,
                               DynamicPriorityQueueStrategy so qo)
                              => Queue si qi sm qm so qo a
                              -- ^ the queue
                              -> Event Double
                              -- ^ the dynamic priority
                              -> Process a
                              -- ^ the dequeued value
dequeueWithDynamicPriority q priority =
  do requestResourceWithDynamicPriority (queueOutputRes q) priority
     a <- liftEvent $
          strategyDequeue (queueMemoryStrategy q) (queueMemory q)
     releaseResource (queueInputRes q)
     liftEvent $
       triggerSignal (dequeuedSource q) a
     return a
  
-- | Try to dequeue from the queue immediately.  
tryDequeue :: (DequeueStrategy si qi,
               DequeueStrategy sm qm)
              => Queue si qi sm qm so qo a
              -- ^ the queue
              -> Event (Maybe a)
              -- ^ the dequeued value of 'Nothing'
tryDequeue q =
  do x <- tryRequestResourceWithinEvent (queueOutputRes q)
     if x 
       then do a <- strategyDequeue (queueMemoryStrategy q) (queueMemory q)
               releaseResourceWithinEvent (queueInputRes q)
               triggerSignal (dequeuedSource q) a
               return $ Just a
       else return Nothing

-- | Enqueue the item suspending the process if the queue is full.  
enqueue :: (EnqueueStrategy si qi,
            EnqueueStrategy sm qm,
            DequeueStrategy so qo)
           => Queue si qi sm qm so qo a
           -- ^ the queue
           -> a
           -- ^ the item to enqueue
           -> Process ()
enqueue q a =
  do requestResource (queueInputRes q)
     liftEvent $
       strategyEnqueue (queueMemoryStrategy q) (queueMemory q) a
     releaseResource (queueOutputRes q)
     liftEvent $
       triggerSignal (enqueuedSource q) a
     
-- | Enqueue with the priority the item suspending the process if the queue is full.  
enqueueWithPriority :: (PriorityQueueStrategy si qi,
                        EnqueueStrategy sm qm,
                        DequeueStrategy so qo)
                       => Queue si qi sm qm so qo a
                       -- ^ the queue
                       -> Double
                       -- ^ the priority
                       -> a
                       -- ^ the item to enqueue
                       -> Process ()
enqueueWithPriority q priority a =
  do requestResourceWithPriority (queueInputRes q) priority
     liftEvent $
       strategyEnqueue (queueMemoryStrategy q) (queueMemory q) a
     releaseResource (queueOutputRes q)
     liftEvent $
       triggerSignal (enqueuedSource q) a
     
-- | Enqueue with the dynamic priority the item suspending the process if the queue is full.  
enqueueWithDynamicPriority :: (DynamicPriorityQueueStrategy si qi,
                               EnqueueStrategy sm qm,
                               DequeueStrategy so qo)
                              => Queue si qi sm qm so qo a
                              -- ^ the queue
                              -> Event Double
                              -- ^ the dynamic priority
                              -> a
                              -- ^ the item to enqueue
                              -> Process ()
enqueueWithDynamicPriority q priority a =
  do requestResourceWithDynamicPriority (queueInputRes q) priority
     liftEvent $
       strategyEnqueue (queueMemoryStrategy q) (queueMemory q) a
     releaseResource (queueOutputRes q)
     liftEvent $
       triggerSignal (enqueuedSource q) a
     
-- | Try to enqueue the item. Return 'False' in the monad if the queue is full.
tryEnqueue :: (EnqueueStrategy sm qm,
               DequeueStrategy so qo)
              => Queue si qi sm qm so qo a
              -- ^ the queue
              -> a
              -- ^ the item which we try to enqueue
              -> Event Bool
tryEnqueue q a =
  do x <- tryRequestResourceWithinEvent (queueInputRes q)
     if x 
       then do strategyEnqueue (queueMemoryStrategy q) (queueMemory q) a
               releaseResourceWithinEvent (queueOutputRes q)
               triggerSignal (enqueuedSource q) a
               return True
       else return False

-- | Try to enqueue the item. If the queue is full then the item will be lost
-- and 'False' will be returned.
enqueueOrLost :: (EnqueueStrategy sm qm,
                  DequeueStrategy so qo)
                 => Queue si qi sm qm so qo a
                 -- ^ the queue
                 -> a
                 -- ^ the item which we try to enqueue
                 -> Event Bool
enqueueOrLost q a =
  do x <- tryRequestResourceWithinEvent (queueInputRes q)
     if x
       then do strategyEnqueue (queueMemoryStrategy q) (queueMemory q) a
               releaseResourceWithinEvent (queueOutputRes q)
               triggerSignal (enqueuedSource q) a
               return True
       else do liftIO $ modifyIORef (queueLostCountRef q) $ (+) 1
               triggerSignal (enqueuedButLostSource q) a
               return False

-- | Try to enqueue the item. If the queue is full then the item will be lost.
enqueueOrLost_ :: (EnqueueStrategy sm qm,
                   DequeueStrategy so qo)
                  => Queue si qi sm qm so qo a
                  -- ^ the queue
                  -> a
                  -- ^ the item which we try to enqueue
                  -> Event ()
enqueueOrLost_ q a =
  do x <- enqueueOrLost q a
     return ()

-- | Return a signal that notifies when any item is enqueued.
enqueued :: Queue si qi sm qm so qo a -> Signal a
enqueued q = publishSignal (enqueuedSource q)

-- | Return a signal which notifies that the item was lost when 
-- attempting to add it to the full queue with help of
-- 'enqueueOrLost' or 'enqueueOrLost_'.
enqueuedButLost :: Queue si qi sm qm so qo a -> Signal a
enqueuedButLost q = publishSignal (enqueuedButLostSource q)

-- | Return a signal that notifies when any item is dequeued.
dequeued :: Queue si qi sm qm so qo a -> Signal a
dequeued q = publishSignal (dequeuedSource q)
