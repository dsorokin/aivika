
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
-- 4 x 4 x 4 = 64 different types of the queue, each of them will have its own
-- behaviour.
--
module Simulation.Aivika.Queue
       (Queue,
        queueNull,
        queueFull,
        queueInputStrategy,
        queueStoringStrategy,
        queueOutputStrategy,
        queueMaxCount,
        queueCount,
        queueLostCount,
        enqueued,
        dequeued,
        enqueuedButLost,
        newQueue,
        dequeue,
        dequeueWithOutputPriority,
        tryDequeue,
        enqueue,
        enqueueWithInputPriority,
        enqueueWithStoringPriority,
        enqueueWithInputStoringPriorities,
        tryEnqueue,
        tryEnqueueWithStoringPriority,
        enqueueOrLost,
        enqueueOrLost_,
        enqueueWithStoringPriorityOrLost,
        enqueueWithStoringPriorityOrLost_) where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
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
          -- ^ The strategy applied to the input (enqueuing) process.
          queueStoringStrategy :: sm,
          -- ^ The strategy applied when storing (in memory) items in the queue.
          queueOutputStrategy :: so,
          -- ^ The strategy applied to the output (dequeuing) process.
          queueInputRes :: Resource si qi,
          queueStore :: qm (QueueItem a),
          queueOutputRes :: Resource so qo,
          queueCountRef :: IORef Int,
          queueLostCountRef :: IORef Int,
          queueInputCountRef :: IORef Int,
          queueOutputCountRef :: IORef Int,
          enqueuedSource :: SignalSource a,
          enqueuedButLostSource :: SignalSource a,
          dequeuedSource :: SignalSource a }

-- | Stores the item and a time of its enqueuing. 
data QueueItem a =
  QueueItem { itemValue :: a,
              -- ^ Return the item value.
              itemInputTime :: Double
              -- ^ Return the time of enqueuing the item.
            }
  
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
     ci <- liftIO $ newIORef 0
     co <- liftIO $ newIORef 0
     ri <- newResourceWithCount si count count
     qm <- newStrategyQueue sm
     ro <- newResourceWithCount so count 0
     s1 <- newSignalSource
     s2 <- newSignalSource
     s3 <- newSignalSource
     return Queue { queueMaxCount = count,
                    queueInputStrategy = si,
                    queueStoringStrategy = sm,
                    queueOutputStrategy = so,
                    queueInputRes = ri,
                    queueStore = qm,
                    queueOutputRes = ro,
                    queueCountRef = i,
                    queueLostCountRef = l,
                    queueInputCountRef = ci,
                    queueOutputCountRef = co,
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
     i <- liftEvent $
          strategyDequeue (queueStoringStrategy q) (queueStore q)
     liftIO $ modifyIORef (queueOutputCountRef q) (+ 1)
     releaseResource (queueInputRes q)
     liftEvent $
       triggerSignal (dequeuedSource q) (itemValue i)
     return (itemValue i)
  
-- | Dequeue with the output priority suspending the process if the queue is empty.
dequeueWithOutputPriority :: (DequeueStrategy si qi,
                              DequeueStrategy sm qm,
                              PriorityQueueStrategy so qo po)
                             => Queue si qi sm qm so qo a
                             -- ^ the queue
                             -> po
                             -- ^ the priority for output
                             -> Process a
                             -- ^ the dequeued value
dequeueWithOutputPriority q priority =
  do requestResourceWithPriority (queueOutputRes q) priority
     i <- liftEvent $
          strategyDequeue (queueStoringStrategy q) (queueStore q)
     liftIO $ modifyIORef (queueOutputCountRef q) (+ 1)
     releaseResource (queueInputRes q)
     liftEvent $
       triggerSignal (dequeuedSource q) (itemValue i)
     return (itemValue i)
  
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
       then do i <- strategyDequeue (queueStoringStrategy q) (queueStore q)
               liftIO $ modifyIORef (queueOutputCountRef q) (+ 1)
               releaseResourceWithinEvent (queueInputRes q)
               triggerSignal (dequeuedSource q) (itemValue i)
               return $ Just (itemValue i)
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
  do t <- liftDynamics time
     let i = QueueItem { itemValue = a,
                         itemInputTime = t }
     requestResource (queueInputRes q)
     liftEvent $
       strategyEnqueue (queueStoringStrategy q) (queueStore q) i
     liftIO $ modifyIORef (queueInputCountRef q) (+ 1)
     releaseResource (queueOutputRes q)
     liftEvent $
       triggerSignal (enqueuedSource q) a
     
-- | Enqueue with the input priority the item suspending the process if the queue is full.  
enqueueWithInputPriority :: (PriorityQueueStrategy si qi pi,
                             EnqueueStrategy sm qm,
                             DequeueStrategy so qo)
                            => Queue si qi sm qm so qo a
                            -- ^ the queue
                            -> pi
                            -- ^ the priority for input
                            -> a
                            -- ^ the item to enqueue
                            -> Process ()
enqueueWithInputPriority q priority a =
  do t <- liftDynamics time
     let i = QueueItem { itemValue = a,
                         itemInputTime = t }
     requestResourceWithPriority (queueInputRes q) priority
     liftEvent $
       strategyEnqueue (queueStoringStrategy q) (queueStore q) i
     liftIO $ modifyIORef (queueInputCountRef q) (+ 1)
     releaseResource (queueOutputRes q)
     liftEvent $
       triggerSignal (enqueuedSource q) a
     
-- | Enqueue with the storing priority the item suspending the process if the queue is full.  
enqueueWithStoringPriority :: (EnqueueStrategy si qi,
                               PriorityQueueStrategy sm qm pm,
                               DequeueStrategy so qo)
                              => Queue si qi sm qm so qo a
                              -- ^ the queue
                              -> pm
                              -- ^ the priority for storing
                              -> a
                              -- ^ the item to enqueue
                              -> Process ()
enqueueWithStoringPriority q priority a =
  do t <- liftDynamics time
     let i = QueueItem { itemValue = a,
                         itemInputTime = t }
     requestResource (queueInputRes q)
     liftEvent $
       strategyEnqueueWithPriority (queueStoringStrategy q) (queueStore q) priority i
     liftIO $ modifyIORef (queueInputCountRef q) (+ 1)
     releaseResource (queueOutputRes q)
     liftEvent $
       triggerSignal (enqueuedSource q) a
     
-- | Enqueue with the input and storing priorities the item suspending the process if the queue is full.  
enqueueWithInputStoringPriorities :: (PriorityQueueStrategy si qi pi,
                                      PriorityQueueStrategy sm qm pm,
                                      DequeueStrategy so qo)
                                     => Queue si qi sm qm so qo a
                                     -- ^ the queue
                                     -> pi
                                     -- ^ the priority for input
                                     -> pm
                                     -- ^ the priority for storing
                                     -> a
                                     -- ^ the item to enqueue
                                     -> Process ()
enqueueWithInputStoringPriorities q pi pm a =
  do t <- liftDynamics time
     let i = QueueItem { itemValue = a,
                         itemInputTime = t }
     requestResourceWithPriority (queueInputRes q) pi
     liftEvent $
       strategyEnqueueWithPriority (queueStoringStrategy q) (queueStore q) pm i
     liftIO $ modifyIORef (queueInputCountRef q) (+ 1)
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
       then do t <- liftDynamics time
               let i = QueueItem { itemValue = a,
                                   itemInputTime = t }
               strategyEnqueue (queueStoringStrategy q) (queueStore q) i
               liftIO $ modifyIORef (queueInputCountRef q) (+ 1)
               releaseResourceWithinEvent (queueOutputRes q)
               triggerSignal (enqueuedSource q) a
               return True
       else return False

-- | Try to enqueue with the storing priority the item. Return 'False' in the monad if the queue is full.
tryEnqueueWithStoringPriority :: (PriorityQueueStrategy sm qm pm,
                                  DequeueStrategy so qo)
                                 => Queue si qi sm qm so qo a
                                 -- ^ the queue
                                 -> pm
                                 -- ^ the priority for storing
                                 -> a
                                 -- ^ the item which we try to enqueue
                                 -> Event Bool
tryEnqueueWithStoringPriority q pm a =
  do x <- tryRequestResourceWithinEvent (queueInputRes q)
     if x 
       then do t <- liftDynamics time
               let i = QueueItem { itemValue = a,
                                   itemInputTime = t }
               strategyEnqueueWithPriority (queueStoringStrategy q) (queueStore q) pm i
               liftIO $ modifyIORef (queueInputCountRef q) (+ 1)
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
       then do t <- liftDynamics time
               let i = QueueItem { itemValue = a,
                                   itemInputTime = t }
               strategyEnqueue (queueStoringStrategy q) (queueStore q) i
               liftIO $ modifyIORef (queueInputCountRef q) (+ 1)
               releaseResourceWithinEvent (queueOutputRes q)
               triggerSignal (enqueuedSource q) a
               return True
       else do liftIO $ modifyIORef (queueLostCountRef q) $ (+) 1
               triggerSignal (enqueuedButLostSource q) a
               return False

-- | Try to enqueue with the storing priority the item. If the queue is full then the item will be lost
-- and 'False' will be returned.
enqueueWithStoringPriorityOrLost :: (PriorityQueueStrategy sm qm pm,
                                     DequeueStrategy so qo)
                                    => Queue si qi sm qm so qo a
                                    -- ^ the queue
                                    -> pm
                                    -- ^ the priority for storing
                                    -> a
                                    -- ^ the item which we try to enqueue
                                    -> Event Bool
enqueueWithStoringPriorityOrLost q pm a =
  do x <- tryRequestResourceWithinEvent (queueInputRes q)
     if x
       then do t <- liftDynamics time
               let i = QueueItem { itemValue = a,
                                   itemInputTime = t }
               strategyEnqueueWithPriority (queueStoringStrategy q) (queueStore q) pm i
               liftIO $ modifyIORef (queueInputCountRef q) (+ 1)
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

-- | Try to enqueue with the storing priority the item. If the queue is full then the item will be lost.
enqueueWithStoringPriorityOrLost_ :: (PriorityQueueStrategy sm qm pm,
                                      DequeueStrategy so qo)
                                     => Queue si qi sm qm so qo a
                                     -- ^ the queue
                                     -> pm
                                     -- ^ the priority for storing
                                     -> a
                                     -- ^ the item which we try to enqueue
                                     -> Event ()
enqueueWithStoringPriorityOrLost_ q pm a =
  do x <- enqueueWithStoringPriorityOrLost q pm a
     return ()

-- | Return a signal that notifies when any item is enqueued.
enqueued :: Queue si qi sm qm so qo a -> Signal a
enqueued q = publishSignal (enqueuedSource q)

-- | Return a signal which notifies that the item was lost when 
-- attempting to add it to the full queue with help of
-- 'enqueueOrLost', 'enqueueOrLost_' or similar functions that imply
-- that the element can be lost.
enqueuedButLost :: Queue si qi sm qm so qo a -> Signal a
enqueuedButLost q = publishSignal (enqueuedButLostSource q)

-- | Return a signal that notifies when any item is dequeued.
dequeued :: Queue si qi sm qm so qo a -> Signal a
dequeued q = publishSignal (dequeuedSource q)
