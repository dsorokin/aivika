
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Queue.Infinite
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines an infinite queue that can use the specified strategies.
--
module Simulation.Aivika.Trans.Queue.Infinite
       (-- * Queue Types
        FCFSQueue,
        LCFSQueue,
        SIROQueue,
        PriorityQueue,
        Queue,
        -- * Creating Queue
        newFCFSQueue,
        newLCFSQueue,
        newSIROQueue,
        newPriorityQueue,
        newQueue,
        -- * Queue Properties and Activities
        enqueueStoringStrategy,
        dequeueStrategy,
        queueNull,
        queueCount,
        queueCountStats,
        enqueueStoreCount,
        dequeueCount,
        dequeueExtractCount,
        enqueueStoreRate,
        dequeueRate,
        dequeueExtractRate,
        queueWaitTime,
        dequeueWaitTime,
        queueRate,
        -- * Dequeuing and Enqueuing
        dequeue,
        dequeueWithOutputPriority,
        tryDequeue,
        enqueue,
        enqueueWithStoringPriority,
        -- * Summary
        queueSummary,
        -- * Derived Signals for Properties
        queueNullChanged,
        queueNullChanged_,
        queueCountChanged,
        queueCountChanged_,
        enqueueStoreCountChanged,
        enqueueStoreCountChanged_,
        dequeueCountChanged,
        dequeueCountChanged_,
        dequeueExtractCountChanged,
        dequeueExtractCountChanged_,
        queueWaitTimeChanged,
        queueWaitTimeChanged_,
        dequeueWaitTimeChanged,
        dequeueWaitTimeChanged_,
        queueRateChanged,
        queueRateChanged_,
        -- * Basic Signals
        enqueueStored,
        dequeueRequested,
        dequeueExtracted,
        -- * Overall Signal
        queueChanged_) where

import Data.IORef
import Data.Monoid

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Process
import Simulation.Aivika.Trans.Internal.Signal
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Resource
import Simulation.Aivika.Trans.QueueStrategy
import Simulation.Aivika.Trans.Statistics

-- | A type synonym for the ordinary FIFO queue also known as the FCFS
-- (First Come - First Serviced) queue.
type FCFSQueue m a = Queue m FCFS FCFS a

-- | A type synonym for the ordinary LIFO queue also known as the LCFS
-- (Last Come - First Serviced) queue.
type LCFSQueue m a = Queue m LCFS FCFS a

-- | A type synonym for the SIRO (Serviced in Random Order) queue.
type SIROQueue m a = Queue m SIRO FCFS a

-- | A type synonym for the queue with static priorities applied when
-- storing the elements in the queue.
type PriorityQueue m a = Queue m StaticPriorities FCFS a

-- | Represents an infinite queue using the specified strategies for
-- internal storing (in memory), @sm@, and dequeueing (output), @so@, where @a@ denotes
-- the type of items stored in the queue. As usual, type @m@ denotes
-- the underlying computation within which the simulation executes.
data Queue m sm so a =
  Queue { enqueueStoringStrategy :: sm,
          -- ^ The strategy applied when storing (in memory) items in the queue.
          dequeueStrategy :: so,
          -- ^ The strategy applied to the dequeueing (output) processes.
          queueStore :: StrategyQueue m sm (QueueItem a),
          dequeueRes :: Resource m so,
          queueCountRef :: ProtoRef m Int,
          queueCountStatsRef :: ProtoRef m (TimingStats Int),
          enqueueStoreCountRef :: ProtoRef m Int,
          dequeueCountRef :: ProtoRef m Int,
          dequeueExtractCountRef :: ProtoRef m Int,
          queueWaitTimeRef :: ProtoRef m (SamplingStats Double),
          dequeueWaitTimeRef :: ProtoRef m (SamplingStats Double),
          enqueueStoredSource :: SignalSource m a,
          dequeueRequestedSource :: SignalSource m (),
          dequeueExtractedSource :: SignalSource m a }

-- | Stores the item and a time of its enqueuing. 
data QueueItem a =
  QueueItem { itemValue :: a,
              -- ^ Return the item value.
              itemStoringTime :: Double
              -- ^ Return the time of storing in the queue.
            }
  
-- | Create a new infinite FCFS queue.  
newFCFSQueue :: Comp m => Event m (FCFSQueue m a)
{-# INLINABLE newFCFSQueue #-}
{-# SPECIALISE newFCFSQueue :: Event IO (FCFSQueue IO a) #-}
newFCFSQueue = newQueue FCFS FCFS
  
-- | Create a new infinite LCFS queue.  
newLCFSQueue :: Comp m => Event m (LCFSQueue m a)  
{-# INLINABLE newLCFSQueue #-}
{-# SPECIALISE newLCFSQueue :: Event IO (LCFSQueue IO a) #-}
newLCFSQueue = newQueue LCFS FCFS
  
-- | Create a new infinite SIRO queue.  
newSIROQueue :: Comp m => Event m (SIROQueue m a)  
{-# INLINABLE newSIROQueue #-}
{-# SPECIALISE newSIROQueue :: Event IO (SIROQueue IO a) #-}
newSIROQueue = newQueue SIRO FCFS
  
-- | Create a new infinite priority queue.  
newPriorityQueue :: Comp m => Event m (PriorityQueue m a)  
{-# INLINABLE newPriorityQueue #-}
{-# SPECIALISE newPriorityQueue :: Event IO (PriorityQueue IO a) #-}
newPriorityQueue = newQueue StaticPriorities FCFS
  
-- | Create a new infinite queue with the specified strategies.  
newQueue :: (Comp m,
             QueueStrategy m sm,
             QueueStrategy m so) =>
            sm
            -- ^ the strategy applied when storing items in the queue
            -> so
            -- ^ the strategy applied to the dequeueing (output) processes when the queue is empty
            -> Event m (Queue m sm so a)  
{-# INLINABLE newQueue #-}
{-# SPECIALISE newQueue :: (QueueStrategy IO sm, QueueStrategy IO so) => sm -> so -> Event IO (Queue IO sm so a) #-}
newQueue sm so =
  do t  <- liftDynamics time
     sn <- liftParameter simulationSession 
     i  <- liftComp $ newProtoRef sn 0
     is <- liftComp $ newProtoRef sn $ returnTimingStats t 0
     cm <- liftComp $ newProtoRef sn 0
     cr <- liftComp $ newProtoRef sn 0
     co <- liftComp $ newProtoRef sn 0
     qm <- liftSimulation $ newStrategyQueue sm
     ro <- liftSimulation $ newResourceWithMaxCount so 0 Nothing
     w  <- liftComp $ newProtoRef sn mempty
     wo <- liftComp $ newProtoRef sn mempty 
     s3 <- liftSimulation newSignalSource
     s4 <- liftSimulation newSignalSource
     s5 <- liftSimulation newSignalSource
     return Queue { enqueueStoringStrategy = sm,
                    dequeueStrategy = so,
                    queueStore = qm,
                    dequeueRes = ro,
                    queueCountRef = i,
                    queueCountStatsRef = is,
                    enqueueStoreCountRef = cm,
                    dequeueCountRef = cr,
                    dequeueExtractCountRef = co,
                    queueWaitTimeRef = w,
                    dequeueWaitTimeRef = wo,
                    enqueueStoredSource = s3,
                    dequeueRequestedSource = s4,
                    dequeueExtractedSource = s5 }

-- | Test whether the queue is empty.
--
-- See also 'queueNullChanged' and 'queueNullChanged_'.
queueNull :: Comp m => Queue m sm so a -> Event m Bool
{-# INLINABLE queueNull #-}
{-# SPECIALISE queueNull :: Queue IO sm so a -> Event IO Bool #-}
queueNull q =
  Event $ \p ->
  do n <- readProtoRef (queueCountRef q)
     return (n == 0)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged :: Comp m => Queue m sm so a -> Signal m Bool
{-# INLINABLE queueNullChanged #-}
{-# SPECIALISE queueNullChanged :: Queue IO sm so a -> Signal IO Bool #-}
queueNullChanged q =
  mapSignalM (const $ queueNull q) (queueNullChanged_ q)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged_ :: Comp m => Queue m sm so a -> Signal m ()
{-# INLINABLE queueNullChanged_ #-}
{-# SPECIALISE queueNullChanged_ :: Queue IO sm so a -> Signal IO () #-}
queueNullChanged_ = queueCountChanged_

-- | Return the current queue size.
--
-- See also 'queueCountStats', 'queueCountChanged' and 'queueCountChanged_'.
queueCount :: Comp m => Queue m sm so a -> Event m Int
{-# INLINABLE queueCount #-}
{-# SPECIALISE queueCount :: Queue IO sm so a -> Event IO Int #-}
queueCount q =
  Event $ \p -> readProtoRef (queueCountRef q)

-- | Return the queue size statistics.
queueCountStats :: Comp m => Queue m sm so a -> Event m (TimingStats Int)
{-# INLINABLE queueCountStats #-}
{-# SPECIALISE queueCountStats :: Queue IO sm so a -> Event IO (TimingStats Int) #-}
queueCountStats q =
  Event $ \p -> readProtoRef (queueCountStatsRef q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged :: Comp m => Queue m sm so a -> Signal m Int
{-# INLINABLE queueCountChanged #-}
{-# SPECIALISE queueCountChanged :: Queue IO sm so a -> Signal IO Int #-}
queueCountChanged q =
  mapSignalM (const $ queueCount q) (queueCountChanged_ q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged_ :: Comp m => Queue m sm so a -> Signal m ()
{-# INLINABLE queueCountChanged_ #-}
{-# SPECIALISE queueCountChanged_ :: Queue IO sm so a -> Signal IO () #-}
queueCountChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the total number of input items that were stored.
--
-- See also 'enqueueStoreCountChanged' and 'enqueueStoreCountChanged_'.
enqueueStoreCount :: Comp m => Queue m sm so a -> Event m Int
{-# INLINABLE enqueueStoreCount #-}
{-# SPECIALISE enqueueStoreCount :: Queue IO sm so a -> Event IO Int #-}
enqueueStoreCount q =
  Event $ \p -> readProtoRef (enqueueStoreCountRef q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged :: Comp m => Queue m sm so a -> Signal m Int
{-# INLINABLE enqueueStoreCountChanged #-}
{-# SPECIALISE enqueueStoreCountChanged :: Queue IO sm so a -> Signal IO Int #-}
enqueueStoreCountChanged q =
  mapSignalM (const $ enqueueStoreCount q) (enqueueStoreCountChanged_ q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged_ :: Comp m => Queue m sm so a -> Signal m ()
{-# INLINABLE enqueueStoreCountChanged_ #-}
{-# SPECIALISE enqueueStoreCountChanged_ :: Queue IO sm so a -> Signal IO () #-}
enqueueStoreCountChanged_ q =
  mapSignal (const ()) (enqueueStored q)
      
-- | Return the total number of requests for dequeueing the items,
-- not taking into account the failed attempts to dequeue immediately
-- without suspension.
--
-- See also 'dequeueCountChanged' and 'dequeueCountChanged_'.
dequeueCount :: Comp m => Queue m sm so a -> Event m Int
{-# INLINABLE dequeueCount #-}
{-# SPECIALISE dequeueCount :: Queue IO sm so a -> Event IO Int #-}
dequeueCount q =
  Event $ \p -> readProtoRef (dequeueCountRef q)
      
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged :: Comp m => Queue m sm so a -> Signal m Int
{-# INLINABLE dequeueCountChanged #-}
{-# SPECIALISE dequeueCountChanged :: Queue IO sm so a -> Signal IO Int #-}
dequeueCountChanged q =
  mapSignalM (const $ dequeueCount q) (dequeueCountChanged_ q)
  
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged_ :: Comp m => Queue m sm so a -> Signal m ()
{-# INLINABLE dequeueCountChanged_ #-}
{-# SPECIALISE dequeueCountChanged_ :: Queue IO sm so a -> Signal IO () #-}
dequeueCountChanged_ q =
  mapSignal (const ()) (dequeueRequested q)
      
-- | Return the total number of output items that were actually dequeued.
--
-- See also 'dequeueExtractCountChanged' and 'dequeueExtractCountChanged_'.
dequeueExtractCount :: Comp m => Queue m sm so a -> Event m Int
{-# INLINABLE dequeueExtractCount #-}
{-# SPECIALISE dequeueExtractCount :: Queue IO sm so a -> Event IO Int #-}
dequeueExtractCount q =
  Event $ \p -> readProtoRef (dequeueExtractCountRef q)
      
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged :: Comp m => Queue m sm so a -> Signal m Int
{-# INLINABLE dequeueExtractCountChanged #-}
{-# SPECIALISE dequeueExtractCountChanged :: Queue IO sm so a -> Signal IO Int #-}
dequeueExtractCountChanged q =
  mapSignalM (const $ dequeueExtractCount q) (dequeueExtractCountChanged_ q)
  
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged_ :: Comp m => Queue m sm so a -> Signal m ()
{-# INLINABLE dequeueExtractCountChanged_ #-}
{-# SPECIALISE dequeueExtractCountChanged_ :: Queue IO sm so a -> Signal IO () #-}
dequeueExtractCountChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the rate of the items that were stored: how many items
-- per time.
enqueueStoreRate :: Comp m => Queue m sm so a -> Event m Double
{-# INLINABLE enqueueStoreRate #-}
{-# SPECIALISE enqueueStoreRate :: Queue IO sm so a -> Event IO Double #-}
enqueueStoreRate q =
  Event $ \p ->
  do x <- readProtoRef (enqueueStoreCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the requests for dequeueing the items: how many requests
-- per time. It does not include the failed attempts to dequeue immediately
-- without suspension.
dequeueRate :: Comp m => Queue m sm so a -> Event m Double
{-# INLINABLE dequeueRate #-}
{-# SPECIALISE dequeueRate :: Queue IO sm so a -> Event IO Double #-}
dequeueRate q =
  Event $ \p ->
  do x <- readProtoRef (dequeueCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the output items that were dequeued: how many items
-- per time.
dequeueExtractRate :: Comp m => Queue m sm so a -> Event m Double
{-# INLINABLE dequeueExtractRate #-}
{-# SPECIALISE dequeueExtractRate :: Queue IO sm so a -> Event IO Double #-}
dequeueExtractRate q =
  Event $ \p ->
  do x <- readProtoRef (dequeueExtractCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the wait time from the time at which the item was stored in the queue to
-- the time at which it was dequeued.
--
-- See also 'queueWaitTimeChanged' and 'queueWaitTimeChanged_'.
queueWaitTime :: Comp m => Queue m sm so a -> Event m (SamplingStats Double)
{-# INLINABLE queueWaitTime #-}
{-# SPECIALISE queueWaitTime :: Queue IO sm so a -> Event IO (SamplingStats Double) #-}
queueWaitTime q =
  Event $ \p -> readProtoRef (queueWaitTimeRef q)
      
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged :: Comp m => Queue m sm so a -> Signal m (SamplingStats Double)
{-# INLINABLE queueWaitTimeChanged #-}
{-# SPECIALISE queueWaitTimeChanged :: Queue IO sm so a -> Signal IO (SamplingStats Double) #-}
queueWaitTimeChanged q =
  mapSignalM (const $ queueWaitTime q) (queueWaitTimeChanged_ q)
  
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged_ :: Comp m => Queue m sm so a -> Signal m ()
{-# INLINABLE queueWaitTimeChanged_ #-}
{-# SPECIALISE queueWaitTimeChanged_ :: Queue IO sm so a -> Signal IO () #-}
queueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the dequeue wait time from the time at which the item was requested
-- for dequeueing to the time at which it was actually dequeued.
--
-- See also 'dequeueWaitTimeChanged' and 'dequeueWaitTimeChanged_'.
dequeueWaitTime :: Comp m => Queue m sm so a -> Event m (SamplingStats Double)
{-# INLINABLE dequeueWaitTime #-}
{-# SPECIALISE dequeueWaitTime :: Queue IO sm so a -> Event IO (SamplingStats Double) #-}
dequeueWaitTime q =
  Event $ \p -> readProtoRef (dequeueWaitTimeRef q)
      
-- | Signal when the 'dequeueWaitTime' property value has changed.
dequeueWaitTimeChanged :: Comp m => Queue m sm so a -> Signal m (SamplingStats Double)
{-# INLINABLE dequeueWaitTimeChanged #-}
{-# SPECIALISE dequeueWaitTimeChanged :: Queue IO sm so a -> Signal IO (SamplingStats Double) #-}
dequeueWaitTimeChanged q =
  mapSignalM (const $ dequeueWaitTime q) (dequeueWaitTimeChanged_ q)
  
-- | Signal when the 'dequeueWaitTime' property value has changed.
dequeueWaitTimeChanged_ :: Comp m => Queue m sm so a -> Signal m ()
{-# INLINABLE dequeueWaitTimeChanged_ #-}
{-# SPECIALISE dequeueWaitTimeChanged_ :: Queue IO sm so a -> Signal IO () #-}
dequeueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)

-- | Return a long-term average queue rate calculated as
-- the average queue size divided by the average wait time.
--
-- See also 'queueRateChanged' and 'queueRateChanged_'.
queueRate :: Comp m => Queue m sm so a -> Event m Double
{-# INLINABLE queueRate #-}
{-# SPECIALISE queueRate :: Queue IO sm so a -> Event IO Double #-}
queueRate q =
  Event $ \p ->
  do x <- readProtoRef (queueCountStatsRef q)
     y <- readProtoRef (queueWaitTimeRef q)
     return (timingStatsMean x / samplingStatsMean y) 

-- | Signal when the 'queueRate' property value has changed.
queueRateChanged :: Comp m => Queue m sm so a -> Signal m Double
{-# INLINABLE queueRateChanged #-}
{-# SPECIALISE queueRateChanged :: Queue IO sm so a -> Signal IO Double #-}
queueRateChanged q =
  mapSignalM (const $ queueRate q) (queueRateChanged_ q)

-- | Signal when the 'queueRate' property value has changed.
queueRateChanged_ :: Comp m => Queue m sm so a -> Signal m ()
{-# INLINABLE queueRateChanged_ #-}
{-# SPECIALISE queueRateChanged_ :: Queue IO sm so a -> Signal IO () #-}
queueRateChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)
  
-- | Dequeue suspending the process if the queue is empty.
dequeue :: (Comp m,
            DequeueStrategy m sm,
            EnqueueStrategy m so)
           => Queue m sm so a
           -- ^ the queue
           -> Process m a
           -- ^ the dequeued value
{-# INLINABLE dequeue #-}
{-# SPECIALISE dequeue :: (DequeueStrategy IO sm, EnqueueStrategy IO so) => Queue IO sm so a -> Process IO a #-}
dequeue q =
  do t <- liftEvent $ dequeueRequest q
     requestResource (dequeueRes q)
     liftEvent $ dequeueExtract q t
  
-- | Dequeue with the output priority suspending the process if the queue is empty.
dequeueWithOutputPriority :: (Comp m,
                              DequeueStrategy m sm,
                              PriorityQueueStrategy m so po)
                             => Queue m sm so a
                             -- ^ the queue
                             -> po
                             -- ^ the priority for output
                             -> Process m a
                             -- ^ the dequeued value
{-# INLINABLE dequeueWithOutputPriority #-}
{-# SPECIALISE dequeueWithOutputPriority :: (DequeueStrategy IO sm, PriorityQueueStrategy IO so po) => Queue IO sm so a -> po -> Process IO a #-}
dequeueWithOutputPriority q po =
  do t <- liftEvent $ dequeueRequest q
     requestResourceWithPriority (dequeueRes q) po
     liftEvent $ dequeueExtract q t
  
-- | Try to dequeue immediately.
tryDequeue :: (Comp m, DequeueStrategy m sm)
              => Queue m sm so a
              -- ^ the queue
              -> Event m (Maybe a)
              -- ^ the dequeued value of 'Nothing'
{-# INLINABLE tryDequeue #-}
{-# SPECIALISE tryDequeue :: DequeueStrategy IO sm => Queue IO sm so a -> Event IO (Maybe a) #-}
tryDequeue q =
  do x <- tryRequestResourceWithinEvent (dequeueRes q)
     if x 
       then do t <- dequeueRequest q
               fmap Just $ dequeueExtract q t
       else return Nothing

-- | Enqueue the item.  
enqueue :: (Comp m,
            EnqueueStrategy m sm,
            DequeueStrategy m so)
           => Queue m sm so a
           -- ^ the queue
           -> a
           -- ^ the item to enqueue
           -> Event m ()
{-# INLINABLE enqueue #-}
{-# SPECIALISE enqueue :: (EnqueueStrategy IO sm, DequeueStrategy IO so) => Queue IO sm so a -> a -> Event IO () #-}
enqueue = enqueueStore
     
-- | Enqueue with the storing priority the item.  
enqueueWithStoringPriority :: (Comp m,
                               PriorityQueueStrategy m sm pm,
                               DequeueStrategy m so)
                              => Queue m sm so a
                              -- ^ the queue
                              -> pm
                              -- ^ the priority for storing
                              -> a
                              -- ^ the item to enqueue
                              -> Event m ()
{-# INLINABLE enqueueWithStoringPriority #-}
{-# SPECIALISE enqueueWithStoringPriority :: (PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Queue IO sm so a -> pm -> a -> Event IO () #-}
enqueueWithStoringPriority = enqueueStoreWithPriority

-- | Return a signal that notifies when the enqueued item
-- is stored in the internal memory of the queue.
enqueueStored :: Comp m => Queue m sm so a -> Signal m a
{-# INLINABLE enqueueStored #-}
{-# SPECIALISE enqueueStored :: Queue IO sm so a -> Signal IO a #-}
enqueueStored q = publishSignal (enqueueStoredSource q)

-- | Return a signal that notifies when the dequeuing operation was requested.
dequeueRequested :: Comp m => Queue m sm so a -> Signal m ()
{-# INLINABLE dequeueRequested #-}
{-# SPECIALISE dequeueRequested :: Queue IO sm so a -> Signal IO () #-}
dequeueRequested q = publishSignal (dequeueRequestedSource q)

-- | Return a signal that notifies when the item was extracted from the internal
-- storage of the queue and prepared for immediate receiving by the dequeuing process.
dequeueExtracted :: Comp m => Queue m sm so a -> Signal m a
{-# INLINABLE dequeueExtracted #-}
{-# SPECIALISE dequeueExtracted :: Queue IO sm so a -> Signal IO a #-}
dequeueExtracted q = publishSignal (dequeueExtractedSource q)

-- | Store the item.
enqueueStore :: (Comp m,
                 EnqueueStrategy m sm,
                 DequeueStrategy m so)
                => Queue m sm so a
                -- ^ the queue
                -> a
                -- ^ the item to be stored
                -> Event m ()
{-# INLINABLE enqueueStore #-}
{-# SPECIALISE enqueueStore :: (EnqueueStrategy IO sm, DequeueStrategy IO so) => Queue IO sm so a -> a -> Event IO () #-}
enqueueStore q a =
  Event $ \p ->
  do let i = QueueItem { itemValue = a,
                         itemStoringTime = pointTime p }
     invokeEvent p $
       strategyEnqueue (queueStore q) i
     c <- readProtoRef (queueCountRef q)
     let c' = c + 1
         t  = pointTime p
     c' `seq` writeProtoRef (queueCountRef q) c'
     modifyProtoRef' (queueCountStatsRef q) (addTimingStats t c')
     modifyProtoRef' (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i)

-- | Store with the priority the item.
enqueueStoreWithPriority :: (Comp m,
                             PriorityQueueStrategy m sm pm,
                             DequeueStrategy m so)
                            => Queue m sm so a
                            -- ^ the queue
                            -> pm
                            -- ^ the priority for storing
                            -> a
                            -- ^ the item to be enqueued
                            -> Event m ()
{-# INLINABLE enqueueStoreWithPriority #-}
{-# SPECIALISE enqueueStoreWithPriority :: (PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Queue IO sm so a -> pm -> a -> Event IO () #-}
enqueueStoreWithPriority q pm a =
  Event $ \p ->
  do let i = QueueItem { itemValue = a,
                         itemStoringTime = pointTime p }
     invokeEvent p $
       strategyEnqueueWithPriority (queueStore q) pm i
     c <- readProtoRef (queueCountRef q)
     let c' = c + 1
         t  = pointTime p
     c' `seq` writeProtoRef (queueCountRef q) c'
     modifyProtoRef' (queueCountStatsRef q) (addTimingStats t c')
     modifyProtoRef' (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i)

-- | Accept the dequeuing request and return the current simulation time.
dequeueRequest :: Comp m
                  => Queue m sm so a
                  -- ^ the queue
                  -> Event m Double
                  -- ^ the current time
{-# INLINABLE dequeueRequest #-}
{-# SPECIALISE dequeueRequest :: Queue IO sm so a -> Event IO Double #-}
dequeueRequest q =
  Event $ \p ->
  do modifyProtoRef' (dequeueCountRef q) (+ 1)
     invokeEvent p $
       triggerSignal (dequeueRequestedSource q) ()
     return $ pointTime p 

-- | Extract an item for the dequeuing request.  
dequeueExtract :: (Comp m, DequeueStrategy m sm)
                  => Queue m sm so a
                  -- ^ the queue
                  -> Double
                  -- ^ the time of the dequeuing request
                  -> Event m a
                  -- ^ the dequeued value
{-# INLINABLE dequeueExtract #-}
{-# SPECIALISE dequeueExtract :: DequeueStrategy IO sm => Queue IO sm so a -> Double -> Event IO a #-}
dequeueExtract q t' =
  Event $ \p ->
  do i <- invokeEvent p $
          strategyDequeue (queueStore q)
     c <- readProtoRef (queueCountRef q)
     let c' = c - 1
         t  = pointTime p
     c' `seq` writeProtoRef (queueCountRef q) c'
     modifyProtoRef' (queueCountStatsRef q) (addTimingStats t c')
     modifyProtoRef' (dequeueExtractCountRef q) (+ 1)
     invokeEvent p $
       dequeueStat q t' i
     invokeEvent p $
       triggerSignal (dequeueExtractedSource q) (itemValue i)
     return $ itemValue i

-- | Update the statistics for the output wait time of the dequeuing operation
-- and the wait time of storing in the queue.
dequeueStat :: Comp m
               => Queue m sm so a
               -- ^ the queue
               -> Double
               -- ^ the time of the dequeuing request
               -> QueueItem a
               -- ^ the item and its input time
               -> Event m ()
               -- ^ the action of updating the statistics
{-# INLINABLE dequeueStat #-}
{-# SPECIALISE dequeueStat :: Queue IO sm so a -> Double -> QueueItem a -> Event IO () #-}
dequeueStat q t' i =
  Event $ \p ->
  do let t1 = itemStoringTime i
         t  = pointTime p
     modifyProtoRef' (dequeueWaitTimeRef q) $
       addSamplingStats (t - t')
     modifyProtoRef' (queueWaitTimeRef q) $
       addSamplingStats (t - t1)

-- | Signal whenever any property of the queue changes.
--
-- The property must have the corresponded signal. There are also characteristics
-- similar to the properties but that have no signals. As a rule, such characteristics
-- already depend on the simulation time and therefore they may change at any
-- time point.
queueChanged_ :: Comp m => Queue m sm so a -> Signal m ()
{-# INLINABLE queueChanged_ #-}
{-# SPECIALISE queueChanged_ :: Queue IO sm so a -> Signal IO () #-}
queueChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  dequeueRequested q <>
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the summary for the queue with desciption of its
-- properties and activities using the specified indent.
queueSummary :: (Comp m, Show sm, Show so) => Queue m sm so a -> Int -> Event m ShowS
{-# INLINABLE queueSummary #-}
{-# SPECIALISE queueSummary :: (Show sm, Show so) => Queue IO sm so a -> Int -> Event IO ShowS #-}
queueSummary q indent =
  do let sm = enqueueStoringStrategy q
         so = dequeueStrategy q
     null <- queueNull q
     count <- queueCount q
     countStats <- queueCountStats q
     enqueueStoreCount <- enqueueStoreCount q
     dequeueCount <- dequeueCount q
     dequeueExtractCount <- dequeueExtractCount q
     enqueueStoreRate <- enqueueStoreRate q
     dequeueRate <- dequeueRate q
     dequeueExtractRate <- dequeueExtractRate q
     waitTime <- queueWaitTime q
     dequeueWaitTime <- dequeueWaitTime q
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "the storing (memory) strategy = " .
       shows sm .
       showString "\n" .
       showString tab .
       showString "the dequeueing (output) strategy = " .
       shows so .
       showString "\n" .
       showString tab .
       showString "empty? = " .
       shows null .
       showString "\n" .
       showString tab .
       showString "the current size = " .
       shows count .
       showString "\n" .
       showString tab .
       showString "the size statistics = \n\n" .
       timingStatsSummary countStats (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "the enqueue store count (number of the input items that were stored) = " .
       shows enqueueStoreCount .
       showString "\n" .
       showString tab .
       showString "the dequeue count (number of requests for dequeueing an item) = " .
       shows dequeueCount .
       showString "\n" .
       showString tab .
       showString "the dequeue extract count (number of the output items that were dequeued) = " .
       shows dequeueExtractCount .
       showString "\n" .
       showString tab .
       showString "the enqueue store rate (how many input items were stored per time) = " .
       shows enqueueStoreRate .
       showString "\n" .
       showString tab .
       showString "the dequeue rate (how many requests for dequeueing per time) = " .
       shows dequeueRate .
       showString "\n" .
       showString tab .
       showString "the dequeue extract rate (how many output items were dequeued per time) = " .
       shows dequeueExtractRate .
       showString "\n" .
       showString tab .
       showString "the wait time (when was stored -> when was dequeued) = \n\n" .
       samplingStatsSummary waitTime (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "the dequeue wait time (when was requested for dequeueing -> when was dequeued) = \n\n" .
       samplingStatsSummary dequeueWaitTime (2 + indent)
