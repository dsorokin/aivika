
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Queue
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines a queue that can use the specified strategies. So, having only
-- the 'FCFS', 'LCFS', 'SIRO' and 'StaticPriorities' strategies, you can build
-- 4 x 4 x 4 = 64 different types of the queue, each of them will have its own
-- behaviour.
--
module Simulation.Aivika.Trans.Queue
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
        enqueueStrategy,
        enqueueStoringStrategy,
        dequeueStrategy,
        queueNull,
        queueFull,
        queueMaxCount,
        queueCount,
        queueCountStats,
        enqueueCount,
        enqueueLostCount,
        enqueueStoreCount,
        dequeueCount,
        dequeueExtractCount,
        queueLoadFactor,
        enqueueRate,
        enqueueStoreRate,
        dequeueRate,
        dequeueExtractRate,
        queueWaitTime,
        queueTotalWaitTime,
        enqueueWaitTime,
        dequeueWaitTime,
        queueRate,
        -- * Dequeuing and Enqueuing
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
        enqueueWithStoringPriorityOrLost_,
        -- * Awaiting
        waitWhileFullQueue,
        -- * Summary
        queueSummary,
        -- * Derived Signals for Properties
        queueNullChanged,
        queueNullChanged_,
        queueFullChanged,
        queueFullChanged_,
        queueCountChanged,
        queueCountChanged_,
        enqueueCountChanged,
        enqueueCountChanged_,
        enqueueLostCountChanged,
        enqueueLostCountChanged_,
        enqueueStoreCountChanged,
        enqueueStoreCountChanged_,
        dequeueCountChanged,
        dequeueCountChanged_,
        dequeueExtractCountChanged,
        dequeueExtractCountChanged_,
        queueLoadFactorChanged,
        queueLoadFactorChanged_,
        queueWaitTimeChanged,
        queueWaitTimeChanged_,
        queueTotalWaitTimeChanged,
        queueTotalWaitTimeChanged_,
        enqueueWaitTimeChanged,
        enqueueWaitTimeChanged_,
        dequeueWaitTimeChanged,
        dequeueWaitTimeChanged_,
        queueRateChanged,
        queueRateChanged_,
        -- * Basic Signals
        enqueueInitiated,
        enqueueStored,
        enqueueLost,
        dequeueRequested,
        dequeueExtracted,
        -- * Overall Signal
        queueChanged_) where

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
type FCFSQueue m a = Queue m FCFS FCFS FCFS a

-- | A type synonym for the ordinary LIFO queue also known as the LCFS
-- (Last Come - First Serviced) queue.
type LCFSQueue m a = Queue m FCFS LCFS FCFS a

-- | A type synonym for the SIRO (Serviced in Random Order) queue.
type SIROQueue m a = Queue m FCFS SIRO FCFS a

-- | A type synonym for the queue with static priorities applied when
-- storing the elements in the queue.
type PriorityQueue m a = Queue m FCFS StaticPriorities FCFS a

-- | Represents a queue using the specified strategies for enqueueing (input), @si@,
-- internal storing (in memory), @sm@, and dequeueing (output), @so@, where @a@ denotes
-- the type of items stored in the queue. Type @m@ denotes the underlying monad within
-- which the simulation executes.
data Queue m si sm so a =
  Queue { queueMaxCount :: Int,
          -- ^ The queue capacity.
          enqueueStrategy :: si,
          -- ^ The strategy applied to the enqueueing (input) processes when the queue is full.
          enqueueStoringStrategy :: sm,
          -- ^ The strategy applied when storing (in memory) items in the queue.
          dequeueStrategy :: so,
          -- ^ The strategy applied to the dequeueing (output) processes when the queue is empty.
          enqueueRes :: Resource m si,
          queueStore :: StrategyQueue m sm (QueueItem a),
          dequeueRes :: Resource m so,
          queueCountRef :: ProtoRef m Int,
          queueCountStatsRef :: ProtoRef m (TimingStats Int),
          enqueueCountRef :: ProtoRef m Int,
          enqueueLostCountRef :: ProtoRef m Int,
          enqueueStoreCountRef :: ProtoRef m Int,
          dequeueCountRef :: ProtoRef m Int,
          dequeueExtractCountRef :: ProtoRef m Int,
          queueWaitTimeRef :: ProtoRef m (SamplingStats Double),
          queueTotalWaitTimeRef :: ProtoRef m (SamplingStats Double),
          enqueueWaitTimeRef :: ProtoRef m (SamplingStats Double),
          dequeueWaitTimeRef :: ProtoRef m (SamplingStats Double),
          enqueueInitiatedSource :: SignalSource m a,
          enqueueLostSource :: SignalSource m a,
          enqueueStoredSource :: SignalSource m a,
          dequeueRequestedSource :: SignalSource m (),
          dequeueExtractedSource :: SignalSource m a }

-- | Stores the item and a time of its enqueuing. 
data QueueItem a =
  QueueItem { itemValue :: a,
              -- ^ Return the item value.
              itemInputTime :: Double,
              -- ^ Return the time of enqueuing the item.
              itemStoringTime :: Double
              -- ^ Return the time of storing in the queue, or
              -- @itemInputTime@ before the actual storing when
              -- the item was just enqueued.
            }
  
-- | Create a new FCFS queue with the specified capacity.  
newFCFSQueue :: Comp m => Int -> Event m (FCFSQueue m a)
{-# INLINABLE newFCFSQueue #-}
{-# SPECIALISE newFCFSQueue :: Int -> Event IO (FCFSQueue IO a) #-}
newFCFSQueue = newQueue FCFS FCFS FCFS
  
-- | Create a new LCFS queue with the specified capacity.  
newLCFSQueue :: Comp m => Int -> Event m (LCFSQueue m a)  
{-# INLINABLE newLCFSQueue #-}
{-# SPECIALISE newLCFSQueue :: Int -> Event IO (LCFSQueue IO a) #-}
newLCFSQueue = newQueue FCFS LCFS FCFS
  
-- | Create a new SIRO queue with the specified capacity.  
newSIROQueue :: Comp m => Int -> Event m (SIROQueue m a)  
{-# INLINABLE newSIROQueue #-}
{-# SPECIALISE newSIROQueue :: Int -> Event IO (SIROQueue IO a) #-}
newSIROQueue = newQueue FCFS SIRO FCFS
  
-- | Create a new priority queue with the specified capacity.  
newPriorityQueue :: Comp m => Int -> Event m (PriorityQueue m a)  
{-# INLINABLE newPriorityQueue #-}
{-# SPECIALISE newPriorityQueue :: Int -> Event IO (PriorityQueue IO a) #-}
newPriorityQueue = newQueue FCFS StaticPriorities FCFS
  
-- | Create a new queue with the specified strategies and capacity.  
newQueue :: (Comp m,
             QueueStrategy m si,
             QueueStrategy m sm,
             QueueStrategy m so) =>
            si
            -- ^ the strategy applied to the enqueueing (input) processes when the queue is full
            -> sm
            -- ^ the strategy applied when storing items in the queue
            -> so
            -- ^ the strategy applied to the dequeueing (output) processes when the queue is empty
            -> Int
            -- ^ the queue capacity
            -> Event m (Queue m si sm so a)  
{-# INLINABLE newQueue #-}
{-# SPECIALISE newQueue :: (QueueStrategy IO si, QueueStrategy IO sm, QueueStrategy IO so) => si -> sm -> so -> Int -> Event IO (Queue IO si sm so a) #-}
newQueue si sm so count =
  do t  <- liftDynamics time
     sn <- liftParameter simulationSession
     i  <- liftComp $ newProtoRef sn 0
     is <- liftComp $ newProtoRef sn $ returnTimingStats t 0
     ci <- liftComp $ newProtoRef sn 0
     cl <- liftComp $ newProtoRef sn 0
     cm <- liftComp $ newProtoRef sn 0
     cr <- liftComp $ newProtoRef sn 0
     co <- liftComp $ newProtoRef sn 0
     ri <- liftSimulation $ newResourceWithMaxCount si count (Just count)
     qm <- liftSimulation $ newStrategyQueue sm
     ro <- liftSimulation $ newResourceWithMaxCount so 0 (Just count)
     w  <- liftComp $ newProtoRef sn mempty
     wt <- liftComp $ newProtoRef sn mempty
     wi <- liftComp $ newProtoRef sn mempty
     wo <- liftComp $ newProtoRef sn mempty 
     s1 <- liftSimulation $ newSignalSource
     s2 <- liftSimulation $ newSignalSource
     s3 <- liftSimulation $ newSignalSource
     s4 <- liftSimulation $ newSignalSource
     s5 <- liftSimulation $ newSignalSource
     return Queue { queueMaxCount = count,
                    enqueueStrategy = si,
                    enqueueStoringStrategy = sm,
                    dequeueStrategy = so,
                    enqueueRes = ri,
                    queueStore = qm,
                    dequeueRes = ro,
                    queueCountRef = i,
                    queueCountStatsRef = is,
                    enqueueCountRef = ci,
                    enqueueLostCountRef = cl,
                    enqueueStoreCountRef = cm,
                    dequeueCountRef = cr,
                    dequeueExtractCountRef = co,
                    queueWaitTimeRef = w,
                    queueTotalWaitTimeRef = wt,
                    enqueueWaitTimeRef = wi,
                    dequeueWaitTimeRef = wo,
                    enqueueInitiatedSource = s1,
                    enqueueLostSource = s2,
                    enqueueStoredSource = s3,
                    dequeueRequestedSource = s4,
                    dequeueExtractedSource = s5 }
  
-- | Test whether the queue is empty.
--
-- See also 'queueNullChanged' and 'queueNullChanged_'.
queueNull :: Comp m => Queue m si sm so a -> Event m Bool
{-# INLINABLE queueNull #-}
{-# SPECIALISE queueNull :: Queue IO si sm so a -> Event IO Bool #-}
queueNull q =
  Event $ \p ->
  do n <- readProtoRef (queueCountRef q)
     return (n == 0)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged :: Comp m => Queue m si sm so a -> Signal m Bool
{-# INLINABLE queueNullChanged #-}
{-# SPECIALISE queueNullChanged :: Queue IO si sm so a -> Signal IO Bool #-}
queueNullChanged q =
  mapSignalM (const $ queueNull q) (queueNullChanged_ q)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE queueNullChanged_ #-}
{-# SPECIALISE queueNullChanged_ :: Queue IO si sm so a -> Signal IO () #-}
queueNullChanged_ = queueCountChanged_

-- | Test whether the queue is full.
--
-- See also 'queueFullChanged' and 'queueFullChanged_'.
queueFull :: Comp m => Queue m si sm so a -> Event m Bool
{-# INLINABLE queueFull #-}
{-# SPECIALISE queueFull :: Queue IO si sm so a -> Event IO Bool #-}
queueFull q =
  Event $ \p ->
  do n <- readProtoRef (queueCountRef q)
     return (n == queueMaxCount q)
  
-- | Signal when the 'queueFull' property value has changed.
queueFullChanged :: Comp m => Queue m si sm so a -> Signal m Bool
{-# INLINABLE queueFullChanged #-}
{-# SPECIALISE queueFullChanged :: Queue IO si sm so a -> Signal IO Bool #-}
queueFullChanged q =
  mapSignalM (const $ queueFull q) (queueFullChanged_ q)
  
-- | Signal when the 'queueFull' property value has changed.
queueFullChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE queueFullChanged_ #-}
{-# SPECIALISE queueFullChanged_ :: Queue IO si sm so a -> Signal IO () #-}
queueFullChanged_ = queueCountChanged_

-- | Return the current queue size.
--
-- See also 'queueCountStats', 'queueCountChanged' and 'queueCountChanged_'.
queueCount :: Comp m => Queue m si sm so a -> Event m Int
{-# INLINABLE queueCount #-}
{-# SPECIALISE queueCount :: Queue IO si sm so a -> Event IO Int #-}
queueCount q =
  Event $ \p -> readProtoRef (queueCountRef q)

-- | Return the queue size statistics.
queueCountStats :: Comp m => Queue m si sm so a -> Event m (TimingStats Int)
{-# INLINABLE queueCountStats #-}
{-# SPECIALISE queueCountStats :: Queue IO si sm so a -> Event IO (TimingStats Int) #-}
queueCountStats q =
  Event $ \p -> readProtoRef (queueCountStatsRef q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged :: Comp m => Queue m si sm so a -> Signal m Int
{-# INLINABLE queueCountChanged #-}
{-# SPECIALISE queueCountChanged :: Queue IO si sm so a -> Signal IO Int #-}
queueCountChanged q =
  mapSignalM (const $ queueCount q) (queueCountChanged_ q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE queueCountChanged_ #-}
{-# SPECIALISE queueCountChanged_ :: Queue IO si sm so a -> Signal IO () #-}
queueCountChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the total number of input items that were enqueued.
--
-- See also 'enqueueCountChanged' and 'enqueueCountChanged_'.
enqueueCount :: Comp m => Queue m si sm so a -> Event m Int
{-# INLINABLE enqueueCount #-}
{-# SPECIALISE enqueueCount :: Queue IO si sm so a -> Event IO Int #-}
enqueueCount q =
  Event $ \p -> readProtoRef (enqueueCountRef q)
  
-- | Signal when the 'enqueueCount' property value has changed.
enqueueCountChanged :: Comp m => Queue m si sm so a -> Signal m Int
{-# INLINABLE enqueueCountChanged #-}
{-# SPECIALISE enqueueCountChanged :: Queue IO si sm so a -> Signal IO Int #-}
enqueueCountChanged q =
  mapSignalM (const $ enqueueCount q) (enqueueCountChanged_ q)
  
-- | Signal when the 'enqueueCount' property value has changed.
enqueueCountChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE enqueueCountChanged_ #-}
{-# SPECIALISE enqueueCountChanged_ :: Queue IO si sm so a -> Signal IO () #-}
enqueueCountChanged_ q =
  mapSignal (const ()) (enqueueInitiated q)
  
-- | Return the number of lost items.
--
-- See also 'enqueueLostCountChanged' and 'enqueueLostCountChanged_'.
enqueueLostCount :: Comp m => Queue m si sm so a -> Event m Int
{-# INLINABLE enqueueLostCount #-}
{-# SPECIALISE enqueueLostCount :: Queue IO si sm so a -> Event IO Int #-}
enqueueLostCount q =
  Event $ \p -> readProtoRef (enqueueLostCountRef q)
  
-- | Signal when the 'enqueueLostCount' property value has changed.
enqueueLostCountChanged :: Comp m => Queue m si sm so a -> Signal m Int
{-# INLINABLE enqueueLostCountChanged #-}
{-# SPECIALISE enqueueLostCountChanged :: Queue IO si sm so a -> Signal IO Int #-}
enqueueLostCountChanged q =
  mapSignalM (const $ enqueueLostCount q) (enqueueLostCountChanged_ q)
  
-- | Signal when the 'enqueueLostCount' property value has changed.
enqueueLostCountChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE enqueueLostCountChanged_ #-}
{-# SPECIALISE enqueueLostCountChanged_ :: Queue IO si sm so a -> Signal IO () #-}
enqueueLostCountChanged_ q =
  mapSignal (const ()) (enqueueLost q)
      
-- | Return the total number of input items that were stored.
--
-- See also 'enqueueStoreCountChanged' and 'enqueueStoreCountChanged_'.
enqueueStoreCount :: Comp m => Queue m si sm so a -> Event m Int
{-# INLINABLE enqueueStoreCount #-}
{-# SPECIALISE enqueueStoreCount :: Queue IO si sm so a -> Event IO Int #-}
enqueueStoreCount q =
  Event $ \p -> readProtoRef (enqueueStoreCountRef q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged :: Comp m => Queue m si sm so a -> Signal m Int
{-# INLINABLE enqueueStoreCountChanged #-}
{-# SPECIALISE enqueueStoreCountChanged :: Queue IO si sm so a -> Signal IO Int #-}
enqueueStoreCountChanged q =
  mapSignalM (const $ enqueueStoreCount q) (enqueueStoreCountChanged_ q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE enqueueStoreCountChanged_ #-}
{-# SPECIALISE enqueueStoreCountChanged_ :: Queue IO si sm so a -> Signal IO () #-}
enqueueStoreCountChanged_ q =
  mapSignal (const ()) (enqueueStored q)
      
-- | Return the total number of requests for dequeueing the items,
-- not taking into account the failed attempts to dequeue immediately
-- without suspension.
--
-- See also 'dequeueCountChanged' and 'dequeueCountChanged_'.
dequeueCount :: Comp m => Queue m si sm so a -> Event m Int
{-# INLINABLE dequeueCount #-}
{-# SPECIALISE dequeueCount :: Queue IO si sm so a -> Event IO Int #-}
dequeueCount q =
  Event $ \p -> readProtoRef (dequeueCountRef q)
      
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged :: Comp m => Queue m si sm so a -> Signal m Int
{-# INLINABLE dequeueCountChanged #-}
{-# SPECIALISE dequeueCountChanged :: Queue IO si sm so a -> Signal IO Int #-}
dequeueCountChanged q =
  mapSignalM (const $ dequeueCount q) (dequeueCountChanged_ q)
  
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE dequeueCountChanged_ #-}
{-# SPECIALISE dequeueCountChanged_ :: Queue IO si sm so a -> Signal IO () #-}
dequeueCountChanged_ q =
  mapSignal (const ()) (dequeueRequested q)
      
-- | Return the total number of output items that were actually dequeued.
--
-- See also 'dequeueExtractCountChanged' and 'dequeueExtractCountChanged_'.
dequeueExtractCount :: Comp m => Queue m si sm so a -> Event m Int
{-# INLINABLE dequeueExtractCount #-}
{-# SPECIALISE dequeueExtractCount :: Queue IO si sm so a -> Event IO Int #-}
dequeueExtractCount q =
  Event $ \p -> readProtoRef (dequeueExtractCountRef q)
      
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged :: Comp m => Queue m si sm so a -> Signal m Int
{-# INLINABLE dequeueExtractCountChanged #-}
{-# SPECIALISE dequeueExtractCountChanged :: Queue IO si sm so a -> Signal IO Int #-}
dequeueExtractCountChanged q =
  mapSignalM (const $ dequeueExtractCount q) (dequeueExtractCountChanged_ q)
  
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE dequeueExtractCountChanged_ #-}
{-# SPECIALISE dequeueExtractCountChanged_ :: Queue IO si sm so a -> Signal IO () #-}
dequeueExtractCountChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the load factor: the queue size divided by its maximum size.
--
-- See also 'queueLoadFactorChanged' and 'queueLoadFactorChanged_'.
queueLoadFactor :: Comp m => Queue m si sm so a -> Event m Double
{-# INLINABLE queueLoadFactor #-}
{-# SPECIALISE queueLoadFactor :: Queue IO si sm so a -> Event IO Double #-}
queueLoadFactor q =
  Event $ \p ->
  do x <- readProtoRef (queueCountRef q)
     let y = queueMaxCount q
     return (fromIntegral x / fromIntegral y)
      
-- | Signal when the 'queueLoadFactor' property value has changed.
queueLoadFactorChanged :: Comp m => Queue m si sm so a -> Signal m Double
{-# INLINABLE queueLoadFactorChanged #-}
{-# SPECIALISE queueLoadFactorChanged :: Queue IO si sm so a -> Signal IO Double #-}
queueLoadFactorChanged q =
  mapSignalM (const $ queueLoadFactor q) (queueLoadFactorChanged_ q)
  
-- | Signal when the 'queueLoadFactor' property value has changed.
queueLoadFactorChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE queueLoadFactorChanged_ #-}
{-# SPECIALISE queueLoadFactorChanged_ :: Queue IO si sm so a -> Signal IO () #-}
queueLoadFactorChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the rate of the input items that were enqueued: how many items
-- per time.
enqueueRate :: Comp m => Queue m si sm so a -> Event m Double
{-# INLINABLE enqueueRate #-}
{-# SPECIALISE enqueueRate :: Queue IO si sm so a -> Event IO Double #-}
enqueueRate q =
  Event $ \p ->
  do x <- readProtoRef (enqueueCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the items that were stored: how many items
-- per time.
enqueueStoreRate :: Comp m => Queue m si sm so a -> Event m Double
{-# INLINABLE enqueueStoreRate #-}
{-# SPECIALISE enqueueStoreRate :: Queue IO si sm so a -> Event IO Double #-}
enqueueStoreRate q =
  Event $ \p ->
  do x <- readProtoRef (enqueueStoreCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the requests for dequeueing the items: how many requests
-- per time. It does not include the failed attempts to dequeue immediately
-- without suspension.
dequeueRate :: Comp m => Queue m si sm so a -> Event m Double
{-# INLINABLE dequeueRate #-}
{-# SPECIALISE dequeueRate :: Queue IO si sm so a -> Event IO Double #-}
dequeueRate q =
  Event $ \p ->
  do x <- readProtoRef (dequeueCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the output items that were actually dequeued: how many items
-- per time.
dequeueExtractRate :: Comp m => Queue m si sm so a -> Event m Double
{-# INLINABLE dequeueExtractRate #-}
{-# SPECIALISE dequeueExtractRate :: Queue IO si sm so a -> Event IO Double #-}
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
queueWaitTime :: Comp m => Queue m si sm so a -> Event m (SamplingStats Double)
{-# INLINABLE queueWaitTime #-}
{-# SPECIALISE queueWaitTime :: Queue IO si sm so a -> Event IO (SamplingStats Double) #-}
queueWaitTime q =
  Event $ \p -> readProtoRef (queueWaitTimeRef q)
      
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged :: Comp m => Queue m si sm so a -> Signal m (SamplingStats Double)
{-# INLINABLE queueWaitTimeChanged #-}
{-# SPECIALISE queueWaitTimeChanged :: Queue IO si sm so a -> Signal IO (SamplingStats Double) #-}
queueWaitTimeChanged q =
  mapSignalM (const $ queueWaitTime q) (queueWaitTimeChanged_ q)
  
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE queueWaitTimeChanged_ #-}
{-# SPECIALISE queueWaitTimeChanged_ :: Queue IO si sm so a -> Signal IO () #-}
queueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the total wait time from the time at which the enqueueing operation
-- was initiated to the time at which the item was dequeued.
--
-- In some sense, @queueTotalWaitTime == queueInputWaitTime + queueWaitTime@.
--
-- See also 'queueTotalWaitTimeChanged' and 'queueTotalWaitTimeChanged_'.
queueTotalWaitTime :: Comp m => Queue m si sm so a -> Event m (SamplingStats Double)
{-# INLINABLE queueTotalWaitTime #-}
{-# SPECIALISE queueTotalWaitTime :: Queue IO si sm so a -> Event IO (SamplingStats Double) #-}
queueTotalWaitTime q =
  Event $ \p -> readProtoRef (queueTotalWaitTimeRef q)
      
-- | Signal when the 'queueTotalWaitTime' property value has changed.
queueTotalWaitTimeChanged :: Comp m => Queue m si sm so a -> Signal m (SamplingStats Double)
{-# INLINABLE queueTotalWaitTimeChanged #-}
{-# SPECIALISE queueTotalWaitTimeChanged :: Queue IO si sm so a -> Signal IO (SamplingStats Double) #-}
queueTotalWaitTimeChanged q =
  mapSignalM (const $ queueTotalWaitTime q) (queueTotalWaitTimeChanged_ q)
  
-- | Signal when the 'queueTotalWaitTime' property value has changed.
queueTotalWaitTimeChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE queueTotalWaitTimeChanged_ #-}
{-# SPECIALISE queueTotalWaitTimeChanged_ :: Queue IO si sm so a -> Signal IO () #-}
queueTotalWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the enqueue wait time from the time at which the enqueueing operation
-- was initiated to the time at which the item was stored in the queue.
--
-- See also 'enqueueWaitTimeChanged' and 'enqueueWaitTimeChanged_'.
enqueueWaitTime :: Comp m => Queue m si sm so a -> Event m (SamplingStats Double)
{-# INLINABLE enqueueWaitTime #-}
{-# SPECIALISE enqueueWaitTime :: Queue IO si sm so a -> Event IO (SamplingStats Double) #-}
enqueueWaitTime q =
  Event $ \p -> readProtoRef (enqueueWaitTimeRef q)
      
-- | Signal when the 'enqueueWaitTime' property value has changed.
enqueueWaitTimeChanged :: Comp m => Queue m si sm so a -> Signal m (SamplingStats Double)
{-# INLINABLE enqueueWaitTimeChanged #-}
{-# SPECIALISE enqueueWaitTimeChanged :: Queue IO si sm so a -> Signal IO (SamplingStats Double) #-}
enqueueWaitTimeChanged q =
  mapSignalM (const $ enqueueWaitTime q) (enqueueWaitTimeChanged_ q)
  
-- | Signal when the 'enqueueWaitTime' property value has changed.
enqueueWaitTimeChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE enqueueWaitTimeChanged_ #-}
{-# SPECIALISE enqueueWaitTimeChanged_ :: Queue IO si sm so a -> Signal IO () #-}
enqueueWaitTimeChanged_ q =
  mapSignal (const ()) (enqueueStored q)
      
-- | Return the dequeue wait time from the time at which the item was requested
-- for dequeueing to the time at which it was actually dequeued.
--
-- See also 'dequeueWaitTimeChanged' and 'dequeueWaitTimeChanged_'.
dequeueWaitTime :: Comp m => Queue m si sm so a -> Event m (SamplingStats Double)
{-# INLINABLE dequeueWaitTime #-}
{-# SPECIALISE dequeueWaitTime :: Queue IO si sm so a -> Event IO (SamplingStats Double) #-}
dequeueWaitTime q =
  Event $ \p -> readProtoRef (dequeueWaitTimeRef q)
      
-- | Signal when the 'dequeueWaitTime' property value has changed.
dequeueWaitTimeChanged :: Comp m => Queue m si sm so a -> Signal m (SamplingStats Double)
{-# INLINABLE dequeueWaitTimeChanged #-}
{-# SPECIALISE dequeueWaitTimeChanged :: Queue IO si sm so a -> Signal IO (SamplingStats Double) #-}
dequeueWaitTimeChanged q =
  mapSignalM (const $ dequeueWaitTime q) (dequeueWaitTimeChanged_ q)
  
-- | Signal when the 'dequeueWaitTime' property value has changed.
dequeueWaitTimeChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE dequeueWaitTimeChanged_ #-}
{-# SPECIALISE dequeueWaitTimeChanged_ :: Queue IO si sm so a -> Signal IO () #-}
dequeueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)

-- | Return a long-term average queue rate calculated as
-- the average queue size divided by the average wait time.
--
-- This value may be less than the actual arrival rate as the queue is
-- finite and new arrivals may be locked while the queue remains full.
--
-- See also 'queueRateChanged' and 'queueRateChanged_'.
queueRate :: Comp m => Queue m si sm so a -> Event m Double
{-# INLINABLE queueRate #-}
{-# SPECIALISE queueRate :: Queue IO si sm so a -> Event IO Double #-}
queueRate q =
  Event $ \p ->
  do x <- readProtoRef (queueCountStatsRef q)
     y <- readProtoRef (queueWaitTimeRef q)
     return (timingStatsMean x / samplingStatsMean y) 
      
-- | Signal when the 'queueRate' property value has changed.
queueRateChanged :: Comp m => Queue m si sm so a -> Signal m Double
{-# INLINABLE queueRateChanged #-}
{-# SPECIALISE queueRateChanged :: Queue IO si sm so a -> Signal IO Double #-}
queueRateChanged q =
  mapSignalM (const $ queueRate q) (queueRateChanged_ q)
      
-- | Signal when the 'queueRate' property value has changed.
queueRateChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE queueRateChanged_ #-}
{-# SPECIALISE queueRateChanged_ :: Queue IO si sm so a -> Signal IO () #-}
queueRateChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)

-- | Dequeue suspending the process if the queue is empty.
dequeue :: (Comp m,
            DequeueStrategy m si,
            DequeueStrategy m sm,
            EnqueueStrategy m so)
           => Queue m si sm so a
           -- ^ the queue
           -> Process m a
           -- ^ the dequeued value
{-# INLINABLE dequeue #-}
{-# SPECIALISE dequeue :: (DequeueStrategy IO si, DequeueStrategy IO sm, EnqueueStrategy IO so) => Queue IO si sm so a -> Process IO a #-}
dequeue q =
  do t <- liftEvent $ dequeueRequest q
     requestResource (dequeueRes q)
     liftEvent $ dequeueExtract q t
  
-- | Dequeue with the output priority suspending the process if the queue is empty.
dequeueWithOutputPriority :: (Comp m,
                              DequeueStrategy m si,
                              DequeueStrategy m sm,
                              PriorityQueueStrategy m so po)
                             => Queue m si sm so a
                             -- ^ the queue
                             -> po
                             -- ^ the priority for output
                             -> Process m a
                             -- ^ the dequeued value
{-# INLINABLE dequeueWithOutputPriority #-}
{-# SPECIALISE dequeueWithOutputPriority :: (DequeueStrategy IO si, DequeueStrategy IO sm, PriorityQueueStrategy IO so po) => Queue IO si sm so a -> po -> Process IO a #-}
dequeueWithOutputPriority q po =
  do t <- liftEvent $ dequeueRequest q
     requestResourceWithPriority (dequeueRes q) po
     liftEvent $ dequeueExtract q t
  
-- | Try to dequeue immediately.
tryDequeue :: (Comp m,
               DequeueStrategy m si,
               DequeueStrategy m sm)
              => Queue m si sm so a
              -- ^ the queue
              -> Event m (Maybe a)
              -- ^ the dequeued value of 'Nothing'
{-# INLINABLE tryDequeue #-}
{-# SPECIALISE tryDequeue :: (DequeueStrategy IO si, DequeueStrategy IO sm) => Queue IO si sm so a -> Event IO (Maybe a) #-}
tryDequeue q =
  do x <- tryRequestResourceWithinEvent (dequeueRes q)
     if x 
       then do t <- dequeueRequest q
               fmap Just $ dequeueExtract q t
       else return Nothing

-- | Enqueue the item suspending the process if the queue is full.  
enqueue :: (Comp m,
            EnqueueStrategy m si,
            EnqueueStrategy m sm,
            DequeueStrategy m so)
           => Queue m si sm so a
           -- ^ the queue
           -> a
           -- ^ the item to enqueue
           -> Process m ()
{-# INLINABLE enqueue #-}
{-# SPECIALISE enqueue :: (EnqueueStrategy IO si, EnqueueStrategy IO sm, DequeueStrategy IO so) => Queue IO si sm so a -> a -> Process IO () #-}
enqueue q a =
  do i <- liftEvent $ enqueueInitiate q a
     requestResource (enqueueRes q)
     liftEvent $ enqueueStore q i
     
-- | Enqueue with the input priority the item suspending the process if the queue is full.  
enqueueWithInputPriority :: (Comp m,
                             PriorityQueueStrategy m si pi,
                             EnqueueStrategy m sm,
                             DequeueStrategy m so)
                            => Queue m si sm so a
                            -- ^ the queue
                            -> pi
                            -- ^ the priority for input
                            -> a
                            -- ^ the item to enqueue
                            -> Process m ()
{-# INLINABLE enqueueWithInputPriority #-}
{-# SPECIALISE enqueueWithInputPriority :: (PriorityQueueStrategy IO si pi, EnqueueStrategy IO sm, DequeueStrategy IO so) => Queue IO si sm so a -> pi -> a -> Process IO () #-}
enqueueWithInputPriority q pi a =
  do i <- liftEvent $ enqueueInitiate q a
     requestResourceWithPriority (enqueueRes q) pi
     liftEvent $ enqueueStore q i
     
-- | Enqueue with the storing priority the item suspending the process if the queue is full.  
enqueueWithStoringPriority :: (Comp m,
                               EnqueueStrategy m si,
                               PriorityQueueStrategy m sm pm,
                               DequeueStrategy m so)
                              => Queue m si sm so a
                              -- ^ the queue
                              -> pm
                              -- ^ the priority for storing
                              -> a
                              -- ^ the item to enqueue
                              -> Process m ()
{-# INLINABLE enqueueWithStoringPriority #-}
{-# SPECIALISE enqueueWithStoringPriority :: (EnqueueStrategy IO si, PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Queue IO si sm so a -> pm -> a -> Process IO () #-}
enqueueWithStoringPriority q pm a =
  do i <- liftEvent $ enqueueInitiate q a
     requestResource (enqueueRes q)
     liftEvent $ enqueueStoreWithPriority q pm i
     
-- | Enqueue with the input and storing priorities the item suspending the process if the queue is full.  
enqueueWithInputStoringPriorities :: (Comp m,
                                      PriorityQueueStrategy m si pi,
                                      PriorityQueueStrategy m sm pm,
                                      DequeueStrategy m so)
                                     => Queue m si sm so a
                                     -- ^ the queue
                                     -> pi
                                     -- ^ the priority for input
                                     -> pm
                                     -- ^ the priority for storing
                                     -> a
                                     -- ^ the item to enqueue
                                     -> Process m ()
{-# INLINABLE enqueueWithInputStoringPriorities #-}
{-# SPECIALISE enqueueWithInputStoringPriorities :: (PriorityQueueStrategy IO si pi, PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Queue IO si sm so a -> pi -> pm -> a -> Process IO () #-}
enqueueWithInputStoringPriorities q pi pm a =
  do i <- liftEvent $ enqueueInitiate q a
     requestResourceWithPriority (enqueueRes q) pi
     liftEvent $ enqueueStoreWithPriority q pm i
     
-- | Try to enqueue the item. Return 'False' in the monad if the queue is full.
tryEnqueue :: (Comp m,
               EnqueueStrategy m sm,
               DequeueStrategy m so)
              => Queue m si sm so a
              -- ^ the queue
              -> a
              -- ^ the item which we try to enqueue
              -> Event m Bool
{-# INLINABLE tryEnqueue #-}
{-# SPECIALISE tryEnqueue :: (EnqueueStrategy IO sm, DequeueStrategy IO so) => Queue IO si sm so a -> a -> Event IO Bool #-}
tryEnqueue q a =
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x 
       then do enqueueInitiate q a >>= enqueueStore q
               return True
       else return False

-- | Try to enqueue with the storing priority the item. Return 'False' in
-- the monad if the queue is full.
tryEnqueueWithStoringPriority :: (Comp m,
                                  PriorityQueueStrategy m sm pm,
                                  DequeueStrategy m so)
                                 => Queue m si sm so a
                                 -- ^ the queue
                                 -> pm
                                 -- ^ the priority for storing
                                 -> a
                                 -- ^ the item which we try to enqueue
                                 -> Event m Bool
{-# INLINABLE tryEnqueueWithStoringPriority #-}
{-# SPECIALISE tryEnqueueWithStoringPriority :: (PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Queue IO si sm so a -> pm -> a -> Event IO Bool #-}
tryEnqueueWithStoringPriority q pm a =
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x 
       then do enqueueInitiate q a >>= enqueueStoreWithPriority q pm
               return True
       else return False

-- | Try to enqueue the item. If the queue is full then the item will be lost
-- and 'False' will be returned.
enqueueOrLost :: (Comp m,
                  EnqueueStrategy m sm,
                  DequeueStrategy m so)
                 => Queue m si sm so a
                 -- ^ the queue
                 -> a
                 -- ^ the item which we try to enqueue
                 -> Event m Bool
{-# INLINABLE enqueueOrLost #-}
{-# SPECIALISE enqueueOrLost :: (EnqueueStrategy IO sm, DequeueStrategy IO so) => Queue IO si sm so a -> a -> Event IO Bool #-}
enqueueOrLost q a =
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x
       then do enqueueInitiate q a >>= enqueueStore q
               return True
       else do enqueueDeny q a
               return False

-- | Try to enqueue with the storing priority the item. If the queue is full
-- then the item will be lost and 'False' will be returned.
enqueueWithStoringPriorityOrLost :: (Comp m,
                                     PriorityQueueStrategy m sm pm,
                                     DequeueStrategy m so)
                                    => Queue m si sm so a
                                    -- ^ the queue
                                    -> pm
                                    -- ^ the priority for storing
                                    -> a
                                    -- ^ the item which we try to enqueue
                                    -> Event m Bool
{-# INLINABLE enqueueWithStoringPriorityOrLost #-}
{-# SPECIALISE enqueueWithStoringPriorityOrLost :: (PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Queue IO si sm so a -> pm -> a -> Event IO Bool #-}
enqueueWithStoringPriorityOrLost q pm a =
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x
       then do enqueueInitiate q a >>= enqueueStoreWithPriority q pm
               return True
       else do enqueueDeny q a
               return False

-- | Try to enqueue the item. If the queue is full then the item will be lost.
enqueueOrLost_ :: (Comp m,
                   EnqueueStrategy m sm,
                   DequeueStrategy m so)
                  => Queue m si sm so a
                  -- ^ the queue
                  -> a
                  -- ^ the item which we try to enqueue
                  -> Event m ()
{-# INLINABLE enqueueOrLost_ #-}
{-# SPECIALISE enqueueOrLost_ :: (EnqueueStrategy IO sm, DequeueStrategy IO so) => Queue IO si sm so a -> a -> Event IO () #-}
enqueueOrLost_ q a =
  do x <- enqueueOrLost q a
     return ()

-- | Try to enqueue with the storing priority the item. If the queue is full
-- then the item will be lost.
enqueueWithStoringPriorityOrLost_ :: (Comp m,
                                      PriorityQueueStrategy m sm pm,
                                      DequeueStrategy m so)
                                     => Queue m si sm so a
                                     -- ^ the queue
                                     -> pm
                                     -- ^ the priority for storing
                                     -> a
                                     -- ^ the item which we try to enqueue
                                     -> Event m ()
{-# INLINABLE enqueueWithStoringPriorityOrLost_ #-}
{-# SPECIALISE enqueueWithStoringPriorityOrLost_ :: (PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Queue IO si sm so a -> pm -> a -> Event IO () #-}
enqueueWithStoringPriorityOrLost_ q pm a =
  do x <- enqueueWithStoringPriorityOrLost q pm a
     return ()

-- | Return a signal that notifies when the enqueuing operation is initiated.
enqueueInitiated :: Comp m => Queue m si sm so a -> Signal m a
{-# INLINABLE enqueueInitiated #-}
{-# SPECIALISE enqueueInitiated :: Queue IO si sm so a -> Signal IO a #-}
enqueueInitiated q = publishSignal (enqueueInitiatedSource q)

-- | Return a signal that notifies when the enqueuing operation is completed
-- and the item is stored in the internal memory of the queue.
enqueueStored :: Comp m => Queue m si sm so a -> Signal m a
{-# INLINABLE enqueueStored #-}
{-# SPECIALISE enqueueStored :: Queue IO si sm so a -> Signal IO a #-}
enqueueStored q = publishSignal (enqueueStoredSource q)

-- | Return a signal which notifies that the item was lost when 
-- attempting to add it to the full queue with help of
-- 'enqueueOrLost', 'enqueueOrLost_' or similar functions that imply
-- that the element can be lost. All their names are ending with @OrLost@
-- or @OrLost_@.
--
-- In other cases the enqueued items are not lost but the corresponded process
-- can suspend until the internal queue storage is freed. Although there is one
-- exception from this rule. If the process trying to enqueue a new element was
-- suspended but then canceled through 'cancelProcess' from the outside then
-- the item will not be added.
enqueueLost :: Comp m => Queue m si sm so a -> Signal m a
{-# INLINABLE enqueueLost #-}
{-# SPECIALISE enqueueLost :: Queue IO si sm so a -> Signal IO a #-}
enqueueLost q = publishSignal (enqueueLostSource q)

-- | Return a signal that notifies when the dequeuing operation was requested.
dequeueRequested :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE dequeueRequested #-}
{-# SPECIALISE dequeueRequested :: Queue IO si sm so a -> Signal IO () #-}
dequeueRequested q = publishSignal (dequeueRequestedSource q)

-- | Return a signal that notifies when the item was extracted from the internal
-- storage of the queue and prepared for immediate receiving by the dequeuing process.
dequeueExtracted :: Comp m => Queue m si sm so a -> Signal m a
{-# INLINABLE dequeueExtracted #-}
{-# SPECIALISE dequeueExtracted :: Queue IO si sm so a -> Signal IO a #-}
dequeueExtracted q = publishSignal (dequeueExtractedSource q)

-- | Initiate the process of enqueuing the item.
enqueueInitiate :: Comp m
                   => Queue m si sm so a
                   -- ^ the queue
                   -> a
                   -- ^ the item to be enqueued
                   -> Event m (QueueItem a)
{-# INLINABLE enqueueInitiate #-}
{-# SPECIALISE enqueueInitiate :: Queue IO si sm so a -> a -> Event IO (QueueItem a) #-}
enqueueInitiate q a =
  Event $ \p ->
  do let t = pointTime p
     modifyProtoRef' (enqueueCountRef q) (+ 1)
     invokeEvent p $
       triggerSignal (enqueueInitiatedSource q) a
     return QueueItem { itemValue = a,
                        itemInputTime = t,
                        itemStoringTime = t  -- it will be updated soon
                      }

-- | Store the item.
enqueueStore :: (Comp m,
                 EnqueueStrategy m sm,
                 DequeueStrategy m so)
                => Queue m si sm so a
                -- ^ the queue
                -> QueueItem a
                -- ^ the item to be stored
                -> Event m ()
{-# INLINABLE enqueueStore #-}
{-# SPECIALISE enqueueStore :: (EnqueueStrategy IO sm, DequeueStrategy IO so) => Queue IO si sm so a -> QueueItem a -> Event IO () #-}
enqueueStore q i =
  Event $ \p ->
  do let i' = i { itemStoringTime = pointTime p }  -- now we have the actual time of storing
     invokeEvent p $
       strategyEnqueue (queueStore q) i'
     c <- readProtoRef (queueCountRef q)
     let c' = c + 1
         t  = pointTime p 
     c' `seq` writeProtoRef (queueCountRef q) c'
     modifyProtoRef' (queueCountStatsRef q) (addTimingStats t c')
     modifyProtoRef' (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       enqueueStat q i'
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i')

-- | Store with the priority the item.
enqueueStoreWithPriority :: (Comp m,
                             PriorityQueueStrategy m sm pm,
                             DequeueStrategy m so)
                            => Queue m si sm so a
                            -- ^ the queue
                            -> pm
                            -- ^ the priority for storing
                            -> QueueItem a
                            -- ^ the item to be enqueued
                            -> Event m ()
{-# INLINABLE enqueueStoreWithPriority #-}
{-# SPECIALISE enqueueStoreWithPriority :: (PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Queue IO si sm so a -> pm -> QueueItem a -> Event IO () #-}
enqueueStoreWithPriority q pm i =
  Event $ \p ->
  do let i' = i { itemStoringTime = pointTime p }  -- now we have the actual time of storing
     invokeEvent p $
       strategyEnqueueWithPriority (queueStore q) pm i'
     c <- readProtoRef (queueCountRef q)
     let c' = c + 1
         t  = pointTime p
     c' `seq` writeProtoRef (queueCountRef q) c'
     modifyProtoRef' (queueCountStatsRef q) (addTimingStats t c')
     modifyProtoRef' (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       enqueueStat q i'
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i')

-- | Deny the enqueuing.
enqueueDeny :: Comp m
               => Queue m si sm so a
               -- ^ the queue
               -> a
               -- ^ the item to be denied
               -> Event m ()
{-# INLINABLE enqueueDeny #-}
{-# SPECIALISE enqueueDeny :: Queue IO si sm so a -> a -> Event IO () #-}
enqueueDeny q a =
  Event $ \p ->
  do modifyProtoRef' (enqueueLostCountRef q) $ (+) 1
     invokeEvent p $
       triggerSignal (enqueueLostSource q) a

-- | Update the statistics for the input wait time of the enqueuing operation.
enqueueStat :: Comp m
               => Queue m si sm so a
               -- ^ the queue
               -> QueueItem a
               -- ^ the item and its input time
               -> Event m ()
               -- ^ the action of updating the statistics
{-# INLINABLE enqueueStat #-}
{-# SPECIALISE enqueueStat :: Queue IO si sm so a -> QueueItem a -> Event IO () #-}
enqueueStat q i =
  Event $ \p ->
  do let t0 = itemInputTime i
         t1 = itemStoringTime i
     modifyProtoRef' (enqueueWaitTimeRef q) $
       addSamplingStats (t1 - t0)

-- | Accept the dequeuing request and return the current simulation time.
dequeueRequest :: Comp m
                  => Queue m si sm so a
                  -- ^ the queue
                  -> Event m Double
                  -- ^ the current time
{-# INLINABLE dequeueRequest #-}
{-# SPECIALISE dequeueRequest :: Queue IO si sm so a -> Event IO Double #-}
dequeueRequest q =
  Event $ \p ->
  do modifyProtoRef' (dequeueCountRef q) (+ 1)
     invokeEvent p $
       triggerSignal (dequeueRequestedSource q) ()
     return $ pointTime p 

-- | Extract an item for the dequeuing request.  
dequeueExtract :: (Comp m,
                   DequeueStrategy m si,
                   DequeueStrategy m sm)
                  => Queue m si sm so a
                  -- ^ the queue
                  -> Double
                  -- ^ the time of the dequeuing request
                  -> Event m a
                  -- ^ the dequeued value
{-# INLINABLE dequeueExtract #-}
{-# SPECIALISE dequeueExtract :: (DequeueStrategy IO si, DequeueStrategy IO sm) => Queue IO si sm so a -> Double -> Event IO a #-}
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
       releaseResourceWithinEvent (enqueueRes q)
     invokeEvent p $
       triggerSignal (dequeueExtractedSource q) (itemValue i)
     return $ itemValue i

-- | Update the statistics for the output wait time of the dequeuing operation
-- and the wait time of storing in the queue.
dequeueStat :: Comp m
               => Queue m si sm so a
               -- ^ the queue
               -> Double
               -- ^ the time of the dequeuing request
               -> QueueItem a
               -- ^ the item and its input time
               -> Event m ()
               -- ^ the action of updating the statistics
{-# INLINABLE dequeueStat #-}
{-# SPECIALISE dequeueStat :: Queue IO si sm so a -> Double -> QueueItem a -> Event IO () #-}
dequeueStat q t' i =
  Event $ \p ->
  do let t0 = itemInputTime i
         t1 = itemStoringTime i
         t  = pointTime p
     modifyProtoRef' (dequeueWaitTimeRef q) $
       addSamplingStats (t - t')
     modifyProtoRef' (queueTotalWaitTimeRef q) $
       addSamplingStats (t - t0)
     modifyProtoRef' (queueWaitTimeRef q) $
       addSamplingStats (t - t1)

-- | Wait while the queue is full.
waitWhileFullQueue :: Comp m => Queue m si sm so a -> Process m ()
{-# INLINABLE waitWhileFullQueue #-}
{-# SPECIALISE waitWhileFullQueue :: Queue IO si sm so a -> Process IO () #-}
waitWhileFullQueue q =
  do x <- liftEvent (queueFull q)
     when x $
       do processAwait (dequeueExtracted q)
          waitWhileFullQueue q

-- | Signal whenever any property of the queue changes.
--
-- The property must have the corresponded signal. There are also characteristics
-- similar to the properties but that have no signals. As a rule, such characteristics
-- already depend on the simulation time and therefore they may change at any
-- time point.
queueChanged_ :: Comp m => Queue m si sm so a -> Signal m ()
{-# INLINABLE queueChanged_ #-}
{-# SPECIALISE queueChanged_ :: Queue IO si sm so a -> Signal IO () #-}
queueChanged_ q =
  mapSignal (const ()) (enqueueInitiated q) <>
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (enqueueLost q) <>
  dequeueRequested q <>
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the summary for the queue with desciption of its
-- properties and activities using the specified indent.
queueSummary :: (Comp m, Show si, Show sm, Show so) => Queue m si sm so a -> Int -> Event m ShowS
{-# INLINABLE queueSummary #-}
{-# SPECIALISE queueSummary :: (Show si, Show sm, Show so) => Queue IO si sm so a -> Int -> Event IO ShowS #-}
queueSummary q indent =
  do let si = enqueueStrategy q
         sm = enqueueStoringStrategy q
         so = dequeueStrategy q
     null <- queueNull q
     full <- queueFull q
     let maxCount = queueMaxCount q
     count <- queueCount q
     countStats <- queueCountStats q
     enqueueCount <- enqueueCount q
     enqueueLostCount <- enqueueLostCount q
     enqueueStoreCount <- enqueueStoreCount q
     dequeueCount <- dequeueCount q
     dequeueExtractCount <- dequeueExtractCount q
     loadFactor <- queueLoadFactor q
     enqueueRate <- enqueueRate q
     enqueueStoreRate <- enqueueStoreRate q
     dequeueRate <- dequeueRate q
     dequeueExtractRate <- dequeueExtractRate q
     waitTime <- queueWaitTime q
     totalWaitTime <- queueTotalWaitTime q
     enqueueWaitTime <- enqueueWaitTime q
     dequeueWaitTime <- dequeueWaitTime q
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "the enqueueing (input) strategy = " .
       shows si .
       showString "\n" .
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
       showString "full? = " .
       shows full .
       showString "\n" .
       showString tab .
       showString "max. capacity = " .
       shows maxCount .
       showString "\n" .
       showString tab .
       showString "size = " .
       shows count .
       showString "\n" .
       showString tab .
       showString "the size statistics = \n\n" .
       timingStatsSummary countStats (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "the enqueue count (number of the input items that were enqueued) = " .
       shows enqueueCount .
       showString "\n" .
       showString tab .
       showString "the enqueue lost count (number of the lost items) = " .
       shows enqueueLostCount .
       showString "\n" .
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
       showString "the load factor (size / max. capacity) = " .
       shows loadFactor .
       showString "\n" .
       showString tab .
       showString "the enqueue rate (how many input items were enqueued per time) = " .
       shows enqueueRate .
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
       showString "the total wait time (when the enqueueing was initiated -> when was dequeued) = \n\n" .
       samplingStatsSummary totalWaitTime (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "the enqueue wait time (when the enqueueing was initiated -> when was stored) = \n\n" .
       samplingStatsSummary enqueueWaitTime (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "the dequeue wait time (when was requested for dequeueing -> when was dequeued) = \n\n" .
       samplingStatsSummary dequeueWaitTime (2 + indent)
