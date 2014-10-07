
-- |
-- Module     : Simulation.Aivika.Queue
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
module Simulation.Aivika.Queue
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

import Data.IORef
import Data.Monoid

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.Internal.Signal
import Simulation.Aivika.Signal
import Simulation.Aivika.Resource
import Simulation.Aivika.QueueStrategy
import Simulation.Aivika.Statistics

import qualified Simulation.Aivika.DoubleLinkedList as DLL 
import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.PriorityQueue as PQ

-- | A type synonym for the ordinary FIFO queue also known as the FCFS
-- (First Come - First Serviced) queue.
type FCFSQueue a =
  Queue FCFS DLL.DoubleLinkedList FCFS DLL.DoubleLinkedList FCFS DLL.DoubleLinkedList a

-- | A type synonym for the ordinary LIFO queue also known as the LCFS
-- (Last Come - First Serviced) queue.
type LCFSQueue a =
  Queue FCFS DLL.DoubleLinkedList LCFS DLL.DoubleLinkedList FCFS DLL.DoubleLinkedList a

-- | A type synonym for the SIRO (Serviced in Random Order) queue.
type SIROQueue a =
  Queue FCFS DLL.DoubleLinkedList SIRO V.Vector FCFS DLL.DoubleLinkedList a

-- | A type synonym for the queue with static priorities applied when
-- storing the elements in the queue.
type PriorityQueue a =
  Queue FCFS DLL.DoubleLinkedList StaticPriorities PQ.PriorityQueue FCFS DLL.DoubleLinkedList a

-- | Represents a queue using the specified strategies for enqueueing (input), @si@,
-- internal storing (in memory), @sm@, and dequeueing (output), @so@, where @a@ denotes
-- the type of items stored in the queue. Types @qi@, @qm@ and @qo@ are
-- determined automatically and you should not care about them - they
-- are dependent types.
data Queue si qi sm qm so qo a =
  Queue { queueMaxCount :: Int,
          -- ^ The queue capacity.
          enqueueStrategy :: si,
          -- ^ The strategy applied to the enqueueing (input) processes when the queue is full.
          enqueueStoringStrategy :: sm,
          -- ^ The strategy applied when storing (in memory) items in the queue.
          dequeueStrategy :: so,
          -- ^ The strategy applied to the dequeueing (output) processes when the queue is empty.
          enqueueRes :: Resource si qi,
          queueStore :: qm (QueueItem a),
          dequeueRes :: Resource so qo,
          queueCountRef :: IORef Int,
          queueCountStatsRef :: IORef (TimingStats Int),
          enqueueCountRef :: IORef Int,
          enqueueLostCountRef :: IORef Int,
          enqueueStoreCountRef :: IORef Int,
          dequeueCountRef :: IORef Int,
          dequeueExtractCountRef :: IORef Int,
          queueWaitTimeRef :: IORef (SamplingStats Double),
          queueTotalWaitTimeRef :: IORef (SamplingStats Double),
          enqueueWaitTimeRef :: IORef (SamplingStats Double),
          dequeueWaitTimeRef :: IORef (SamplingStats Double),
          enqueueInitiatedSource :: SignalSource a,
          enqueueLostSource :: SignalSource a,
          enqueueStoredSource :: SignalSource a,
          dequeueRequestedSource :: SignalSource (),
          dequeueExtractedSource :: SignalSource a }

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
newFCFSQueue :: Int -> Event (FCFSQueue a)  
newFCFSQueue = newQueue FCFS FCFS FCFS
  
-- | Create a new LCFS queue with the specified capacity.  
newLCFSQueue :: Int -> Event (LCFSQueue a)  
newLCFSQueue = newQueue FCFS LCFS FCFS
  
-- | Create a new SIRO queue with the specified capacity.  
newSIROQueue :: Int -> Event (SIROQueue a)  
newSIROQueue = newQueue FCFS SIRO FCFS
  
-- | Create a new priority queue with the specified capacity.  
newPriorityQueue :: Int -> Event (PriorityQueue a)  
newPriorityQueue = newQueue FCFS StaticPriorities FCFS
  
-- | Create a new queue with the specified strategies and capacity.  
newQueue :: (QueueStrategy si qi,
             QueueStrategy sm qm,
             QueueStrategy so qo) =>
            si
            -- ^ the strategy applied to the enqueueing (input) processes when the queue is full
            -> sm
            -- ^ the strategy applied when storing items in the queue
            -> so
            -- ^ the strategy applied to the dequeueing (output) processes when the queue is empty
            -> Int
            -- ^ the queue capacity
            -> Event (Queue si qi sm qm so qo a)  
newQueue si sm so count =
  do t  <- liftDynamics time
     i  <- liftIO $ newIORef 0
     is <- liftIO $ newIORef $ returnTimingStats t 0
     ci <- liftIO $ newIORef 0
     cl <- liftIO $ newIORef 0
     cm <- liftIO $ newIORef 0
     cr <- liftIO $ newIORef 0
     co <- liftIO $ newIORef 0
     ri <- liftSimulation $ newResourceWithMaxCount si count (Just count)
     qm <- liftSimulation $ newStrategyQueue sm
     ro <- liftSimulation $ newResourceWithMaxCount so 0 (Just count)
     w  <- liftIO $ newIORef mempty
     wt <- liftIO $ newIORef mempty
     wi <- liftIO $ newIORef mempty
     wo <- liftIO $ newIORef mempty 
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
queueNull :: Queue si qi sm qm so qo a -> Event Bool
queueNull q =
  Event $ \p ->
  do n <- readIORef (queueCountRef q)
     return (n == 0)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged :: Queue si qi sm qm so qo a -> Signal Bool
queueNullChanged q =
  mapSignalM (const $ queueNull q) (queueNullChanged_ q)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueNullChanged_ = queueCountChanged_

-- | Test whether the queue is full.
--
-- See also 'queueFullChanged' and 'queueFullChanged_'.
queueFull :: Queue si qi sm qm so qo a -> Event Bool
queueFull q =
  Event $ \p ->
  do n <- readIORef (queueCountRef q)
     return (n == queueMaxCount q)
  
-- | Signal when the 'queueFull' property value has changed.
queueFullChanged :: Queue si qi sm qm so qo a -> Signal Bool
queueFullChanged q =
  mapSignalM (const $ queueFull q) (queueFullChanged_ q)
  
-- | Signal when the 'queueFull' property value has changed.
queueFullChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueFullChanged_ = queueCountChanged_

-- | Return the current queue size.
--
-- See also 'queueCountStats', 'queueCountChanged' and 'queueCountChanged_'.
queueCount :: Queue si qi sm qm so qo a -> Event Int
queueCount q =
  Event $ \p -> readIORef (queueCountRef q)

-- | Return the queue size statistics.
queueCountStats :: Queue si qi sm qm so qo a -> Event (TimingStats Int)
queueCountStats q =
  Event $ \p -> readIORef (queueCountStatsRef q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged :: Queue si qi sm qm so qo a -> Signal Int
queueCountChanged q =
  mapSignalM (const $ queueCount q) (queueCountChanged_ q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueCountChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the total number of input items that were enqueued.
--
-- See also 'enqueueCountChanged' and 'enqueueCountChanged_'.
enqueueCount :: Queue si qi sm qm so qo a -> Event Int
enqueueCount q =
  Event $ \p -> readIORef (enqueueCountRef q)
  
-- | Signal when the 'enqueueCount' property value has changed.
enqueueCountChanged :: Queue si qi sm qm so qo a -> Signal Int
enqueueCountChanged q =
  mapSignalM (const $ enqueueCount q) (enqueueCountChanged_ q)
  
-- | Signal when the 'enqueueCount' property value has changed.
enqueueCountChanged_ :: Queue si qi sm qm so qo a -> Signal ()
enqueueCountChanged_ q =
  mapSignal (const ()) (enqueueInitiated q)
  
-- | Return the number of lost items.
--
-- See also 'enqueueLostCountChanged' and 'enqueueLostCountChanged_'.
enqueueLostCount :: Queue si qi sm qm so qo a -> Event Int
enqueueLostCount q =
  Event $ \p -> readIORef (enqueueLostCountRef q)
  
-- | Signal when the 'enqueueLostCount' property value has changed.
enqueueLostCountChanged :: Queue si qi sm qm so qo a -> Signal Int
enqueueLostCountChanged q =
  mapSignalM (const $ enqueueLostCount q) (enqueueLostCountChanged_ q)
  
-- | Signal when the 'enqueueLostCount' property value has changed.
enqueueLostCountChanged_ :: Queue si qi sm qm so qo a -> Signal ()
enqueueLostCountChanged_ q =
  mapSignal (const ()) (enqueueLost q)
      
-- | Return the total number of input items that were stored.
--
-- See also 'enqueueStoreCountChanged' and 'enqueueStoreCountChanged_'.
enqueueStoreCount :: Queue si qi sm qm so qo a -> Event Int
enqueueStoreCount q =
  Event $ \p -> readIORef (enqueueStoreCountRef q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged :: Queue si qi sm qm so qo a -> Signal Int
enqueueStoreCountChanged q =
  mapSignalM (const $ enqueueStoreCount q) (enqueueStoreCountChanged_ q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged_ :: Queue si qi sm qm so qo a -> Signal ()
enqueueStoreCountChanged_ q =
  mapSignal (const ()) (enqueueStored q)
      
-- | Return the total number of requests for dequeueing the items,
-- not taking into account the failed attempts to dequeue immediately
-- without suspension.
--
-- See also 'dequeueCountChanged' and 'dequeueCountChanged_'.
dequeueCount :: Queue si qi sm qm so qo a -> Event Int
dequeueCount q =
  Event $ \p -> readIORef (dequeueCountRef q)
      
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged :: Queue si qi sm qm so qo a -> Signal Int
dequeueCountChanged q =
  mapSignalM (const $ dequeueCount q) (dequeueCountChanged_ q)
  
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged_ :: Queue si qi sm qm so qo a -> Signal ()
dequeueCountChanged_ q =
  mapSignal (const ()) (dequeueRequested q)
      
-- | Return the total number of output items that were actually dequeued.
--
-- See also 'dequeueExtractCountChanged' and 'dequeueExtractCountChanged_'.
dequeueExtractCount :: Queue si qi sm qm so qo a -> Event Int
dequeueExtractCount q =
  Event $ \p -> readIORef (dequeueExtractCountRef q)
      
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged :: Queue si qi sm qm so qo a -> Signal Int
dequeueExtractCountChanged q =
  mapSignalM (const $ dequeueExtractCount q) (dequeueExtractCountChanged_ q)
  
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged_ :: Queue si qi sm qm so qo a -> Signal ()
dequeueExtractCountChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the load factor: the queue size divided by its maximum size.
--
-- See also 'queueLoadFactorChanged' and 'queueLoadFactorChanged_'.
queueLoadFactor :: Queue si qi sm qm so qo a -> Event Double
queueLoadFactor q =
  Event $ \p ->
  do x <- readIORef (queueCountRef q)
     let y = queueMaxCount q
     return (fromIntegral x / fromIntegral y)
      
-- | Signal when the 'queueLoadFactor' property value has changed.
queueLoadFactorChanged :: Queue si qi sm qm so qo a -> Signal Double
queueLoadFactorChanged q =
  mapSignalM (const $ queueLoadFactor q) (queueLoadFactorChanged_ q)
  
-- | Signal when the 'queueLoadFactor' property value has changed.
queueLoadFactorChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueLoadFactorChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the rate of the input items that were enqueued: how many items
-- per time.
enqueueRate :: Queue si qi sm qm so qo a -> Event Double
enqueueRate q =
  Event $ \p ->
  do x <- readIORef (enqueueCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the items that were stored: how many items
-- per time.
enqueueStoreRate :: Queue si qi sm qm so qo a -> Event Double
enqueueStoreRate q =
  Event $ \p ->
  do x <- readIORef (enqueueStoreCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the requests for dequeueing the items: how many requests
-- per time. It does not include the failed attempts to dequeue immediately
-- without suspension.
dequeueRate :: Queue si qi sm qm so qo a -> Event Double
dequeueRate q =
  Event $ \p ->
  do x <- readIORef (dequeueCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the output items that were actually dequeued: how many items
-- per time.
dequeueExtractRate :: Queue si qi sm qm so qo a -> Event Double
dequeueExtractRate q =
  Event $ \p ->
  do x <- readIORef (dequeueExtractCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the wait time from the time at which the item was stored in the queue to
-- the time at which it was dequeued.
--
-- See also 'queueWaitTimeChanged' and 'queueWaitTimeChanged_'.
queueWaitTime :: Queue si qi sm qm so qo a -> Event (SamplingStats Double)
queueWaitTime q =
  Event $ \p -> readIORef (queueWaitTimeRef q)
      
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged :: Queue si qi sm qm so qo a -> Signal (SamplingStats Double)
queueWaitTimeChanged q =
  mapSignalM (const $ queueWaitTime q) (queueWaitTimeChanged_ q)
  
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the total wait time from the time at which the enqueueing operation
-- was initiated to the time at which the item was dequeued.
--
-- In some sense, @queueTotalWaitTime == queueInputWaitTime + queueWaitTime@.
--
-- See also 'queueTotalWaitTimeChanged' and 'queueTotalWaitTimeChanged_'.
queueTotalWaitTime :: Queue si qi sm qm so qo a -> Event (SamplingStats Double)
queueTotalWaitTime q =
  Event $ \p -> readIORef (queueTotalWaitTimeRef q)
      
-- | Signal when the 'queueTotalWaitTime' property value has changed.
queueTotalWaitTimeChanged :: Queue si qi sm qm so qo a -> Signal (SamplingStats Double)
queueTotalWaitTimeChanged q =
  mapSignalM (const $ queueTotalWaitTime q) (queueTotalWaitTimeChanged_ q)
  
-- | Signal when the 'queueTotalWaitTime' property value has changed.
queueTotalWaitTimeChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueTotalWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the enqueue wait time from the time at which the enqueueing operation
-- was initiated to the time at which the item was stored in the queue.
--
-- See also 'enqueueWaitTimeChanged' and 'enqueueWaitTimeChanged_'.
enqueueWaitTime :: Queue si qi sm qm so qo a -> Event (SamplingStats Double)
enqueueWaitTime q =
  Event $ \p -> readIORef (enqueueWaitTimeRef q)
      
-- | Signal when the 'enqueueWaitTime' property value has changed.
enqueueWaitTimeChanged :: Queue si qi sm qm so qo a -> Signal (SamplingStats Double)
enqueueWaitTimeChanged q =
  mapSignalM (const $ enqueueWaitTime q) (enqueueWaitTimeChanged_ q)
  
-- | Signal when the 'enqueueWaitTime' property value has changed.
enqueueWaitTimeChanged_ :: Queue si qi sm qm so qo a -> Signal ()
enqueueWaitTimeChanged_ q =
  mapSignal (const ()) (enqueueStored q)
      
-- | Return the dequeue wait time from the time at which the item was requested
-- for dequeueing to the time at which it was actually dequeued.
--
-- See also 'dequeueWaitTimeChanged' and 'dequeueWaitTimeChanged_'.
dequeueWaitTime :: Queue si qi sm qm so qo a -> Event (SamplingStats Double)
dequeueWaitTime q =
  Event $ \p -> readIORef (dequeueWaitTimeRef q)
      
-- | Signal when the 'dequeueWaitTime' property value has changed.
dequeueWaitTimeChanged :: Queue si qi sm qm so qo a -> Signal (SamplingStats Double)
dequeueWaitTimeChanged q =
  mapSignalM (const $ dequeueWaitTime q) (dequeueWaitTimeChanged_ q)
  
-- | Signal when the 'dequeueWaitTime' property value has changed.
dequeueWaitTimeChanged_ :: Queue si qi sm qm so qo a -> Signal ()
dequeueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)

-- | Return a long-term average queue rate calculated as
-- the average queue size divided by the average wait time.
--
-- This value may be less than the actual arrival rate as the queue is
-- finite and new arrivals may be locked while the queue remains full.
--
-- See also 'queueRateChanged' and 'queueRateChanged_'.
queueRate :: Queue si qi sm qm so qo a -> Event Double
queueRate q =
  Event $ \p ->
  do x <- readIORef (queueCountStatsRef q)
     y <- readIORef (queueWaitTimeRef q)
     return (timingStatsMean x / samplingStatsMean y) 
      
-- | Signal when the 'queueRate' property value has changed.
queueRateChanged :: Queue si qi sm qm so qo a -> Signal Double
queueRateChanged q =
  mapSignalM (const $ queueRate q) (queueRateChanged_ q)
      
-- | Signal when the 'queueRate' property value has changed.
queueRateChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueRateChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)

-- | Dequeue suspending the process if the queue is empty.
dequeue :: (DequeueStrategy si qi,
            DequeueStrategy sm qm,
            EnqueueStrategy so qo)
           => Queue si qi sm qm so qo a
           -- ^ the queue
           -> Process a
           -- ^ the dequeued value
dequeue q =
  do t <- liftEvent $ dequeueRequest q
     requestResource (dequeueRes q)
     liftEvent $ dequeueExtract q t
  
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
dequeueWithOutputPriority q po =
  do t <- liftEvent $ dequeueRequest q
     requestResourceWithPriority (dequeueRes q) po
     liftEvent $ dequeueExtract q t
  
-- | Try to dequeue immediately.
tryDequeue :: (DequeueStrategy si qi,
               DequeueStrategy sm qm)
              => Queue si qi sm qm so qo a
              -- ^ the queue
              -> Event (Maybe a)
              -- ^ the dequeued value of 'Nothing'
tryDequeue q =
  do x <- tryRequestResourceWithinEvent (dequeueRes q)
     if x 
       then do t <- dequeueRequest q
               fmap Just $ dequeueExtract q t
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
  do i <- liftEvent $ enqueueInitiate q a
     requestResource (enqueueRes q)
     liftEvent $ enqueueStore q i
     
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
enqueueWithInputPriority q pi a =
  do i <- liftEvent $ enqueueInitiate q a
     requestResourceWithPriority (enqueueRes q) pi
     liftEvent $ enqueueStore q i
     
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
enqueueWithStoringPriority q pm a =
  do i <- liftEvent $ enqueueInitiate q a
     requestResource (enqueueRes q)
     liftEvent $ enqueueStoreWithPriority q pm i
     
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
  do i <- liftEvent $ enqueueInitiate q a
     requestResourceWithPriority (enqueueRes q) pi
     liftEvent $ enqueueStoreWithPriority q pm i
     
-- | Try to enqueue the item. Return 'False' in the monad if the queue is full.
tryEnqueue :: (EnqueueStrategy sm qm,
               DequeueStrategy so qo)
              => Queue si qi sm qm so qo a
              -- ^ the queue
              -> a
              -- ^ the item which we try to enqueue
              -> Event Bool
tryEnqueue q a =
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x 
       then do enqueueInitiate q a >>= enqueueStore q
               return True
       else return False

-- | Try to enqueue with the storing priority the item. Return 'False' in
-- the monad if the queue is full.
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
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x 
       then do enqueueInitiate q a >>= enqueueStoreWithPriority q pm
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
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x
       then do enqueueInitiate q a >>= enqueueStore q
               return True
       else do enqueueDeny q a
               return False

-- | Try to enqueue with the storing priority the item. If the queue is full
-- then the item will be lost and 'False' will be returned.
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
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x
       then do enqueueInitiate q a >>= enqueueStoreWithPriority q pm
               return True
       else do enqueueDeny q a
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

-- | Try to enqueue with the storing priority the item. If the queue is full
-- then the item will be lost.
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

-- | Return a signal that notifies when the enqueuing operation is initiated.
enqueueInitiated :: Queue si qi sm qm so qo a -> Signal a
enqueueInitiated q = publishSignal (enqueueInitiatedSource q)

-- | Return a signal that notifies when the enqueuing operation is completed
-- and the item is stored in the internal memory of the queue.
enqueueStored :: Queue si qi sm qm so qo a -> Signal a
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
enqueueLost :: Queue si qi sm qm so qo a -> Signal a
enqueueLost q = publishSignal (enqueueLostSource q)

-- | Return a signal that notifies when the dequeuing operation was requested.
dequeueRequested :: Queue si qi sm qm so qo a -> Signal ()
dequeueRequested q = publishSignal (dequeueRequestedSource q)

-- | Return a signal that notifies when the item was extracted from the internal
-- storage of the queue and prepared for immediate receiving by the dequeuing process.
dequeueExtracted :: Queue si qi sm qm so qo a -> Signal a
dequeueExtracted q = publishSignal (dequeueExtractedSource q)

-- | Initiate the process of enqueuing the item.
enqueueInitiate :: Queue si qi sm qm so qo a
                   -- ^ the queue
                   -> a
                   -- ^ the item to be enqueued
                   -> Event (QueueItem a)
enqueueInitiate q a =
  Event $ \p ->
  do let t = pointTime p
     modifyIORef' (enqueueCountRef q) (+ 1)
     invokeEvent p $
       triggerSignal (enqueueInitiatedSource q) a
     return QueueItem { itemValue = a,
                        itemInputTime = t,
                        itemStoringTime = t  -- it will be updated soon
                      }

-- | Store the item.
enqueueStore :: (EnqueueStrategy sm qm,
                 DequeueStrategy so qo)
                => Queue si qi sm qm so qo a
                -- ^ the queue
                -> QueueItem a
                -- ^ the item to be stored
                -> Event ()
enqueueStore q i =
  Event $ \p ->
  do let i' = i { itemStoringTime = pointTime p }  -- now we have the actual time of storing
     invokeEvent p $
       strategyEnqueue (enqueueStoringStrategy q) (queueStore q) i'
     c <- readIORef (queueCountRef q)
     let c' = c + 1
         t  = pointTime p 
     c' `seq` writeIORef (queueCountRef q) c'
     modifyIORef' (queueCountStatsRef q) (addTimingStats t c')
     modifyIORef' (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       enqueueStat q i'
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i')

-- | Store with the priority the item.
enqueueStoreWithPriority :: (PriorityQueueStrategy sm qm pm,
                             DequeueStrategy so qo)
                            => Queue si qi sm qm so qo a
                            -- ^ the queue
                            -> pm
                            -- ^ the priority for storing
                            -> QueueItem a
                            -- ^ the item to be enqueued
                            -> Event ()
enqueueStoreWithPriority q pm i =
  Event $ \p ->
  do let i' = i { itemStoringTime = pointTime p }  -- now we have the actual time of storing
     invokeEvent p $
       strategyEnqueueWithPriority (enqueueStoringStrategy q) (queueStore q) pm i'
     c <- readIORef (queueCountRef q)
     let c' = c + 1
         t  = pointTime p
     c' `seq` writeIORef (queueCountRef q) c'
     modifyIORef' (queueCountStatsRef q) (addTimingStats t c')
     modifyIORef' (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       enqueueStat q i'
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i')

-- | Deny the enqueuing.
enqueueDeny :: Queue si qi sm qm so qo a
               -- ^ the queue
               -> a
               -- ^ the item to be denied
               -> Event ()
enqueueDeny q a =
  Event $ \p ->
  do modifyIORef' (enqueueLostCountRef q) $ (+) 1
     invokeEvent p $
       triggerSignal (enqueueLostSource q) a

-- | Update the statistics for the input wait time of the enqueuing operation.
enqueueStat :: Queue si qi sm qm so qo a
               -- ^ the queue
               -> QueueItem a
               -- ^ the item and its input time
               -> Event ()
               -- ^ the action of updating the statistics
enqueueStat q i =
  Event $ \p ->
  do let t0 = itemInputTime i
         t1 = itemStoringTime i
     modifyIORef' (enqueueWaitTimeRef q) $
       addSamplingStats (t1 - t0)

-- | Accept the dequeuing request and return the current simulation time.
dequeueRequest :: Queue si qi sm qm so qo a
                 -- ^ the queue
                 -> Event Double
                 -- ^ the current time
dequeueRequest q =
  Event $ \p ->
  do modifyIORef' (dequeueCountRef q) (+ 1)
     invokeEvent p $
       triggerSignal (dequeueRequestedSource q) ()
     return $ pointTime p 

-- | Extract an item for the dequeuing request.  
dequeueExtract :: (DequeueStrategy si qi,
                   DequeueStrategy sm qm)
                  => Queue si qi sm qm so qo a
                  -- ^ the queue
                  -> Double
                  -- ^ the time of the dequeuing request
                  -> Event a
                  -- ^ the dequeued value
dequeueExtract q t' =
  Event $ \p ->
  do i <- invokeEvent p $
          strategyDequeue (enqueueStoringStrategy q) (queueStore q)
     c <- readIORef (queueCountRef q)
     let c' = c - 1
         t  = pointTime p
     c' `seq` writeIORef (queueCountRef q) c'
     modifyIORef' (queueCountStatsRef q) (addTimingStats t c')
     modifyIORef' (dequeueExtractCountRef q) (+ 1)
     invokeEvent p $
       dequeueStat q t' i
     invokeEvent p $
       releaseResourceWithinEvent (enqueueRes q)
     invokeEvent p $
       triggerSignal (dequeueExtractedSource q) (itemValue i)
     return $ itemValue i

-- | Update the statistics for the output wait time of the dequeuing operation
-- and the wait time of storing in the queue.
dequeueStat :: Queue si qi sm qm so qo a
               -- ^ the queue
               -> Double
               -- ^ the time of the dequeuing request
               -> QueueItem a
               -- ^ the item and its input time
               -> Event ()
               -- ^ the action of updating the statistics
dequeueStat q t' i =
  Event $ \p ->
  do let t0 = itemInputTime i
         t1 = itemStoringTime i
         t  = pointTime p
     modifyIORef' (dequeueWaitTimeRef q) $
       addSamplingStats (t - t')
     modifyIORef' (queueTotalWaitTimeRef q) $
       addSamplingStats (t - t0)
     modifyIORef' (queueWaitTimeRef q) $
       addSamplingStats (t - t1)

-- | Wait while the queue is full.
waitWhileFullQueue :: Queue si qi sm qm so qo a -> Process ()
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
queueChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueChanged_ q =
  mapSignal (const ()) (enqueueInitiated q) <>
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (enqueueLost q) <>
  dequeueRequested q <>
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the summary for the queue with desciption of its
-- properties and activities using the specified indent.
queueSummary :: (Show si, Show sm, Show so) => Queue si qi sm qm so qo a -> Int -> Event ShowS
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
