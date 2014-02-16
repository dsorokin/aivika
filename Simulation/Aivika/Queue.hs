
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
        enqueueCount,
        queueLostCount,
        queueStoreCount,
        queueOutputRequestCount,
        queueOutputCount,
        queueLoadFactor,
        queueInputRate,
        queueStoreRate,
        queueOutputRequestRate,
        queueOutputRate,
        queueWaitTime,
        queueTotalWaitTime,
        queueInputWaitTime,
        queueOutputWaitTime,
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
        queueLostCountChanged,
        queueLostCountChanged_,
        queueStoreCountChanged,
        queueStoreCountChanged_,
        queueOutputRequestCountChanged,
        queueOutputRequestCountChanged_,
        queueOutputCountChanged,
        queueOutputCountChanged_,
        queueLoadFactorChanged,
        queueLoadFactorChanged_,
        queueWaitTimeChanged,
        queueWaitTimeChanged_,
        queueTotalWaitTimeChanged,
        queueTotalWaitTimeChanged_,
        queueInputWaitTimeChanged,
        queueInputWaitTimeChanged_,
        queueOutputWaitTimeChanged,
        queueOutputWaitTimeChanged_,
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
import Simulation.Aivika.Stream
import Simulation.Aivika.Processor

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
          enqueueCountRef :: IORef Int,
          queueLostCountRef :: IORef Int,
          queueStoreCountRef :: IORef Int,
          queueOutputRequestCountRef :: IORef Int,
          queueOutputCountRef :: IORef Int,
          queueWaitTimeRef :: IORef (SamplingStats Double),
          queueTotalWaitTimeRef :: IORef (SamplingStats Double),
          queueInputWaitTimeRef :: IORef (SamplingStats Double),
          queueOutputWaitTimeRef :: IORef (SamplingStats Double),
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
newFCFSQueue :: Int -> Simulation (FCFSQueue a)  
newFCFSQueue = newQueue FCFS FCFS FCFS
  
-- | Create a new LCFS queue with the specified capacity.  
newLCFSQueue :: Int -> Simulation (LCFSQueue a)  
newLCFSQueue = newQueue FCFS LCFS FCFS
  
-- | Create a new SIRO queue with the specified capacity.  
newSIROQueue :: Int -> Simulation (SIROQueue a)  
newSIROQueue = newQueue FCFS SIRO FCFS
  
-- | Create a new priority queue with the specified capacity.  
newPriorityQueue :: Int -> Simulation (PriorityQueue a)  
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
            -> Simulation (Queue si qi sm qm so qo a)  
newQueue si sm so count =
  do i  <- liftIO $ newIORef 0
     ci <- liftIO $ newIORef 0
     cl <- liftIO $ newIORef 0
     cm <- liftIO $ newIORef 0
     cr <- liftIO $ newIORef 0
     co <- liftIO $ newIORef 0
     ri <- newResourceWithMaxCount si count (Just count)
     qm <- newStrategyQueue sm
     ro <- newResourceWithMaxCount so 0 (Just count)
     w  <- liftIO $ newIORef mempty
     wt <- liftIO $ newIORef mempty
     wi <- liftIO $ newIORef mempty
     wo <- liftIO $ newIORef mempty 
     s1 <- newSignalSource
     s2 <- newSignalSource
     s3 <- newSignalSource
     s4 <- newSignalSource
     s5 <- newSignalSource
     return Queue { queueMaxCount = count,
                    enqueueStrategy = si,
                    enqueueStoringStrategy = sm,
                    dequeueStrategy = so,
                    enqueueRes = ri,
                    queueStore = qm,
                    dequeueRes = ro,
                    queueCountRef = i,
                    enqueueCountRef = ci,
                    queueLostCountRef = cl,
                    queueStoreCountRef = cm,
                    queueOutputRequestCountRef = cr,
                    queueOutputCountRef = co,
                    queueWaitTimeRef = w,
                    queueTotalWaitTimeRef = wt,
                    queueInputWaitTimeRef = wi,
                    queueOutputWaitTimeRef = wo,
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

-- | Return the queue size.
--
-- See also 'queueCountChanged' and 'queueCountChanged_'.
queueCount :: Queue si qi sm qm so qo a -> Event Int
queueCount q =
  Event $ \p -> readIORef (queueCountRef q)
  
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
-- See also 'queueLostCountChanged' and 'queueLostCountChanged_'.
queueLostCount :: Queue si qi sm qm so qo a -> Event Int
queueLostCount q =
  Event $ \p -> readIORef (queueLostCountRef q)
  
-- | Signal when the 'queueLostCount' property value has changed.
queueLostCountChanged :: Queue si qi sm qm so qo a -> Signal Int
queueLostCountChanged q =
  mapSignalM (const $ queueLostCount q) (queueLostCountChanged_ q)
  
-- | Signal when the 'queueLostCount' property value has changed.
queueLostCountChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueLostCountChanged_ q =
  mapSignal (const ()) (enqueueLost q)
      
-- | Return the total number of input items that were stored.
--
-- See also 'queueStoreCountChanged' and 'queueStoreCountChanged_'.
queueStoreCount :: Queue si qi sm qm so qo a -> Event Int
queueStoreCount q =
  Event $ \p -> readIORef (queueStoreCountRef q)
  
-- | Signal when the 'queueStoreCount' property value has changed.
queueStoreCountChanged :: Queue si qi sm qm so qo a -> Signal Int
queueStoreCountChanged q =
  mapSignalM (const $ queueStoreCount q) (queueStoreCountChanged_ q)
  
-- | Signal when the 'queueStoreCount' property value has changed.
queueStoreCountChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueStoreCountChanged_ q =
  mapSignal (const ()) (enqueueStored q)
      
-- | Return the total number of requests for dequeueing the items,
-- not taking into account the attempts to dequeue immediately
-- without suspension.
--
-- See also 'queueOutputRequestCountChanged' and 'queueOutputRequestCountChanged_'.
queueOutputRequestCount :: Queue si qi sm qm so qo a -> Event Int
queueOutputRequestCount q =
  Event $ \p -> readIORef (queueOutputRequestCountRef q)
      
-- | Signal when the 'queueOutputRequestCount' property value has changed.
queueOutputRequestCountChanged :: Queue si qi sm qm so qo a -> Signal Int
queueOutputRequestCountChanged q =
  mapSignalM (const $ queueOutputRequestCount q) (queueOutputRequestCountChanged_ q)
  
-- | Signal when the 'queueOutputRequestCount' property value has changed.
queueOutputRequestCountChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueOutputRequestCountChanged_ q =
  mapSignal (const ()) (dequeueRequested q)
      
-- | Return the total number of output items that were dequeued.
--
-- See also 'queueOutputCountChanged' and 'queueOutputCountChanged_'.
queueOutputCount :: Queue si qi sm qm so qo a -> Event Int
queueOutputCount q =
  Event $ \p -> readIORef (queueOutputCountRef q)
      
-- | Signal when the 'queueOutputCount' property value has changed.
queueOutputCountChanged :: Queue si qi sm qm so qo a -> Signal Int
queueOutputCountChanged q =
  mapSignalM (const $ queueOutputCount q) (queueOutputCountChanged_ q)
  
-- | Signal when the 'queueOutputCount' property value has changed.
queueOutputCountChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueOutputCountChanged_ q =
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
queueInputRate :: Queue si qi sm qm so qo a -> Event Double
queueInputRate q =
  Event $ \p ->
  do x <- readIORef (enqueueCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the items that were stored: how many items
-- per time.
queueStoreRate :: Queue si qi sm qm so qo a -> Event Double
queueStoreRate q =
  Event $ \p ->
  do x <- readIORef (queueStoreCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the requests for dequeueing the items: how many requests
-- per time. It does not include the attempts to dequeue immediately
-- without suspension.
queueOutputRequestRate :: Queue si qi sm qm so qo a -> Event Double
queueOutputRequestRate q =
  Event $ \p ->
  do x <- readIORef (queueOutputRequestCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the output items that were dequeued: how many items
-- per time.
queueOutputRate :: Queue si qi sm qm so qo a -> Event Double
queueOutputRate q =
  Event $ \p ->
  do x <- readIORef (queueOutputCountRef q)
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
      
-- | Return the input wait time from the time at which the enqueueing operation
-- was initiated to the time at which the item was stored in the queue.
--
-- See also 'queueInputWaitTimeChanged' and 'queueInputWaitTimeChanged_'.
queueInputWaitTime :: Queue si qi sm qm so qo a -> Event (SamplingStats Double)
queueInputWaitTime q =
  Event $ \p -> readIORef (queueInputWaitTimeRef q)
      
-- | Signal when the 'queueInputWaitTime' property value has changed.
queueInputWaitTimeChanged :: Queue si qi sm qm so qo a -> Signal (SamplingStats Double)
queueInputWaitTimeChanged q =
  mapSignalM (const $ queueInputWaitTime q) (queueInputWaitTimeChanged_ q)
  
-- | Signal when the 'queueInputWaitTime' property value has changed.
queueInputWaitTimeChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueInputWaitTimeChanged_ q =
  mapSignal (const ()) (enqueueStored q)
      
-- | Return the output wait time from the time at which the item was requested
-- for dequeueing to the time at which it was actually dequeued.
--
-- See also 'queueOutputWaitTimeChanged' and 'queueOutputWaitTimeChanged_'.
queueOutputWaitTime :: Queue si qi sm qm so qo a -> Event (SamplingStats Double)
queueOutputWaitTime q =
  Event $ \p -> readIORef (queueOutputWaitTimeRef q)
      
-- | Signal when the 'queueOutputWaitTime' property value has changed.
queueOutputWaitTimeChanged :: Queue si qi sm qm so qo a -> Signal (SamplingStats Double)
queueOutputWaitTimeChanged q =
  mapSignalM (const $ queueOutputWaitTime q) (queueOutputWaitTimeChanged_ q)
  
-- | Signal when the 'queueOutputWaitTime' property value has changed.
queueOutputWaitTimeChanged_ :: Queue si qi sm qm so qo a -> Signal ()
queueOutputWaitTimeChanged_ q =
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
     modifyIORef' (queueCountRef q) (+ 1)
     modifyIORef' (queueStoreCountRef q) (+ 1)
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
     modifyIORef' (queueCountRef q) (+ 1)
     modifyIORef' (queueStoreCountRef q) (+ 1)
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
  do modifyIORef' (queueLostCountRef q) $ (+) 1
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
     modifyIORef' (queueInputWaitTimeRef q) $
       addSamplingStats (t1 - t0)

-- | Accept the dequeuing request and return the current simulation time.
dequeueRequest :: Queue si qi sm qm so qo a
                 -- ^ the queue
                 -> Event Double
                 -- ^ the current time
dequeueRequest q =
  Event $ \p ->
  do modifyIORef' (queueOutputRequestCountRef q) (+ 1)
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
     modifyIORef' (queueCountRef q) (+ (- 1))
     modifyIORef' (queueOutputCountRef q) (+ 1)
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
     modifyIORef' (queueOutputWaitTimeRef q) $
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
     enqueueCount <- enqueueCount q
     lostCount <- queueLostCount q
     storeCount <- queueStoreCount q
     outputRequestCount <- queueOutputRequestCount q
     outputCount <- queueOutputCount q
     loadFactor <- queueLoadFactor q
     inputRate <- queueInputRate q
     storeRate <- queueStoreRate q
     outputRequestRate <- queueOutputRequestRate q
     outputRate <- queueOutputRate q
     waitTime <- queueWaitTime q
     totalWaitTime <- queueTotalWaitTime q
     inputWaitTime <- queueInputWaitTime q
     outputWaitTime <- queueOutputWaitTime q
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
       showString "the enqueue count (number of the input items that were enqueued) = " .
       shows enqueueCount .
       showString "\n" .
       showString tab .
       showString "the lost count (number of the lost items) = " .
       shows lostCount .
       showString "\n" .
       showString tab .
       showString "the store count (number of the input items that were stored) = " .
       shows storeCount .
       showString "\n" .
       showString tab .
       showString "the output request count (number of requests for dequeueing an item) = " .
       shows outputRequestCount .
       showString "\n" .
       showString tab .
       showString "the output count (number of the output items that were dequeued) = " .
       shows outputCount .
       showString "\n" .
       showString tab .
       showString "the load factor (size / max. capacity) = " .
       shows loadFactor .
       showString "\n" .
       showString tab .
       showString "the input rate (how many input items were enqueued per time) = " .
       shows inputRate .
       showString "\n" .
       showString tab .
       showString "the store rate (how many input items were stored per time) = " .
       shows storeRate .
       showString "\n" .
       showString tab .
       showString "the output request rate (how many requests for dequeueing per time) = " .
       shows outputRequestRate .
       showString "\n" .
       showString tab .
       showString "the output rate (how many output items were dequeued per time) = " .
       shows outputRate .
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
       showString "the input wait time (when the enqueueing was initiated -> when was stored) = \n\n" .
       samplingStatsSummary inputWaitTime (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "the output wait time (when was requested for dequeueing -> when was dequeued) = \n\n" .
       samplingStatsSummary outputWaitTime (2 + indent)
