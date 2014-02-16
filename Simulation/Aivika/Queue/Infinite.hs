
-- |
-- Module     : Simulation.Aivika.Queue.Infinite
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines an infinite queue that can use the specified strategies.
--
module Simulation.Aivika.Queue.Infinite
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
        enqueueStoreCount,
        dequeueCount,
        dequeueExtractCount,
        queueStoreRate,
        queueOutputRequestRate,
        queueOutputRate,
        queueWaitTime,
        queueOutputWaitTime,
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
        queueOutputWaitTimeChanged,
        queueOutputWaitTimeChanged_,
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
  Queue FCFS DLL.DoubleLinkedList FCFS DLL.DoubleLinkedList a

-- | A type synonym for the ordinary LIFO queue also known as the LCFS
-- (Last Come - First Serviced) queue.
type LCFSQueue a =
  Queue LCFS DLL.DoubleLinkedList FCFS DLL.DoubleLinkedList a

-- | A type synonym for the SIRO (Serviced in Random Order) queue.
type SIROQueue a =
  Queue SIRO V.Vector FCFS DLL.DoubleLinkedList a

-- | A type synonym for the queue with static priorities applied when
-- storing the elements in the queue.
type PriorityQueue a =
  Queue StaticPriorities PQ.PriorityQueue FCFS DLL.DoubleLinkedList a

-- | Represents an infinite queue using the specified strategies for
-- internal storing (in memory), @sm@, and dequeueing (output), @so@, where @a@ denotes
-- the type of items stored in the queue. Types @qm@ and @qo@ are
-- determined automatically and you should not care about them - they
-- are dependent types.
data Queue sm qm so qo a =
  Queue { enqueueStoringStrategy :: sm,
          -- ^ The strategy applied when storing (in memory) items in the queue.
          dequeueStrategy :: so,
          -- ^ The strategy applied to the dequeueing (output) processes.
          queueStore :: qm (QueueItem a),
          dequeueRes :: Resource so qo,
          queueCountRef :: IORef Int,
          enqueueStoreCountRef :: IORef Int,
          dequeueCountRef :: IORef Int,
          dequeueExtractCountRef :: IORef Int,
          queueWaitTimeRef :: IORef (SamplingStats Double),
          queueOutputWaitTimeRef :: IORef (SamplingStats Double),
          enqueueStoredSource :: SignalSource a,
          dequeueRequestedSource :: SignalSource (),
          dequeueExtractedSource :: SignalSource a }

-- | Stores the item and a time of its enqueuing. 
data QueueItem a =
  QueueItem { itemValue :: a,
              -- ^ Return the item value.
              itemStoringTime :: Double
              -- ^ Return the time of storing in the queue.
            }
  
-- | Create a new infinite FCFS queue.  
newFCFSQueue :: Simulation (FCFSQueue a)  
newFCFSQueue = newQueue FCFS FCFS
  
-- | Create a new infinite LCFS queue.  
newLCFSQueue :: Simulation (LCFSQueue a)  
newLCFSQueue = newQueue LCFS FCFS
  
-- | Create a new infinite SIRO queue.  
newSIROQueue :: Simulation (SIROQueue a)  
newSIROQueue = newQueue SIRO FCFS
  
-- | Create a new infinite priority queue.  
newPriorityQueue :: Simulation (PriorityQueue a)  
newPriorityQueue = newQueue StaticPriorities FCFS
  
-- | Create a new infinite queue with the specified strategies.  
newQueue :: (QueueStrategy sm qm,
             QueueStrategy so qo) =>
            sm
            -- ^ the strategy applied when storing items in the queue
            -> so
            -- ^ the strategy applied to the dequeueing (output) processes when the queue is empty
            -> Simulation (Queue sm qm so qo a)  
newQueue sm so =
  do i  <- liftIO $ newIORef 0
     cm <- liftIO $ newIORef 0
     cr <- liftIO $ newIORef 0
     co <- liftIO $ newIORef 0
     qm <- newStrategyQueue sm
     ro <- newResourceWithMaxCount so 0 Nothing
     w  <- liftIO $ newIORef mempty
     wo <- liftIO $ newIORef mempty 
     s3 <- newSignalSource
     s4 <- newSignalSource
     s5 <- newSignalSource
     return Queue { enqueueStoringStrategy = sm,
                    dequeueStrategy = so,
                    queueStore = qm,
                    dequeueRes = ro,
                    queueCountRef = i,
                    enqueueStoreCountRef = cm,
                    dequeueCountRef = cr,
                    dequeueExtractCountRef = co,
                    queueWaitTimeRef = w,
                    queueOutputWaitTimeRef = wo,
                    enqueueStoredSource = s3,
                    dequeueRequestedSource = s4,
                    dequeueExtractedSource = s5 }

-- | Test whether the queue is empty.
--
-- See also 'queueNullChanged' and 'queueNullChanged_'.
queueNull :: Queue sm qm so qo a -> Event Bool
queueNull q =
  Event $ \p ->
  do n <- readIORef (queueCountRef q)
     return (n == 0)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged :: Queue sm qm so qo a -> Signal Bool
queueNullChanged q =
  mapSignalM (const $ queueNull q) (queueNullChanged_ q)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged_ :: Queue sm qm so qo a -> Signal ()
queueNullChanged_ = queueCountChanged_

-- | Return the queue size.
--
-- See also 'queueCountChanged' and 'queueCountChanged_'.
queueCount :: Queue sm qm so qo a -> Event Int
queueCount q =
  Event $ \p -> readIORef (queueCountRef q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged :: Queue sm qm so qo a -> Signal Int
queueCountChanged q =
  mapSignalM (const $ queueCount q) (queueCountChanged_ q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged_ :: Queue sm qm so qo a -> Signal ()
queueCountChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the total number of input items that were stored.
--
-- See also 'enqueueStoreCountChanged' and 'enqueueStoreCountChanged_'.
enqueueStoreCount :: Queue sm qm so qo a -> Event Int
enqueueStoreCount q =
  Event $ \p -> readIORef (enqueueStoreCountRef q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged :: Queue sm qm so qo a -> Signal Int
enqueueStoreCountChanged q =
  mapSignalM (const $ enqueueStoreCount q) (enqueueStoreCountChanged_ q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged_ :: Queue sm qm so qo a -> Signal ()
enqueueStoreCountChanged_ q =
  mapSignal (const ()) (enqueueStored q)
      
-- | Return the total number of requests for dequeueing the items,
-- not taking into account the failed attempts to dequeue immediately
-- without suspension.
--
-- See also 'dequeueCountChanged' and 'dequeueCountChanged_'.
dequeueCount :: Queue sm qm so qo a -> Event Int
dequeueCount q =
  Event $ \p -> readIORef (dequeueCountRef q)
      
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged :: Queue sm qm so qo a -> Signal Int
dequeueCountChanged q =
  mapSignalM (const $ dequeueCount q) (dequeueCountChanged_ q)
  
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged_ :: Queue sm qm so qo a -> Signal ()
dequeueCountChanged_ q =
  mapSignal (const ()) (dequeueRequested q)
      
-- | Return the total number of output items that were actually dequeued.
--
-- See also 'dequeueExtractCountChanged' and 'dequeueExtractCountChanged_'.
dequeueExtractCount :: Queue sm qm so qo a -> Event Int
dequeueExtractCount q =
  Event $ \p -> readIORef (dequeueExtractCountRef q)
      
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged :: Queue sm qm so qo a -> Signal Int
dequeueExtractCountChanged q =
  mapSignalM (const $ dequeueExtractCount q) (dequeueExtractCountChanged_ q)
  
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged_ :: Queue sm qm so qo a -> Signal ()
dequeueExtractCountChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the rate of the items that were stored: how many items
-- per time.
queueStoreRate :: Queue sm qm so qo a -> Event Double
queueStoreRate q =
  Event $ \p ->
  do x <- readIORef (enqueueStoreCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the requests for dequeueing the items: how many requests
-- per time. It does not include the attempts to dequeue immediately
-- without suspension.
queueOutputRequestRate :: Queue sm qm so qo a -> Event Double
queueOutputRequestRate q =
  Event $ \p ->
  do x <- readIORef (dequeueCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the output items that were dequeued: how many items
-- per time.
queueOutputRate :: Queue sm qm so qo a -> Event Double
queueOutputRate q =
  Event $ \p ->
  do x <- readIORef (dequeueExtractCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the wait time from the time at which the item was stored in the queue to
-- the time at which it was dequeued.
--
-- See also 'queueWaitTimeChanged' and 'queueWaitTimeChanged_'.
queueWaitTime :: Queue sm qm so qo a -> Event (SamplingStats Double)
queueWaitTime q =
  Event $ \p -> readIORef (queueWaitTimeRef q)
      
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged :: Queue sm qm so qo a -> Signal (SamplingStats Double)
queueWaitTimeChanged q =
  mapSignalM (const $ queueWaitTime q) (queueWaitTimeChanged_ q)
  
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged_ :: Queue sm qm so qo a -> Signal ()
queueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the output wait time from the time at which the item was requested
-- for dequeueing to the time at which it was actually dequeued.
--
-- See also 'queueOutputWaitTimeChanged' and 'queueOutputWaitTimeChanged_'.
queueOutputWaitTime :: Queue sm qm so qo a -> Event (SamplingStats Double)
queueOutputWaitTime q =
  Event $ \p -> readIORef (queueOutputWaitTimeRef q)
      
-- | Signal when the 'queueOutputWaitTime' property value has changed.
queueOutputWaitTimeChanged :: Queue sm qm so qo a -> Signal (SamplingStats Double)
queueOutputWaitTimeChanged q =
  mapSignalM (const $ queueOutputWaitTime q) (queueOutputWaitTimeChanged_ q)
  
-- | Signal when the 'queueOutputWaitTime' property value has changed.
queueOutputWaitTimeChanged_ :: Queue sm qm so qo a -> Signal ()
queueOutputWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)
  
-- | Dequeue suspending the process if the queue is empty.
dequeue :: (DequeueStrategy sm qm,
            EnqueueStrategy so qo)
           => Queue sm qm so qo a
           -- ^ the queue
           -> Process a
           -- ^ the dequeued value
dequeue q =
  do t <- liftEvent $ dequeueRequest q
     requestResource (dequeueRes q)
     liftEvent $ dequeueExtract q t
  
-- | Dequeue with the output priority suspending the process if the queue is empty.
dequeueWithOutputPriority :: (DequeueStrategy sm qm,
                              PriorityQueueStrategy so qo po)
                             => Queue sm qm so qo a
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
tryDequeue :: DequeueStrategy sm qm
              => Queue sm qm so qo a
              -- ^ the queue
              -> Event (Maybe a)
              -- ^ the dequeued value of 'Nothing'
tryDequeue q =
  do x <- tryRequestResourceWithinEvent (dequeueRes q)
     if x 
       then do t <- dequeueRequest q
               fmap Just $ dequeueExtract q t
       else return Nothing

-- | Enqueue the item.  
enqueue :: (EnqueueStrategy sm qm,
            DequeueStrategy so qo)
           => Queue sm qm so qo a
           -- ^ the queue
           -> a
           -- ^ the item to enqueue
           -> Event ()
enqueue = enqueueStore
     
-- | Enqueue with the storing priority the item.  
enqueueWithStoringPriority :: (PriorityQueueStrategy sm qm pm,
                               DequeueStrategy so qo)
                              => Queue sm qm so qo a
                              -- ^ the queue
                              -> pm
                              -- ^ the priority for storing
                              -> a
                              -- ^ the item to enqueue
                              -> Event ()
enqueueWithStoringPriority = enqueueStoreWithPriority

-- | Return a signal that notifies when the enqueued item
-- is stored in the internal memory of the queue.
enqueueStored :: Queue sm qm so qo a -> Signal a
enqueueStored q = publishSignal (enqueueStoredSource q)

-- | Return a signal that notifies when the dequeuing operation was requested.
dequeueRequested :: Queue sm qm so qo a -> Signal ()
dequeueRequested q = publishSignal (dequeueRequestedSource q)

-- | Return a signal that notifies when the item was extracted from the internal
-- storage of the queue and prepared for immediate receiving by the dequeuing process.
dequeueExtracted :: Queue sm qm so qo a -> Signal a
dequeueExtracted q = publishSignal (dequeueExtractedSource q)

-- | Store the item.
enqueueStore :: (EnqueueStrategy sm qm,
                 DequeueStrategy so qo)
                => Queue sm qm so qo a
                -- ^ the queue
                -> a
                -- ^ the item to be stored
                -> Event ()
enqueueStore q a =
  Event $ \p ->
  do let i = QueueItem { itemValue = a,
                         itemStoringTime = pointTime p }
     invokeEvent p $
       strategyEnqueue (enqueueStoringStrategy q) (queueStore q) i
     modifyIORef' (queueCountRef q) (+ 1)
     modifyIORef' (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i)

-- | Store with the priority the item.
enqueueStoreWithPriority :: (PriorityQueueStrategy sm qm pm,
                             DequeueStrategy so qo)
                            => Queue sm qm so qo a
                            -- ^ the queue
                            -> pm
                            -- ^ the priority for storing
                            -> a
                            -- ^ the item to be enqueued
                            -> Event ()
enqueueStoreWithPriority q pm a =
  Event $ \p ->
  do let i = QueueItem { itemValue = a,
                         itemStoringTime = pointTime p }
     invokeEvent p $
       strategyEnqueueWithPriority (enqueueStoringStrategy q) (queueStore q) pm i
     modifyIORef' (queueCountRef q) (+ 1)
     modifyIORef' (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i)

-- | Accept the dequeuing request and return the current simulation time.
dequeueRequest :: Queue sm qm so qo a
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
dequeueExtract :: DequeueStrategy sm qm
                  => Queue sm qm so qo a
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
     modifyIORef' (dequeueExtractCountRef q) (+ 1)
     invokeEvent p $
       dequeueStat q t' i
     invokeEvent p $
       triggerSignal (dequeueExtractedSource q) (itemValue i)
     return $ itemValue i

-- | Update the statistics for the output wait time of the dequeuing operation
-- and the wait time of storing in the queue.
dequeueStat :: Queue sm qm so qo a
               -- ^ the queue
               -> Double
               -- ^ the time of the dequeuing request
               -> QueueItem a
               -- ^ the item and its input time
               -> Event ()
               -- ^ the action of updating the statistics
dequeueStat q t' i =
  Event $ \p ->
  do let t1 = itemStoringTime i
         t  = pointTime p
     modifyIORef' (queueOutputWaitTimeRef q) $
       addSamplingStats (t - t')
     modifyIORef' (queueWaitTimeRef q) $
       addSamplingStats (t - t1)

-- | Signal whenever any property of the queue changes.
--
-- The property must have the corresponded signal. There are also characteristics
-- similar to the properties but that have no signals. As a rule, such characteristics
-- already depend on the simulation time and therefore they may change at any
-- time point.
queueChanged_ :: Queue sm qm so qo a -> Signal ()
queueChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  dequeueRequested q <>
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the summary for the queue with desciption of its
-- properties and activities using the specified indent.
queueSummary :: (Show sm, Show so) => Queue sm qm so qo a -> Int -> Event ShowS
queueSummary q indent =
  do let sm = enqueueStoringStrategy q
         so = dequeueStrategy q
     null <- queueNull q
     count <- queueCount q
     enqueueStoreCount <- enqueueStoreCount q
     dequeueCount <- dequeueCount q
     dequeueExtractCount <- dequeueExtractCount q
     storeRate <- queueStoreRate q
     outputRequestRate <- queueOutputRequestRate q
     outputRate <- queueOutputRate q
     waitTime <- queueWaitTime q
     outputWaitTime <- queueOutputWaitTime q
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
       showString "size = " .
       shows count .
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
       showString "the output wait time (when was requested for dequeueing -> when was dequeued) = \n\n" .
       samplingStatsSummary outputWaitTime (2 + indent)
