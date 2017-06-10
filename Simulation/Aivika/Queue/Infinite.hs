
-- |
-- Module     : Simulation.Aivika.Queue.Infinite
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
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
        queueDelete,
        queueDelete_,
        queueDeleteBy,
        queueDeleteBy_,
        queueContains,
        queueContainsBy,
        clearQueue,
        -- * Statistics Reset
        resetQueue,
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
import Data.Maybe

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.Signal
import Simulation.Aivika.Resource.Base
import Simulation.Aivika.QueueStrategy
import Simulation.Aivika.Statistics

import qualified Simulation.Aivika.DoubleLinkedList as DLL 
import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.PriorityQueue as PQ

-- | A type synonym for the ordinary FIFO queue also known as the FCFS
-- (First Come - First Serviced) queue.
type FCFSQueue a = Queue FCFS FCFS a

-- | A type synonym for the ordinary LIFO queue also known as the LCFS
-- (Last Come - First Serviced) queue.
type LCFSQueue a = Queue LCFS FCFS a

-- | A type synonym for the SIRO (Serviced in Random Order) queue.
type SIROQueue a = Queue SIRO FCFS a

-- | A type synonym for the queue with static priorities applied when
-- storing the elements in the queue.
type PriorityQueue a = Queue StaticPriorities FCFS a

-- | Represents an infinite queue using the specified strategies for
-- internal storing (in memory), @sm@, and dequeueing (output), @so@, where @a@ denotes
-- the type of items stored in the queue.
data Queue sm so a =
  Queue { enqueueStoringStrategy :: sm,
          -- ^ The strategy applied when storing (in memory) items in the queue.
          dequeueStrategy :: so,
          -- ^ The strategy applied to the dequeueing (output) processes.
          queueStore :: StrategyQueue sm (QueueItem a),
          dequeueRes :: Resource so,
          queueCountRef :: IORef Int,
          queueCountStatsRef :: IORef (TimingStats Int),
          enqueueStoreCountRef :: IORef Int,
          dequeueCountRef :: IORef Int,
          dequeueExtractCountRef :: IORef Int,
          queueWaitTimeRef :: IORef (SamplingStats Double),
          dequeueWaitTimeRef :: IORef (SamplingStats Double),
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
newFCFSQueue :: Event (FCFSQueue a)  
newFCFSQueue = newQueue FCFS FCFS
  
-- | Create a new infinite LCFS queue.  
newLCFSQueue :: Event (LCFSQueue a)  
newLCFSQueue = newQueue LCFS FCFS
  
-- | Create a new infinite SIRO queue.  
newSIROQueue :: Event (SIROQueue a)  
newSIROQueue = newQueue SIRO FCFS
  
-- | Create a new infinite priority queue.  
newPriorityQueue :: Event (PriorityQueue a)  
newPriorityQueue = newQueue StaticPriorities FCFS
  
-- | Create a new infinite queue with the specified strategies.  
newQueue :: (QueueStrategy sm,
             QueueStrategy so) =>
            sm
            -- ^ the strategy applied when storing items in the queue
            -> so
            -- ^ the strategy applied to the dequeueing (output) processes when the queue is empty
            -> Event (Queue sm so a)  
newQueue sm so =
  do t  <- liftDynamics time
     i  <- liftIO $ newIORef 0
     is <- liftIO $ newIORef $ returnTimingStats t 0
     cm <- liftIO $ newIORef 0
     cr <- liftIO $ newIORef 0
     co <- liftIO $ newIORef 0
     qm <- liftSimulation $ newStrategyQueue sm
     ro <- liftSimulation $ newResourceWithMaxCount so 0 Nothing
     w  <- liftIO $ newIORef mempty
     wo <- liftIO $ newIORef mempty 
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
queueNull :: Queue sm so a -> Event Bool
queueNull q =
  Event $ \p ->
  do n <- readIORef (queueCountRef q)
     return (n == 0)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged :: Queue sm so a -> Signal Bool
queueNullChanged q =
  mapSignalM (const $ queueNull q) (queueNullChanged_ q)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged_ :: Queue sm so a -> Signal ()
queueNullChanged_ = queueCountChanged_

-- | Return the current queue size.
--
-- See also 'queueCountStats', 'queueCountChanged' and 'queueCountChanged_'.
queueCount :: Queue sm so a -> Event Int
queueCount q =
  Event $ \p -> readIORef (queueCountRef q)

-- | Return the queue size statistics.
queueCountStats :: Queue sm so a -> Event (TimingStats Int)
queueCountStats q =
  Event $ \p -> readIORef (queueCountStatsRef q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged :: Queue sm so a -> Signal Int
queueCountChanged q =
  mapSignalM (const $ queueCount q) (queueCountChanged_ q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged_ :: Queue sm so a -> Signal ()
queueCountChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the total number of input items that were stored.
--
-- See also 'enqueueStoreCountChanged' and 'enqueueStoreCountChanged_'.
enqueueStoreCount :: Queue sm so a -> Event Int
enqueueStoreCount q =
  Event $ \p -> readIORef (enqueueStoreCountRef q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged :: Queue sm so a -> Signal Int
enqueueStoreCountChanged q =
  mapSignalM (const $ enqueueStoreCount q) (enqueueStoreCountChanged_ q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged_ :: Queue sm so a -> Signal ()
enqueueStoreCountChanged_ q =
  mapSignal (const ()) (enqueueStored q)
      
-- | Return the total number of requests for dequeueing the items,
-- not taking into account the failed attempts to dequeue immediately
-- without suspension.
--
-- See also 'dequeueCountChanged' and 'dequeueCountChanged_'.
dequeueCount :: Queue sm so a -> Event Int
dequeueCount q =
  Event $ \p -> readIORef (dequeueCountRef q)
      
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged :: Queue sm so a -> Signal Int
dequeueCountChanged q =
  mapSignalM (const $ dequeueCount q) (dequeueCountChanged_ q)
  
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged_ :: Queue sm so a -> Signal ()
dequeueCountChanged_ q =
  mapSignal (const ()) (dequeueRequested q)
      
-- | Return the total number of output items that were actually dequeued.
--
-- See also 'dequeueExtractCountChanged' and 'dequeueExtractCountChanged_'.
dequeueExtractCount :: Queue sm so a -> Event Int
dequeueExtractCount q =
  Event $ \p -> readIORef (dequeueExtractCountRef q)
      
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged :: Queue sm so a -> Signal Int
dequeueExtractCountChanged q =
  mapSignalM (const $ dequeueExtractCount q) (dequeueExtractCountChanged_ q)
  
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged_ :: Queue sm so a -> Signal ()
dequeueExtractCountChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the rate of the items that were stored: how many items
-- per time.
enqueueStoreRate :: Queue sm so a -> Event Double
enqueueStoreRate q =
  Event $ \p ->
  do x <- readIORef (enqueueStoreCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the requests for dequeueing the items: how many requests
-- per time. It does not include the failed attempts to dequeue immediately
-- without suspension.
dequeueRate :: Queue sm so a -> Event Double
dequeueRate q =
  Event $ \p ->
  do x <- readIORef (dequeueCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the output items that were dequeued: how many items
-- per time.
dequeueExtractRate :: Queue sm so a -> Event Double
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
queueWaitTime :: Queue sm so a -> Event (SamplingStats Double)
queueWaitTime q =
  Event $ \p -> readIORef (queueWaitTimeRef q)
      
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged :: Queue sm so a -> Signal (SamplingStats Double)
queueWaitTimeChanged q =
  mapSignalM (const $ queueWaitTime q) (queueWaitTimeChanged_ q)
  
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged_ :: Queue sm so a -> Signal ()
queueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the dequeue wait time from the time at which the item was requested
-- for dequeueing to the time at which it was actually dequeued.
--
-- See also 'dequeueWaitTimeChanged' and 'dequeueWaitTimeChanged_'.
dequeueWaitTime :: Queue sm so a -> Event (SamplingStats Double)
dequeueWaitTime q =
  Event $ \p -> readIORef (dequeueWaitTimeRef q)
      
-- | Signal when the 'dequeueWaitTime' property value has changed.
dequeueWaitTimeChanged :: Queue sm so a -> Signal (SamplingStats Double)
dequeueWaitTimeChanged q =
  mapSignalM (const $ dequeueWaitTime q) (dequeueWaitTimeChanged_ q)
  
-- | Signal when the 'dequeueWaitTime' property value has changed.
dequeueWaitTimeChanged_ :: Queue sm so a -> Signal ()
dequeueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)

-- | Return a long-term average queue rate calculated as
-- the average queue size divided by the average wait time.
--
-- See also 'queueRateChanged' and 'queueRateChanged_'.
queueRate :: Queue sm so a -> Event Double
queueRate q =
  Event $ \p ->
  do x <- readIORef (queueCountStatsRef q)
     y <- readIORef (queueWaitTimeRef q)
     return (timingStatsMean x / samplingStatsMean y) 

-- | Signal when the 'queueRate' property value has changed.
queueRateChanged :: Queue sm so a -> Signal Double
queueRateChanged q =
  mapSignalM (const $ queueRate q) (queueRateChanged_ q)

-- | Signal when the 'queueRate' property value has changed.
queueRateChanged_ :: Queue sm so a -> Signal ()
queueRateChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)
  
-- | Dequeue suspending the process if the queue is empty.
dequeue :: (DequeueStrategy sm,
            EnqueueStrategy so)
           => Queue sm so a
           -- ^ the queue
           -> Process a
           -- ^ the dequeued value
dequeue q =
  do t <- liftEvent $ dequeueRequest q
     requestResource (dequeueRes q)
     liftEvent $ dequeueExtract q t
  
-- | Dequeue with the output priority suspending the process if the queue is empty.
dequeueWithOutputPriority :: (DequeueStrategy sm,
                              PriorityQueueStrategy so po)
                             => Queue sm so a
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
tryDequeue :: DequeueStrategy sm
              => Queue sm so a
              -- ^ the queue
              -> Event (Maybe a)
              -- ^ the dequeued value of 'Nothing'
tryDequeue q =
  do x <- tryRequestResourceWithinEvent (dequeueRes q)
     if x 
       then do t <- dequeueRequest q
               fmap Just $ dequeueExtract q t
       else return Nothing

-- | Remove the item from the queue and return a flag indicating
-- whether the item was found and actually removed.
queueDelete :: (Eq a,
                DeletingQueueStrategy sm,
                DequeueStrategy so)
               => Queue sm so a
               -- ^ the queue
               -> a
               -- ^ the item to remove from the queue
               -> Event Bool
               -- ^ whether the item was found and removed
queueDelete q a = fmap isJust $ queueDeleteBy q (== a)

-- | Remove the specified item from the queue.
queueDelete_ :: (Eq a,
                 DeletingQueueStrategy sm,
                 DequeueStrategy so)
                => Queue sm so a
                -- ^ the queue
                -> a
                -- ^ the item to remove from the queue
                -> Event ()
queueDelete_ q a = fmap (const ()) $ queueDeleteBy q (== a)

-- | Remove an item satisfying the specified predicate and return the item if found.
queueDeleteBy :: (DeletingQueueStrategy sm,
                  DequeueStrategy so)
                 => Queue sm so a
                 -- ^ the queue
                 -> (a -> Bool)
                 -- ^ the predicate
                 -> Event (Maybe a)
queueDeleteBy q pred =
  do x <- tryRequestResourceWithinEvent (dequeueRes q)
     if x
       then do i <- strategyQueueDeleteBy (queueStore q) (pred . itemValue)
               case i of
                 Nothing ->
                   do releaseResourceWithinEvent (dequeueRes q)
                      return Nothing
                 Just i ->
                   do t <- dequeueRequest q
                      fmap Just $ dequeuePostExtract q t i
       else return Nothing
               
-- | Remove an item satisfying the specified predicate.
queueDeleteBy_ :: (DeletingQueueStrategy sm,
                   DequeueStrategy so)
                  => Queue sm so a
                  -- ^ the queue
                  -> (a -> Bool)
                  -- ^ the predicate
                  -> Event ()
queueDeleteBy_ q pred = fmap (const ()) $ queueDeleteBy q pred

-- | Detect whether the item is contained in the queue.
queueContains :: (Eq a,
                  DeletingQueueStrategy sm)
                 => Queue sm so a
                 -- ^ the queue
                 -> a
                 -- ^ the item to search the queue for
                 -> Event Bool
                 -- ^ whether the item was found
queueContains q a = fmap isJust $ queueContainsBy q (== a)

-- | Detect whether an item satisfying the specified predicate is contained in the queue.
queueContainsBy :: DeletingQueueStrategy sm
                   => Queue sm so a
                   -- ^ the queue
                   -> (a -> Bool)
                   -- ^ the predicate
                   -> Event (Maybe a)
                   -- ^ the item if it was found
queueContainsBy q pred =
  do x <- strategyQueueContainsBy (queueStore q) (pred . itemValue)
     case x of
       Nothing -> return Nothing
       Just i  -> return $ Just (itemValue i)

-- | Clear the queue immediately.
clearQueue :: DequeueStrategy sm
              => Queue sm so a
              -- ^ the queue
              -> Event ()
clearQueue q =
  do x <- tryDequeue q
     case x of
       Nothing -> return ()
       Just a  -> clearQueue q

-- | Enqueue the item.  
enqueue :: (EnqueueStrategy sm,
            DequeueStrategy so)
           => Queue sm so a
           -- ^ the queue
           -> a
           -- ^ the item to enqueue
           -> Event ()
enqueue = enqueueStore
     
-- | Enqueue with the storing priority the item.  
enqueueWithStoringPriority :: (PriorityQueueStrategy sm pm,
                               DequeueStrategy so)
                              => Queue sm so a
                              -- ^ the queue
                              -> pm
                              -- ^ the priority for storing
                              -> a
                              -- ^ the item to enqueue
                              -> Event ()
enqueueWithStoringPriority = enqueueStoreWithPriority

-- | Return a signal that notifies when the enqueued item
-- is stored in the internal memory of the queue.
enqueueStored :: Queue sm so a -> Signal a
enqueueStored q = publishSignal (enqueueStoredSource q)

-- | Return a signal that notifies when the dequeuing operation was requested.
dequeueRequested :: Queue sm so a -> Signal ()
dequeueRequested q = publishSignal (dequeueRequestedSource q)

-- | Return a signal that notifies when the item was extracted from the internal
-- storage of the queue and prepared for immediate receiving by the dequeuing process.
dequeueExtracted :: Queue sm so a -> Signal a
dequeueExtracted q = publishSignal (dequeueExtractedSource q)

-- | Store the item.
enqueueStore :: (EnqueueStrategy sm,
                 DequeueStrategy so)
                => Queue sm so a
                -- ^ the queue
                -> a
                -- ^ the item to be stored
                -> Event ()
enqueueStore q a =
  Event $ \p ->
  do let i = QueueItem { itemValue = a,
                         itemStoringTime = pointTime p }
     invokeEvent p $
       strategyEnqueue (queueStore q) i
     c <- readIORef (queueCountRef q)
     let c' = c + 1
         t  = pointTime p
     c' `seq` writeIORef (queueCountRef q) c'
     modifyIORef' (queueCountStatsRef q) (addTimingStats t c')
     modifyIORef' (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i)

-- | Store with the priority the item.
enqueueStoreWithPriority :: (PriorityQueueStrategy sm pm,
                             DequeueStrategy so)
                            => Queue sm so a
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
       strategyEnqueueWithPriority (queueStore q) pm i
     c <- readIORef (queueCountRef q)
     let c' = c + 1
         t  = pointTime p
     c' `seq` writeIORef (queueCountRef q) c'
     modifyIORef' (queueCountStatsRef q) (addTimingStats t c')
     modifyIORef' (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i)

-- | Accept the dequeuing request and return the current simulation time.
dequeueRequest :: Queue sm so a
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
dequeueExtract :: DequeueStrategy sm
                  => Queue sm so a
                  -- ^ the queue
                  -> Double
                  -- ^ the time of the dequeuing request
                  -> Event a
                  -- ^ the dequeued value
dequeueExtract q t' =
  Event $ \p ->
  do i <- invokeEvent p $
          strategyDequeue (queueStore q)
     invokeEvent p $
       dequeuePostExtract q t' i

-- | A post action after extracting the item by the dequeuing request.  
dequeuePostExtract :: DequeueStrategy sm
                      => Queue sm so a
                      -- ^ the queue
                      -> Double
                      -- ^ the time of the dequeuing request
                      -> QueueItem a
                      -- ^ the item to dequeue
                      -> Event a
                      -- ^ the dequeued value
dequeuePostExtract q t' i =
  Event $ \p ->
  do c <- readIORef (queueCountRef q)
     let c' = c - 1
         t  = pointTime p
     c' `seq` writeIORef (queueCountRef q) c'
     modifyIORef' (queueCountStatsRef q) (addTimingStats t c')
     modifyIORef' (dequeueExtractCountRef q) (+ 1)
     invokeEvent p $
       dequeueStat q t' i
     invokeEvent p $
       triggerSignal (dequeueExtractedSource q) (itemValue i)
     return $ itemValue i

-- | Update the statistics for the output wait time of the dequeuing operation
-- and the wait time of storing in the queue.
dequeueStat :: Queue sm so a
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
     modifyIORef' (dequeueWaitTimeRef q) $
       addSamplingStats (t - t')
     modifyIORef' (queueWaitTimeRef q) $
       addSamplingStats (t - t1)

-- | Signal whenever any property of the queue changes.
--
-- The property must have the corresponded signal. There are also characteristics
-- similar to the properties but that have no signals. As a rule, such characteristics
-- already depend on the simulation time and therefore they may change at any
-- time point.
queueChanged_ :: Queue sm so a -> Signal ()
queueChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  dequeueRequested q <>
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the summary for the queue with desciption of its
-- properties and activities using the specified indent.
queueSummary :: (Show sm, Show so) => Queue sm so a -> Int -> Event ShowS
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

-- | Reset the statistics.
resetQueue :: Queue sm so a -> Event ()
resetQueue q =
  Event $ \p ->
  do let t = pointTime p
     queueCount <- readIORef (queueCountRef q)
     writeIORef (queueCountStatsRef q) $
       returnTimingStats t queueCount
     writeIORef (enqueueStoreCountRef q) 0
     writeIORef (dequeueCountRef q) 0
     writeIORef (dequeueExtractCountRef q) 0
     writeIORef (queueWaitTimeRef q) mempty
     writeIORef (dequeueWaitTimeRef q) mempty
