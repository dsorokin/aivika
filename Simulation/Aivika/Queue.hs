
-- |
-- Module     : Simulation.Aivika.Queue
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
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
       (FCFSQueue,
        LCFSQueue,
        SIROQueue,
        PriorityQueue,
        Queue,
        newFCFSQueue,
        newLCFSQueue,
        newSIROQueue,
        newPriorityQueue,
        newQueue,
        queueNull,
        queueFull,
        queueInputStrategy,
        queueStoringStrategy,
        queueOutputStrategy,
        queueMaxCount,
        queueCount,
        queueLostCount,
        queueInputCount,
        queueStoreCount,
        queueOutputCount,
        queueLoadFactor,
        queueInputRate,
        queueStoreRate,
        queueOutputRate,
        queueWaitTime,
        queueTotalWaitTime,
        queueInputWaitTime,
        queueOutputWaitTime,
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
        enqueueInitiated,
        enqueueStored,
        enqueueLost,
        dequeueRequested,
        dequeueExtracted) where

import Data.IORef
import Data.Monoid

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.Internal.Signal
import Simulation.Aivika.Internal.Observable
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

-- | Represents the queue using the specified strategies for input @si@,
-- internal storing (in memory) @sm@ and output @so@, where @a@ denotes
-- the type of items stored in the queue. Types @qi@, @qm@ and @qo@ are
-- determined automatically and you should not care about them - they
-- are dependent types.
data Queue si qi sm qm so qo a =
  Queue { queueMaxCount :: Int,
          -- ^ The queue capacity.
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
          queueStoreCountRef :: IORef Int,
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
            -- ^ the strategy applied to the input (enqueuing) process
            -> sm
            -- ^ the strategy applied when storing items in the queue
            -> so
            -- ^ the strategy applied to the output (dequeuing) process
            -> Int
            -- ^ the queue capacity
            -> Simulation (Queue si qi sm qm so qo a)  
newQueue si sm so count =
  do i  <- liftIO $ newIORef 0
     l  <- liftIO $ newIORef 0
     ci <- liftIO $ newIORef 0
     cm <- liftIO $ newIORef 0
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
                    queueInputStrategy = si,
                    queueStoringStrategy = sm,
                    queueOutputStrategy = so,
                    queueInputRes = ri,
                    queueStore = qm,
                    queueOutputRes = ro,
                    queueCountRef = i,
                    queueLostCountRef = l,
                    queueInputCountRef = ci,
                    queueStoreCountRef = cm,
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
queueCount :: Queue si qi sm qm so qo a -> Observable Int
queueCount q =
  let read = Event $ \p -> readIORef (queueCountRef q)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (enqueueStored q) <>
                    mapSignal (const ()) (dequeueExtracted q) }
  
-- | Return the number of lost items.
queueLostCount :: Queue si qi sm qm so qo a -> Observable Int
queueLostCount q =
  let read = Event $ \p -> readIORef (queueLostCountRef q)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (enqueueLost q) }

-- | Return the total number of input items that were enqueued.
queueInputCount :: Queue si qi sm qm so qo a -> Observable Int
queueInputCount q =
  let read = Event $ \p -> readIORef (queueInputCountRef q)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (enqueueInitiated q) }
      
-- | Return the total number of input items that were stored.
queueStoreCount :: Queue si qi sm qm so qo a -> Observable Int
queueStoreCount q =
  let read = Event $ \p -> readIORef (queueStoreCountRef q)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (enqueueStored q) }
      
-- | Return the total number of output items that were dequeued.
queueOutputCount :: Queue si qi sm qm so qo a -> Observable Int
queueOutputCount q =
  let read = Event $ \p -> readIORef (queueOutputCountRef q)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (dequeueExtracted q) }

-- | Return the load factor: the queue size divided by its maximum size.
queueLoadFactor :: Queue si qi sm qm so qo a -> Observable Double
queueLoadFactor q =
  let read =
        Event $ \p ->
        do x <- readIORef (queueCountRef q)
           let y = queueMaxCount q
           return (fromIntegral x / fromIntegral y)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (enqueueStored q) <>
                    mapSignal (const ()) (dequeueExtracted q) }
      
-- | Return the rate of the input items that were enqueued: how many items
-- per time.
queueInputRate :: Queue si qi sm qm so qo a -> Event Double
queueInputRate q =
  Event $ \p ->
  do x <- readIORef (queueInputCountRef q)
     t <- invokeDynamics p $ time
     return (fromIntegral x / t)
      
-- | Return the rate of the items that were stored: how many items
-- per time.
queueStoreRate :: Queue si qi sm qm so qo a -> Event Double
queueStoreRate q =
  Event $ \p ->
  do x <- readIORef (queueStoreCountRef q)
     t <- invokeDynamics p $ time
     return (fromIntegral x / t)
      
-- | Return the rate of the output items that were dequeued: how many items
-- per time.
queueOutputRate :: Queue si qi sm qm so qo a -> Event Double
queueOutputRate q =
  Event $ \p ->
  do x <- readIORef (queueOutputCountRef q)
     t <- invokeDynamics p $ time
     return (fromIntegral x / t)
      
-- | Return the wait time from the time at which the item was stored in the queue to
-- the time at which it was dequeued.
queueWaitTime :: Queue si qi sm qm so qo a -> Observable (SamplingStats Double)
queueWaitTime q =
  let read = Event $ \p -> readIORef (queueWaitTimeRef q)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (dequeueExtracted q) }
      
-- | Return the total wait time from the time at which the item was enqueued to
-- the time at which it was dequeued.
--
-- In some sense, @queueTotalWaitTime == queueInputWaitTime + queueWaitTime@.
queueTotalWaitTime :: Queue si qi sm qm so qo a -> Observable (SamplingStats Double)
queueTotalWaitTime q =
  let read = Event $ \p -> readIORef (queueTotalWaitTimeRef q)
  in Observable { readObservable = read,
                  observableChanged_ = 
                    mapSignal (const ()) (dequeueExtracted q) }
      
-- | Return the input wait time from the time at which the item was enqueued
-- to the time at which it was stored in the queue.
queueInputWaitTime :: Queue si qi sm qm so qo a -> Observable (SamplingStats Double)
queueInputWaitTime q =
  let read = Event $ \p -> readIORef (queueInputWaitTimeRef q)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (enqueueStored q) }
      
-- | Return the output wait time from the time at which the item was requested
-- for dequeuing to the time at which it was actually dequeued.
queueOutputWaitTime :: Queue si qi sm qm so qo a -> Observable (SamplingStats Double)
queueOutputWaitTime q =
  let read = Event $ \p -> readIORef (queueOutputWaitTimeRef q)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (dequeueExtracted q) }
  
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
     requestResource (queueOutputRes q)
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
     requestResourceWithPriority (queueOutputRes q) po
     liftEvent $ dequeueExtract q t
  
-- | Try to dequeue immediately.
tryDequeue :: (DequeueStrategy si qi,
               DequeueStrategy sm qm)
              => Queue si qi sm qm so qo a
              -- ^ the queue
              -> Event (Maybe a)
              -- ^ the dequeued value of 'Nothing'
tryDequeue q =
  do x <- tryRequestResourceWithinEvent (queueOutputRes q)
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
     requestResource (queueInputRes q)
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
     requestResourceWithPriority (queueInputRes q) pi
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
     requestResource (queueInputRes q)
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
     requestResourceWithPriority (queueInputRes q) pi
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
  do x <- tryRequestResourceWithinEvent (queueInputRes q)
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
  do x <- tryRequestResourceWithinEvent (queueInputRes q)
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
  do x <- tryRequestResourceWithinEvent (queueInputRes q)
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
  do x <- tryRequestResourceWithinEvent (queueInputRes q)
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
  do t <- invokeDynamics p time
     modifyIORef (queueInputCountRef q) (+ 1)
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
  do t <- invokeDynamics p time
     let i' = i { itemStoringTime = t }  -- now we have the actual time of storing
     invokeEvent p $
       strategyEnqueue (queueStoringStrategy q) (queueStore q) i'
     modifyIORef (queueCountRef q) (+ 1)
     modifyIORef (queueStoreCountRef q) (+ 1)
     invokeEvent p $
       enqueueStat q i'
     invokeEvent p $
       releaseResourceWithinEvent (queueOutputRes q)
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
  do t <- invokeDynamics p time
     let i' = i { itemStoringTime = t }  -- now we have the actual time of storing
     invokeEvent p $
       strategyEnqueueWithPriority (queueStoringStrategy q) (queueStore q) pm i'
     modifyIORef (queueCountRef q) (+ 1)
     modifyIORef (queueStoreCountRef q) (+ 1)
     invokeEvent p $
       enqueueStat q i'
     invokeEvent p $
       releaseResourceWithinEvent (queueOutputRes q)
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
  do modifyIORef (queueLostCountRef q) $ (+) 1
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
     modifyIORef (queueInputWaitTimeRef q) $
       addSamplingStats (t1 - t0)

-- | Accept the dequeuing request and return the current simulation time.
dequeueRequest :: Queue si qi sm qm so qo a
                 -- ^ the queue
                 -> Event Double
                 -- ^ the current time
dequeueRequest q =
  Event $ \p ->
  do invokeEvent p $
       triggerSignal (dequeueRequestedSource q) ()
     invokeDynamics p time 

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
          strategyDequeue (queueStoringStrategy q) (queueStore q)
     modifyIORef (queueCountRef q) (+ (- 1))
     modifyIORef (queueOutputCountRef q) (+ 1)
     invokeEvent p $
       dequeueStat q t' i
     invokeEvent p $
       releaseResourceWithinEvent (queueInputRes q)
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
     t <- invokeDynamics p time
     modifyIORef (queueOutputWaitTimeRef q) $
       addSamplingStats (t - t')
     modifyIORef (queueTotalWaitTimeRef q) $
       addSamplingStats (t - t0)
     modifyIORef (queueWaitTimeRef q) $
       addSamplingStats (t - t1)

