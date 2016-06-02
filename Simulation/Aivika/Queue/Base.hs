
-- |
-- Module     : Simulation.Aivika.Queue.Base
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines an optimised finite queue, which has no counters nor signals.
--
module Simulation.Aivika.Queue.Base
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
        queueDelete,
        queueDelete_,
        queueDeleteBy,
        queueDeleteBy_,
        queueContains,
        queueContainsBy,
        clearQueue) where

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
import Simulation.Aivika.Resource.Base
import Simulation.Aivika.QueueStrategy

import qualified Simulation.Aivika.DoubleLinkedList as DLL 
import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.PriorityQueue as PQ

-- | A type synonym for the ordinary FIFO queue also known as the FCFS
-- (First Come - First Serviced) queue.
type FCFSQueue a = Queue FCFS FCFS FCFS a

-- | A type synonym for the ordinary LIFO queue also known as the LCFS
-- (Last Come - First Serviced) queue.
type LCFSQueue a = Queue FCFS LCFS FCFS a

-- | A type synonym for the SIRO (Serviced in Random Order) queue.
type SIROQueue a = Queue FCFS SIRO FCFS a

-- | A type synonym for the queue with static priorities applied when
-- storing the elements in the queue.
type PriorityQueue a = Queue FCFS StaticPriorities FCFS a

-- | Represents a queue using the specified strategies for enqueueing (input), @si@,
-- internal storing (in memory), @sm@, and dequeueing (output), @so@, where @a@ denotes
-- the type of items stored in the queue.
data Queue si sm so a =
  Queue { queueMaxCount :: Int,
          -- ^ The queue capacity.
          enqueueStrategy :: si,
          -- ^ The strategy applied to the enqueueing (input) processes when the queue is full.
          enqueueStoringStrategy :: sm,
          -- ^ The strategy applied when storing (in memory) items in the queue.
          dequeueStrategy :: so,
          -- ^ The strategy applied to the dequeueing (output) processes when the queue is empty.
          enqueueRes :: Resource si,
          queueStore :: StrategyQueue sm a,
          dequeueRes :: Resource so,
          queueCountRef :: IORef Int
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
newQueue :: (QueueStrategy si,
             QueueStrategy sm,
             QueueStrategy so) =>
            si
            -- ^ the strategy applied to the enqueueing (input) processes when the queue is full
            -> sm
            -- ^ the strategy applied when storing items in the queue
            -> so
            -- ^ the strategy applied to the dequeueing (output) processes when the queue is empty
            -> Int
            -- ^ the queue capacity
            -> Simulation (Queue si sm so a)  
newQueue si sm so count =
  do i  <- liftIO $ newIORef 0
     ri <- newResourceWithMaxCount si count (Just count)
     qm <- newStrategyQueue sm
     ro <- newResourceWithMaxCount so 0 (Just count)
     return Queue { queueMaxCount = count,
                    enqueueStrategy = si,
                    enqueueStoringStrategy = sm,
                    dequeueStrategy = so,
                    enqueueRes = ri,
                    queueStore = qm,
                    dequeueRes = ro,
                    queueCountRef = i }
  
-- | Test whether the queue is empty.
--
-- See also 'queueNullChanged' and 'queueNullChanged_'.
queueNull :: Queue si sm so a -> Event Bool
queueNull q =
  Event $ \p ->
  do n <- readIORef (queueCountRef q)
     return (n == 0)
  
-- | Test whether the queue is full.
--
-- See also 'queueFullChanged' and 'queueFullChanged_'.
queueFull :: Queue si sm so a -> Event Bool
queueFull q =
  Event $ \p ->
  do n <- readIORef (queueCountRef q)
     return (n == queueMaxCount q)
  
-- | Return the current queue size.
--
-- See also 'queueCountStats', 'queueCountChanged' and 'queueCountChanged_'.
queueCount :: Queue si sm so a -> Event Int
queueCount q =
  Event $ \p -> readIORef (queueCountRef q)

-- | Dequeue suspending the process if the queue is empty.
dequeue :: (DequeueStrategy si,
            DequeueStrategy sm,
            EnqueueStrategy so)
           => Queue si sm so a
           -- ^ the queue
           -> Process a
           -- ^ the dequeued value
dequeue q =
  do requestResource (dequeueRes q)
     liftEvent $ dequeueExtract q
  
-- | Dequeue with the output priority suspending the process if the queue is empty.
dequeueWithOutputPriority :: (DequeueStrategy si,
                              DequeueStrategy sm,
                              PriorityQueueStrategy so po)
                             => Queue si sm so a
                             -- ^ the queue
                             -> po
                             -- ^ the priority for output
                             -> Process a
                             -- ^ the dequeued value
dequeueWithOutputPriority q po =
  do requestResourceWithPriority (dequeueRes q) po
     liftEvent $ dequeueExtract q
  
-- | Try to dequeue immediately.
tryDequeue :: (DequeueStrategy si,
               DequeueStrategy sm)
              => Queue si sm so a
              -- ^ the queue
              -> Event (Maybe a)
              -- ^ the dequeued value of 'Nothing'
tryDequeue q =
  do x <- tryRequestResourceWithinEvent (dequeueRes q)
     if x 
       then fmap Just $ dequeueExtract q
       else return Nothing

-- | Remove the item from the queue and return a flag indicating
-- whether the item was found and actually removed.
queueDelete :: (Eq a,
                DequeueStrategy si,
                DeletingQueueStrategy sm,
                DequeueStrategy so)
               => Queue si sm so a
               -- ^ the queue
               -> a
               -- ^ the item to remove from the queue
               -> Event Bool
               -- ^ whether the item was found and removed
queueDelete q a = fmap isJust $ queueDeleteBy q (== a)

-- | Remove the specified item from the queue.
queueDelete_ :: (Eq a,
                 DequeueStrategy si,
                 DeletingQueueStrategy sm,
                 DequeueStrategy so)
                => Queue si sm so a
                -- ^ the queue
                -> a
                -- ^ the item to remove from the queue
                -> Event ()
queueDelete_ q a = fmap (const ()) $ queueDeleteBy q (== a)

-- | Remove an item satisfying the specified predicate and return the item if found.
queueDeleteBy :: (DequeueStrategy si,
                  DeletingQueueStrategy sm,
                  DequeueStrategy so)
                 => Queue si sm so a
                 -- ^ the queue
                 -> (a -> Bool)
                 -- ^ the predicate
                 -> Event (Maybe a)
queueDeleteBy q pred =
  do x <- tryRequestResourceWithinEvent (dequeueRes q)
     if x
       then do i <- strategyQueueDeleteBy (queueStore q) pred
               case i of
                 Nothing ->
                   do releaseResourceWithinEvent (dequeueRes q)
                      return Nothing
                 Just i ->
                   fmap Just $ dequeuePostExtract q i
       else return Nothing
               
-- | Remove an item satisfying the specified predicate.
queueDeleteBy_ :: (DequeueStrategy si,
                   DeletingQueueStrategy sm,
                   DequeueStrategy so)
                  => Queue si sm so a
                  -- ^ the queue
                  -> (a -> Bool)
                  -- ^ the predicate
                  -> Event ()
queueDeleteBy_ q pred = fmap (const ()) $ queueDeleteBy q pred

-- | Detect whether the item is contained in the queue.
queueContains :: (Eq a,
                  DeletingQueueStrategy sm)
                 => Queue si sm so a
                 -- ^ the queue
                 -> a
                 -- ^ the item to search the queue for
                 -> Event Bool
                 -- ^ whether the item was found
queueContains q a = fmap isJust $ queueContainsBy q (== a)

-- | Detect whether an item satisfying the specified predicate is contained in the queue.
queueContainsBy :: DeletingQueueStrategy sm
                   => Queue si sm so a
                   -- ^ the queue
                   -> (a -> Bool)
                   -- ^ the predicate
                   -> Event (Maybe a)
                   -- ^ the item if it was found
queueContainsBy q pred =
  strategyQueueContainsBy (queueStore q) pred

-- | Clear the queue immediately.
clearQueue :: (DequeueStrategy si,
               DequeueStrategy sm)
              => Queue si sm so a
              -- ^ the queue
              -> Event ()
clearQueue q =
  do x <- tryDequeue q
     case x of
       Nothing -> return ()
       Just a  -> clearQueue q
              
-- | Enqueue the item suspending the process if the queue is full.  
enqueue :: (EnqueueStrategy si,
            EnqueueStrategy sm,
            DequeueStrategy so)
           => Queue si sm so a
           -- ^ the queue
           -> a
           -- ^ the item to enqueue
           -> Process ()
enqueue q a =
  do requestResource (enqueueRes q)
     liftEvent $ enqueueStore q a
     
-- | Enqueue with the input priority the item suspending the process if the queue is full.  
enqueueWithInputPriority :: (PriorityQueueStrategy si pi,
                             EnqueueStrategy sm,
                             DequeueStrategy so)
                            => Queue si sm so a
                            -- ^ the queue
                            -> pi
                            -- ^ the priority for input
                            -> a
                            -- ^ the item to enqueue
                            -> Process ()
enqueueWithInputPriority q pi a =
  do requestResourceWithPriority (enqueueRes q) pi
     liftEvent $ enqueueStore q a
     
-- | Enqueue with the storing priority the item suspending the process if the queue is full.  
enqueueWithStoringPriority :: (EnqueueStrategy si,
                               PriorityQueueStrategy sm pm,
                               DequeueStrategy so)
                              => Queue si sm so a
                              -- ^ the queue
                              -> pm
                              -- ^ the priority for storing
                              -> a
                              -- ^ the item to enqueue
                              -> Process ()
enqueueWithStoringPriority q pm a =
  do requestResource (enqueueRes q)
     liftEvent $ enqueueStoreWithPriority q pm a
     
-- | Enqueue with the input and storing priorities the item suspending the process if the queue is full.  
enqueueWithInputStoringPriorities :: (PriorityQueueStrategy si pi,
                                      PriorityQueueStrategy sm pm,
                                      DequeueStrategy so)
                                     => Queue si sm so a
                                     -- ^ the queue
                                     -> pi
                                     -- ^ the priority for input
                                     -> pm
                                     -- ^ the priority for storing
                                     -> a
                                     -- ^ the item to enqueue
                                     -> Process ()
enqueueWithInputStoringPriorities q pi pm a =
  do requestResourceWithPriority (enqueueRes q) pi
     liftEvent $ enqueueStoreWithPriority q pm a
     
-- | Try to enqueue the item. Return 'False' in the monad if the queue is full.
tryEnqueue :: (EnqueueStrategy sm,
               DequeueStrategy so)
              => Queue si sm so a
              -- ^ the queue
              -> a
              -- ^ the item which we try to enqueue
              -> Event Bool
tryEnqueue q a =
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x 
       then do enqueueStore q a
               return True
       else return False

-- | Try to enqueue with the storing priority the item. Return 'False' in
-- the monad if the queue is full.
tryEnqueueWithStoringPriority :: (PriorityQueueStrategy sm pm,
                                  DequeueStrategy so)
                                 => Queue si sm so a
                                 -- ^ the queue
                                 -> pm
                                 -- ^ the priority for storing
                                 -> a
                                 -- ^ the item which we try to enqueue
                                 -> Event Bool
tryEnqueueWithStoringPriority q pm a =
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x 
       then do enqueueStoreWithPriority q pm a
               return True
       else return False

-- | Store the item.
enqueueStore :: (EnqueueStrategy sm,
                 DequeueStrategy so)
                => Queue si sm so a
                -- ^ the queue
                -> a
                -- ^ the item to be stored
                -> Event ()
enqueueStore q a =
  Event $ \p ->
  do invokeEvent p $
       strategyEnqueue (queueStore q) a
     c <- readIORef (queueCountRef q)
     let c' = c + 1
     c' `seq` writeIORef (queueCountRef q) c'
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)

-- | Store with the priority the item.
enqueueStoreWithPriority :: (PriorityQueueStrategy sm pm,
                             DequeueStrategy so)
                            => Queue si sm so a
                            -- ^ the queue
                            -> pm
                            -- ^ the priority for storing
                            -> a
                            -- ^ the item to be enqueued
                            -> Event ()
enqueueStoreWithPriority q pm a =
  Event $ \p ->
  do invokeEvent p $
       strategyEnqueueWithPriority (queueStore q) pm a
     c <- readIORef (queueCountRef q)
     let c' = c + 1
     c' `seq` writeIORef (queueCountRef q) c'
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)

-- | Extract an item for the dequeuing request.  
dequeueExtract :: (DequeueStrategy si,
                   DequeueStrategy sm)
                  => Queue si sm so a
                  -- ^ the queue
                  -> Event a
                  -- ^ the dequeued value
dequeueExtract q =
  Event $ \p ->
  do a <- invokeEvent p $
          strategyDequeue (queueStore q)
     invokeEvent p $
       dequeuePostExtract q a

-- | A post action after extracting the item by the dequeuing request.  
dequeuePostExtract :: (DequeueStrategy si,
                       DequeueStrategy sm)
                      => Queue si sm so a
                      -- ^ the queue
                      -> a
                      -- ^ the item to dequeue
                      -> Event a
                      -- ^ the dequeued value
dequeuePostExtract q a =
  Event $ \p ->
  do c <- readIORef (queueCountRef q)
     let c' = c - 1
     c' `seq` writeIORef (queueCountRef q) c'
     invokeEvent p $
       releaseResourceWithinEvent (enqueueRes q)
     return a
