
-- |
-- Module     : Simulation.Aivika.Queue.Infinite.Base
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines an infinite optimised queue, which has no counters nor signals.
--
module Simulation.Aivika.Queue.Infinite.Base
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
          queueStore :: StrategyQueue sm a,
          dequeueRes :: Resource so,
          queueCountRef :: IORef Int }
  
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
newQueue :: (QueueStrategy sm,
             QueueStrategy so) =>
            sm
            -- ^ the strategy applied when storing items in the queue
            -> so
            -- ^ the strategy applied to the dequeueing (output) processes when the queue is empty
            -> Simulation (Queue sm so a)  
newQueue sm so =
  do i  <- liftIO $ newIORef 0
     qm <- newStrategyQueue sm
     ro <- newResourceWithMaxCount so 0 Nothing
     return Queue { enqueueStoringStrategy = sm,
                    dequeueStrategy = so,
                    queueStore = qm,
                    dequeueRes = ro,
                    queueCountRef = i }

-- | Test whether the queue is empty.
--
-- See also 'queueNullChanged' and 'queueNullChanged_'.
queueNull :: Queue sm so a -> Event Bool
queueNull q =
  Event $ \p ->
  do n <- readIORef (queueCountRef q)
     return (n == 0)
  
-- | Return the current queue size.
--
-- See also 'queueCountStats', 'queueCountChanged' and 'queueCountChanged_'.
queueCount :: Queue sm so a -> Event Int
queueCount q =
  Event $ \p -> readIORef (queueCountRef q)

-- | Dequeue suspending the process if the queue is empty.
dequeue :: (DequeueStrategy sm,
            EnqueueStrategy so)
           => Queue sm so a
           -- ^ the queue
           -> Process a
           -- ^ the dequeued value
dequeue q =
  do requestResource (dequeueRes q)
     liftEvent $ dequeueExtract q
  
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
  do requestResourceWithPriority (dequeueRes q) po
     liftEvent $ dequeueExtract q
  
-- | Try to dequeue immediately.
tryDequeue :: DequeueStrategy sm
              => Queue sm so a
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
       then do i <- strategyQueueDeleteBy (queueStore q) pred
               case i of
                 Nothing ->
                   do releaseResourceWithinEvent (dequeueRes q)
                      return Nothing
                 Just i ->
                   fmap Just $ dequeuePostExtract q i
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
                            => Queue sm so a
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
dequeueExtract :: DequeueStrategy sm
                  => Queue sm so a
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
dequeuePostExtract :: DequeueStrategy sm
                      => Queue sm so a
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
     return a
