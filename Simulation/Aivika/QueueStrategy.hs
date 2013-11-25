
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- |
-- Module     : Simulation.Aivika.QueueStrategy
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines the queue strategies.
--
module Simulation.Aivika.QueueStrategy
       (-- * Strategy Classes
        QueueStrategy(..),
        DequeueStrategy(..),
        EnqueueStrategy(..),
        PriorityQueueStrategy(..),
        -- * Strategy Instances
        FCFS(..),
        LCFS(..),
        SIRO(..),
        StaticPriorities(..)) where

import System.Random
import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Event
import Simulation.Aivika.DoubleLinkedList
import qualified Simulation.Aivika.PriorityQueue as PQ
import qualified Simulation.Aivika.Vector as V

-- | Defines the basic queue strategy.
class QueueStrategy s q | s -> q where

  -- | Create a new queue by the specified strategy.
  newStrategyQueue :: s
                      -- ^ the strategy
                      -> Simulation (q i)
                      -- ^ a new queue

  -- | Test whether the queue is empty.
  strategyQueueNull :: s
                       -- ^ the strategy
                       -> q i
                       -- ^ the queue
                       -> Event Bool
                       -- ^ the result of the test

-- | Defines a strategy with support of the dequeuing operation.
class QueueStrategy s q => DequeueStrategy s q | s -> q where

  -- | Dequeue the front element and return it.
  strategyDequeue :: s
                     -- ^ the strategy
                     -> q i
                     -- ^ the queue
                     -> Event i
                     -- ^ the dequeued element

-- | It defines a strategy when we can enqueue a single element.
class DequeueStrategy s q => EnqueueStrategy s q | s -> q where

  -- | Enqueue an element.
  strategyEnqueue :: s
                     -- ^ the strategy
                     -> q i
                     -- ^ the queue
                     -> i
                     -- ^ the element to be enqueued
                     -> Event ()
                     -- ^ the action of enqueuing

-- | It defines a strategy when we can enqueue an element with the specified priority.
class DequeueStrategy s q => PriorityQueueStrategy s q p | s -> q, s -> p where

  -- | Enqueue an element with the specified priority.
  strategyEnqueueWithPriority :: s
                                 -- ^ the strategy
                                 -> q i
                                 -- ^ the queue
                                 -> p
                                 -- ^ the priority
                                 -> i
                                 -- ^ the element to be enqueued
                                 -> Event ()
                                 -- ^ the action of enqueuing

-- | Strategy: First Come - First Served (FCFS).
data FCFS = FCFS deriving (Eq, Ord, Show)

-- | Strategy: Last Come - First Served (LCFS)
data LCFS = LCFS deriving (Eq, Ord, Show)

-- | Strategy: Service in Random Order (SIRO).
data SIRO = SIRO deriving (Eq, Ord, Show)

-- | Strategy: Static Priorities. It uses the priority queue.
data StaticPriorities = StaticPriorities deriving (Eq, Ord, Show)

instance QueueStrategy FCFS DoubleLinkedList where

  newStrategyQueue s = liftIO newList

  strategyQueueNull s q = liftIO $ listNull q

instance DequeueStrategy FCFS DoubleLinkedList where

  strategyDequeue s q =
    liftIO $
    do i <- listFirst q
       listRemoveFirst q
       return i

instance EnqueueStrategy FCFS DoubleLinkedList where

  strategyEnqueue s q i = liftIO $ listAddLast q i

instance QueueStrategy LCFS DoubleLinkedList where

  newStrategyQueue s = liftIO newList
       
  strategyQueueNull s q = liftIO $ listNull q

instance DequeueStrategy LCFS DoubleLinkedList where

  strategyDequeue s q =
    liftIO $
    do i <- listFirst q
       listRemoveFirst q
       return i

instance EnqueueStrategy LCFS DoubleLinkedList where

  strategyEnqueue s q i = liftIO $ listInsertFirst q i

instance QueueStrategy StaticPriorities PQ.PriorityQueue where

  newStrategyQueue s = liftIO PQ.newQueue

  strategyQueueNull s q = liftIO $ PQ.queueNull q

instance DequeueStrategy StaticPriorities PQ.PriorityQueue where

  strategyDequeue s q =
    liftIO $
    do (_, i) <- PQ.queueFront q
       PQ.dequeue q
       return i

instance PriorityQueueStrategy StaticPriorities PQ.PriorityQueue Double where

  strategyEnqueueWithPriority s q p i = liftIO $ PQ.enqueue q p i

instance QueueStrategy SIRO V.Vector where

  newStrategyQueue s = liftIO V.newVector

  strategyQueueNull s q =
    liftIO $
    do n <- V.vectorCount q
       return (n == 0)

instance DequeueStrategy SIRO V.Vector where

  strategyDequeue s q =
    liftIO $
    do n <- V.vectorCount q
       i <- getStdRandom (randomR (0, n - 1))
       x <- V.readVector q i
       V.vectorDeleteAt q i
       return x

instance EnqueueStrategy SIRO V.Vector where

  strategyEnqueue s q i = liftIO $ V.appendVector q i
