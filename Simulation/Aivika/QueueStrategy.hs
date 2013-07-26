
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
       (QueueStrategy(..),
        DequeueStrategy(..),
        EnqueueStrategy(..),
        PriorityQueueStrategy(..)) where

import Simulation.Aivika.DoubleLinkedList
import qualified Simulation.Aivika.PriorityQueue as PQ

-- | Defines the basic queue strategy.
class QueueStrategy s q | s -> q where

  -- | Create a new queue by the specified strategy.
  newStrategyQueue :: s -> IO (q i)

  -- | Test whether the queue is empty.
  strategyQueueNull :: s -> q i -> IO Bool

-- | Defines a strategy with support of the dequeuing operation.
class QueueStrategy s q => DequeueStrategy s q | s -> q where

  -- | Dequeue the front element and return it.
  strategyDequeue :: s -> q i -> IO i

-- | It defines a strategy when we can enqueue a single element.
class DequeueStrategy s q => EnqueueStrategy s q | s -> q where

  -- | Enqueue an element.
  strategyEnqueue :: s -> q i -> i -> IO ()

-- | It defines a strategy when we can enqueue an element with the specified priority.
class DequeueStrategy s q => PriorityQueueStrategy s q | s -> q where

  -- | Enqueue an element with the specified priority.
  strategyEnqueueWithPriority :: s -> q i -> Double -> i -> IO ()

-- | Strategy: First Come - First Served (FCFS).
data FCFS = FCFS

-- | Strategy: Last Come - First Served (LCFS)
data LCFS = LCFS

-- | Strategy: Static Priorities. It uses the priority queue.
data StaticPriorities = StaticPriorities

instance QueueStrategy FCFS DoubleLinkedList where

  newStrategyQueue = const newList

  strategyQueueNull = const listNull

instance DequeueStrategy FCFS DoubleLinkedList where

  strategyDequeue =
    const $ \q ->
    do i <- listFirst q
       listRemoveFirst q
       return i

instance EnqueueStrategy FCFS DoubleLinkedList where

  strategyEnqueue = const listAddLast

instance QueueStrategy LCFS DoubleLinkedList where

  newStrategyQueue = const newList
       
  strategyQueueNull = const listNull

instance DequeueStrategy LCFS DoubleLinkedList where

  strategyDequeue =
    const $ \q ->
    do i <- listFirst q
       listRemoveFirst q
       return i

instance EnqueueStrategy LCFS DoubleLinkedList where

  strategyEnqueue = const listInsertFirst

instance QueueStrategy StaticPriorities PQ.PriorityQueue where

  newStrategyQueue = const PQ.newQueue

  strategyQueueNull = const PQ.queueNull

instance DequeueStrategy StaticPriorities PQ.PriorityQueue where

  strategyDequeue =
    const $ \q ->
    do (_, i) <- PQ.queueFront q
       PQ.dequeue q
       return i

instance PriorityQueueStrategy StaticPriorities PQ.PriorityQueue where

  strategyEnqueueWithPriority = const PQ.enqueue
