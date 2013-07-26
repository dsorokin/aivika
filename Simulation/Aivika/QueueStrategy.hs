
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
        UnitQueueStrategy(..),
        PriorityQueueStrategy(..)) where

import Simulation.Aivika.DoubleLinkedList
import qualified Simulation.Aivika.PriorityQueue as PQ

-- | Defines the basic queue strategy.
class QueueStrategy s q | s -> q where

  -- | Create a new queue by the specified strategy.
  newStrategyQueue :: s -> IO (q i)

  -- | Test whether the queue is empty.
  strategyQueueNull :: s -> q i -> IO Bool

  -- | Get the front element from the queue.
  strategyQueueFront :: s -> q i -> IO i

  -- | Dequeue the front element.
  strategyDequeue :: s -> q i -> IO ()

-- | It defines a strategy when we enqueue a single element.
class QueueStrategy s q => UnitQueueStrategy s q | s -> q where

  -- | Enqueue an element.
  strategyEnqueue :: s -> q i -> i -> IO ()

-- | It defines a strategy when we enqueue an element with the specified priority.
class QueueStrategy s q => PriorityQueueStrategy s q | s -> q where

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

  strategyDequeue = const listRemoveFirst

  strategyQueueNull = const listNull

  strategyQueueFront = const listFirst

instance UnitQueueStrategy FCFS DoubleLinkedList where

  strategyEnqueue = const listAddLast

instance QueueStrategy LCFS DoubleLinkedList where

  newStrategyQueue = const newList

  strategyDequeue = const listRemoveFirst

  strategyQueueNull = const listNull

  strategyQueueFront = const listFirst

instance UnitQueueStrategy LCFS DoubleLinkedList where

  strategyEnqueue = const listInsertFirst

instance QueueStrategy StaticPriorities PQ.PriorityQueue where

  newStrategyQueue = const PQ.newQueue

  strategyDequeue = const PQ.dequeue

  strategyQueueNull = const PQ.queueNull

  strategyQueueFront = const $ fmap snd . PQ.queueFront

instance PriorityQueueStrategy StaticPriorities PQ.PriorityQueue where

  strategyEnqueueWithPriority = const PQ.enqueue

