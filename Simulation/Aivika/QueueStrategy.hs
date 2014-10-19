
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}

-- |
-- Module     : Simulation.Aivika.QueueStrategy
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
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
class QueueStrategy s where

  data StrategyQueue s :: * -> *

  -- | Create a new queue by the specified strategy.
  newStrategyQueue :: s
                      -- ^ the strategy
                      -> Simulation (StrategyQueue s i)
                      -- ^ a new queue

  -- | Test whether the queue is empty.
  strategyQueueNull :: StrategyQueue s i
                       -- ^ the queue
                       -> Event Bool
                       -- ^ the result of the test

-- | Defines a strategy with support of the dequeuing operation.
class QueueStrategy s => DequeueStrategy s where

  -- | Dequeue the front element and return it.
  strategyDequeue :: StrategyQueue s i
                     -- ^ the queue
                     -> Event i
                     -- ^ the dequeued element

-- | It defines a strategy when we can enqueue a single element.
class DequeueStrategy s => EnqueueStrategy s where

  -- | Enqueue an element.
  strategyEnqueue :: StrategyQueue s i
                     -- ^ the queue
                     -> i
                     -- ^ the element to be enqueued
                     -> Event ()
                     -- ^ the action of enqueuing

-- | It defines a strategy when we can enqueue an element with the specified priority.
class DequeueStrategy s => PriorityQueueStrategy s p | s -> p where

  -- | Enqueue an element with the specified priority.
  strategyEnqueueWithPriority :: StrategyQueue s i
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

instance QueueStrategy FCFS where

  newtype StrategyQueue FCFS i = FCFSQueue (DoubleLinkedList i)

  newStrategyQueue s = fmap FCFSQueue $ liftIO newList

  strategyQueueNull (FCFSQueue q) = liftIO $ listNull q

instance DequeueStrategy FCFS where

  strategyDequeue (FCFSQueue q) =
    liftIO $
    do i <- listFirst q
       listRemoveFirst q
       return i

instance EnqueueStrategy FCFS where

  strategyEnqueue (FCFSQueue q) i = liftIO $ listAddLast q i

instance QueueStrategy LCFS where

  newtype StrategyQueue LCFS i = LCFSQueue (DoubleLinkedList i)
  
  newStrategyQueue s = fmap LCFSQueue $ liftIO newList
       
  strategyQueueNull (LCFSQueue q) = liftIO $ listNull q

instance DequeueStrategy LCFS where

  strategyDequeue (LCFSQueue q) =
    liftIO $
    do i <- listFirst q
       listRemoveFirst q
       return i

instance EnqueueStrategy LCFS where

  strategyEnqueue (LCFSQueue q) i = liftIO $ listInsertFirst q i

instance QueueStrategy StaticPriorities where

  newtype StrategyQueue StaticPriorities i = StaticPriorityQueue (PQ.PriorityQueue i)
  
  newStrategyQueue s = fmap StaticPriorityQueue $ liftIO PQ.newQueue

  strategyQueueNull (StaticPriorityQueue q) = liftIO $ PQ.queueNull q

instance DequeueStrategy StaticPriorities where

  strategyDequeue (StaticPriorityQueue q) =
    liftIO $
    do (_, i) <- PQ.queueFront q
       PQ.dequeue q
       return i

instance PriorityQueueStrategy StaticPriorities Double where

  strategyEnqueueWithPriority (StaticPriorityQueue q) p i = liftIO $ PQ.enqueue q p i

instance QueueStrategy SIRO where

  newtype StrategyQueue SIRO i = SIROQueue (V.Vector i)

  newStrategyQueue s = fmap SIROQueue $ liftIO V.newVector

  strategyQueueNull (SIROQueue q) =
    liftIO $
    do n <- V.vectorCount q
       return (n == 0)

instance DequeueStrategy SIRO where

  strategyDequeue (SIROQueue q) =
    liftIO $
    do n <- V.vectorCount q
       i <- getStdRandom (randomR (0, n - 1))
       x <- V.readVector q i
       V.vectorDeleteAt q i
       return x

instance EnqueueStrategy SIRO where

  strategyEnqueue (SIROQueue q) i = liftIO $ V.appendVector q i
