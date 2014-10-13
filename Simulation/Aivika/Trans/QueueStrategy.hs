
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}

-- |
-- Module     : Simulation.Aivika.Trans.QueueStrategy
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the queue strategies.
--
module Simulation.Aivika.Trans.QueueStrategy
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

import Control.Monad.Trans

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Parameter.Random
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.EventQueue

import qualified Simulation.Aivika.DoubleLinkedList as LL
import qualified Simulation.Aivika.PriorityQueue as PQ
import qualified Simulation.Aivika.Vector as V

-- | Defines the basic queue strategy.
class QueueStrategy m s where

  -- | The strategy queue.
  data StrategyQueue m s :: * -> *

  -- | Create a new queue by the specified strategy.
  newStrategyQueue :: s
                      -- ^ the strategy
                      -> Simulation m (StrategyQueue m s a)
                      -- ^ a new queue

  -- | Test whether the queue is empty.
  strategyQueueNull :: StrategyQueue m s a
                       -- ^ the queue
                       -> Event m Bool
                       -- ^ the result of the test

-- | Defines a strategy with support of the dequeuing operation.
class QueueStrategy m s => DequeueStrategy m s where

  -- | Dequeue the front element and return it.
  strategyDequeue :: StrategyQueue m s a
                     -- ^ the queue
                     -> Event m a
                     -- ^ the dequeued element

-- | It defines a strategy when we can enqueue a single element.
class DequeueStrategy m s => EnqueueStrategy m s where

  -- | Enqueue an element.
  strategyEnqueue :: StrategyQueue m s a
                     -- ^ the queue
                     -> a
                     -- ^ the element to be enqueued
                     -> Event m ()
                     -- ^ the action of enqueuing

-- | It defines a strategy when we can enqueue an element with the specified priority.
class DequeueStrategy m s => PriorityQueueStrategy m s p | s -> p where

  -- | Enqueue an element with the specified priority.
  strategyEnqueueWithPriority :: StrategyQueue m s a
                                 -- ^ the queue
                                 -> p
                                 -- ^ the priority
                                 -> a
                                 -- ^ the element to be enqueued
                                 -> Event m ()
                                 -- ^ the action of enqueuing

-- | Strategy: First Come - First Served (FCFS).
data FCFS = FCFS deriving (Eq, Ord, Show)

-- | Strategy: Last Come - First Served (LCFS)
data LCFS = LCFS deriving (Eq, Ord, Show)

-- | Strategy: Service in Random Order (SIRO).
data SIRO = SIRO deriving (Eq, Ord, Show)

-- | Strategy: Static Priorities. It uses the priority queue.
data StaticPriorities = StaticPriorities deriving (Eq, Ord, Show)

instance QueueStrategy IO FCFS where

  newtype StrategyQueue IO FCFS a = FCFSQueue (LL.DoubleLinkedList a)

  {-# SPECIALISE INLINE newStrategyQueue :: FCFS -> Simulation IO (StrategyQueue IO FCFS a) #-}
  newStrategyQueue s = fmap FCFSQueue $ liftIO LL.newList

  {-# SPECIALISE INLINE strategyQueueNull :: StrategyQueue IO FCFS a -> Event IO Bool #-}
  strategyQueueNull (FCFSQueue q) = liftIO $ LL.listNull q

instance DequeueStrategy IO FCFS where

  {-# SPECIALISE INLINE strategyDequeue :: StrategyQueue IO FCFS a -> Event IO a #-}
  strategyDequeue (FCFSQueue q) =
    liftIO $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

instance EnqueueStrategy IO FCFS where

  {-# SPECIALISE INLINE strategyEnqueue :: StrategyQueue IO FCFS a -> a -> Event IO () #-}
  strategyEnqueue (FCFSQueue q) i = liftIO $ LL.listAddLast q i

instance QueueStrategy IO LCFS where

  newtype StrategyQueue IO LCFS a = LCFSQueue (LL.DoubleLinkedList a)

  {-# SPECIALISE INLINE newStrategyQueue :: LCFS -> Simulation IO (StrategyQueue IO LCFS a) #-}
  newStrategyQueue s = fmap LCFSQueue $ liftIO LL.newList
       
  {-# SPECIALISE INLINE strategyQueueNull :: StrategyQueue IO LCFS a -> Event IO Bool #-}
  strategyQueueNull (LCFSQueue q) = liftIO $ LL.listNull q

instance DequeueStrategy IO LCFS where

  {-# SPECIALISE INLINE strategyDequeue :: StrategyQueue IO LCFS a -> Event IO a #-}
  strategyDequeue (LCFSQueue q) =
    liftIO $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

instance EnqueueStrategy IO LCFS where

  {-# SPECIALISE INLINE strategyEnqueue :: StrategyQueue IO LCFS a -> a -> Event IO () #-}
  strategyEnqueue (LCFSQueue q) i = liftIO $ LL.listInsertFirst q i

instance QueueStrategy IO StaticPriorities where

  newtype StrategyQueue IO StaticPriorities a = StaticPriorityQueue (PQ.PriorityQueue a)

  {-# SPECIALISE INLINE newStrategyQueue :: StaticPriorities -> Simulation IO (StrategyQueue IO StaticPriorities a) #-}
  newStrategyQueue s = fmap StaticPriorityQueue $ liftIO PQ.newQueue

  {-# SPECIALISE INLINE strategyQueueNull :: StrategyQueue IO StaticPriorities a -> Event IO Bool #-}
  strategyQueueNull (StaticPriorityQueue q) = liftIO $ PQ.queueNull q

instance DequeueStrategy IO StaticPriorities where

  {-# SPECIALISE INLINE strategyDequeue :: StrategyQueue IO StaticPriorities a -> Event IO a #-}
  strategyDequeue (StaticPriorityQueue q) =
    liftIO $
    do (_, i) <- PQ.queueFront q
       PQ.dequeue q
       return i

instance PriorityQueueStrategy IO StaticPriorities Double where

  {-# SPECIALISE INLINE strategyEnqueueWithPriority :: StrategyQueue IO StaticPriorities a -> Double -> a -> Event IO () #-}
  strategyEnqueueWithPriority (StaticPriorityQueue q) p i = liftIO $ PQ.enqueue q p i

instance QueueStrategy IO SIRO where

  newtype StrategyQueue IO SIRO a = SIROQueue (V.Vector a)
  
  {-# SPECIALISE INLINE newStrategyQueue :: SIRO -> Simulation IO (StrategyQueue IO SIRO a) #-}
  newStrategyQueue s = fmap SIROQueue $ liftIO V.newVector

  {-# SPECIALISE INLINE strategyQueueNull :: StrategyQueue IO SIRO a -> Event IO Bool #-}
  strategyQueueNull (SIROQueue q) =
    liftIO $
    do n <- V.vectorCount q
       return (n == 0)

instance DequeueStrategy IO SIRO where

  {-# SPECIALISE INLINE strategyDequeue :: StrategyQueue IO SIRO a -> Event IO a #-}
  strategyDequeue (SIROQueue q) =
    do n <- liftIO $ V.vectorCount q
       i <- liftParameter $ randomUniformInt 0 (n - 1)
       x <- liftIO $ V.readVector q i
       liftIO $ V.vectorDeleteAt q i
       return x

instance EnqueueStrategy IO SIRO where

  {-# SPECIALISE INLINE strategyEnqueue :: StrategyQueue IO SIRO a -> a -> Event IO () #-}
  strategyEnqueue (SIROQueue q) i = liftIO $ V.appendVector q i
