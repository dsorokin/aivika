
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}

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
import Simulation.Aivika.Trans.Comp.Template
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Parameter.Random
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event

import qualified Simulation.Aivika.Trans.DoubleLinkedList as LL
import qualified Simulation.Aivika.Trans.PriorityQueue as PQ
import qualified Simulation.Aivika.Trans.Vector as V

-- | Defines the basic queue strategy.
class Comp m => QueueStrategy m s where

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

instance Comp m => QueueStrategy m FCFS where

  newtype StrategyQueue m FCFS a = FCFSQueue (LL.DoubleLinkedList m a)

  {-# SPECIALISE INLINE newStrategyQueue :: FCFS -> Simulation IO (StrategyQueue IO FCFS a) #-}
  newStrategyQueue s =
    fmap FCFSQueue $
    do session <- liftParameter simulationSession
       liftComp $ LL.newList session

  {-# SPECIALISE INLINE strategyQueueNull :: StrategyQueue IO FCFS a -> Event IO Bool #-}
  strategyQueueNull (FCFSQueue q) = liftComp $ LL.listNull q

instance QueueStrategy m FCFS => DequeueStrategy m FCFS where

  {-# SPECIALISE INLINE strategyDequeue :: StrategyQueue IO FCFS a -> Event IO a #-}
  strategyDequeue (FCFSQueue q) =
    liftComp $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

instance DequeueStrategy m FCFS => EnqueueStrategy m FCFS where

  {-# SPECIALISE INLINE strategyEnqueue :: StrategyQueue IO FCFS a -> a -> Event IO () #-}
  strategyEnqueue (FCFSQueue q) i = liftComp $ LL.listAddLast q i

instance Comp m => QueueStrategy m LCFS where

  newtype StrategyQueue m LCFS a = LCFSQueue (LL.DoubleLinkedList m a)

  {-# SPECIALISE INLINE newStrategyQueue :: LCFS -> Simulation IO (StrategyQueue IO LCFS a) #-}
  newStrategyQueue s =
    fmap LCFSQueue $
    do session <- liftParameter simulationSession
       liftComp $ LL.newList session
       
  {-# SPECIALISE INLINE strategyQueueNull :: StrategyQueue IO LCFS a -> Event IO Bool #-}
  strategyQueueNull (LCFSQueue q) = liftComp $ LL.listNull q

instance QueueStrategy m LCFS => DequeueStrategy m LCFS where

  {-# SPECIALISE INLINE strategyDequeue :: StrategyQueue IO LCFS a -> Event IO a #-}
  strategyDequeue (LCFSQueue q) =
    liftComp $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

instance DequeueStrategy m LCFS => EnqueueStrategy m LCFS where

  {-# SPECIALISE INLINE strategyEnqueue :: StrategyQueue IO LCFS a -> a -> Event IO () #-}
  strategyEnqueue (LCFSQueue q) i = liftComp $ LL.listInsertFirst q i

instance Comp m => QueueStrategy m StaticPriorities where

  newtype StrategyQueue m StaticPriorities a = StaticPriorityQueue (PQ.PriorityQueue m a)

  {-# SPECIALISE INLINE newStrategyQueue :: StaticPriorities -> Simulation IO (StrategyQueue IO StaticPriorities a) #-}
  newStrategyQueue s =
    fmap StaticPriorityQueue $
    do session <- liftParameter simulationSession
       liftComp $ PQ.newQueue session

  {-# SPECIALISE INLINE strategyQueueNull :: StrategyQueue IO StaticPriorities a -> Event IO Bool #-}
  strategyQueueNull (StaticPriorityQueue q) = liftComp $ PQ.queueNull q

instance QueueStrategy m StaticPriorities => DequeueStrategy m StaticPriorities where

  {-# SPECIALISE INLINE strategyDequeue :: StrategyQueue IO StaticPriorities a -> Event IO a #-}
  strategyDequeue (StaticPriorityQueue q) =
    liftComp $
    do (_, i) <- PQ.queueFront q
       PQ.dequeue q
       return i

instance DequeueStrategy m StaticPriorities => PriorityQueueStrategy m StaticPriorities Double where

  {-# SPECIALISE INLINE strategyEnqueueWithPriority :: StrategyQueue IO StaticPriorities a -> Double -> a -> Event IO () #-}
  strategyEnqueueWithPriority (StaticPriorityQueue q) p i = liftComp $ PQ.enqueue q p i

instance Comp m => QueueStrategy m SIRO where

  newtype StrategyQueue m SIRO a = SIROQueue (V.Vector m a)
  
  {-# SPECIALISE INLINE newStrategyQueue :: SIRO -> Simulation IO (StrategyQueue IO SIRO a) #-}
  newStrategyQueue s =
    fmap SIROQueue $
    do session <- liftParameter simulationSession
       liftComp $ V.newVector session

  {-# SPECIALISE INLINE strategyQueueNull :: StrategyQueue IO SIRO a -> Event IO Bool #-}
  strategyQueueNull (SIROQueue q) =
    liftComp $
    do n <- V.vectorCount q
       return (n == 0)

instance QueueStrategy m SIRO => DequeueStrategy m SIRO where

  {-# SPECIALISE INLINE strategyDequeue :: StrategyQueue IO SIRO a -> Event IO a #-}
  strategyDequeue (SIROQueue q) =
    do n <- liftComp $ V.vectorCount q
       i <- liftParameter $ randomUniformInt 0 (n - 1)
       x <- liftComp $ V.readVector q i
       liftComp $ V.vectorDeleteAt q i
       return x

instance DequeueStrategy m SIRO => EnqueueStrategy m SIRO where

  {-# SPECIALISE INLINE strategyEnqueue :: StrategyQueue IO SIRO a -> a -> Event IO () #-}
  strategyEnqueue (SIROQueue q) i = liftComp $ V.appendVector q i
