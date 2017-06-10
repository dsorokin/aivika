
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}

-- |
-- Module     : Simulation.Aivika.QueueStrategy
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines the queue strategies.
--
module Simulation.Aivika.QueueStrategy where

import System.Random
import Control.Monad.Trans
import Data.Maybe

import Simulation.Aivika.Simulation
import Simulation.Aivika.Event
import Simulation.Aivika.DoubleLinkedList
import qualified Simulation.Aivika.PriorityQueue as PQ
import qualified Simulation.Aivika.Vector as V

-- | Defines the basic queue strategy.
class QueueStrategy s where

  -- | A queue used by the strategy.
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

-- | Defines a strategy with support of the deleting operation.
class DequeueStrategy s => DeletingQueueStrategy s where

  -- | Remove the element and return a flag indicating whether
  -- the element was found and removed.
  strategyQueueDelete :: Eq i
                         => StrategyQueue s i
                         -- ^ the queue
                         -> i
                         -- ^ the element
                         -> Event Bool
                         -- ^ whether the element was found and removed
  strategyQueueDelete s i = fmap isJust $ strategyQueueDeleteBy s (== i)

  -- | Remove an element satisfying the predicate and return the element if found.
  strategyQueueDeleteBy :: StrategyQueue s i
                           -- ^ the queue
                           -> (i -> Bool)
                           -- ^ the predicate
                           -> Event (Maybe i)
                           -- ^ the element if it was found and removed

  -- | Detect whether the specified element is contained in the queue.
  strategyQueueContains :: Eq i
                           => StrategyQueue s i
                           -- ^ the queue
                           -> i
                           -- ^ the element to find
                           -> Event Bool
                           -- ^ whether the element is contained in the queue
  strategyQueueContains s i = fmap isJust $ strategyQueueContainsBy s (== i)

  -- | Detect whether an element satifying the specified predicate is contained in the queue.
  strategyQueueContainsBy :: StrategyQueue s i
                             -- ^ the queue
                             -> (i -> Bool)
                             -- ^ the predicate
                             -> Event (Maybe i)
                             -- ^ the element if it was found

-- | Strategy: First Come - First Served (FCFS).
data FCFS = FCFS deriving (Eq, Ord, Show)

-- | Strategy: Last Come - First Served (LCFS)
data LCFS = LCFS deriving (Eq, Ord, Show)

-- | Strategy: Service in Random Order (SIRO).
data SIRO = SIRO deriving (Eq, Ord, Show)

-- | Strategy: Static Priorities. It uses the priority queue.
data StaticPriorities = StaticPriorities deriving (Eq, Ord, Show)

-- | An implementation of the 'FCFS' queue strategy.
instance QueueStrategy FCFS where

  -- | A queue used by the 'FCFS' strategy.
  newtype StrategyQueue FCFS i = FCFSQueue (DoubleLinkedList i)

  newStrategyQueue s = fmap FCFSQueue $ liftIO newList

  strategyQueueNull (FCFSQueue q) = liftIO $ listNull q

-- | An implementation of the 'FCFS' queue strategy.
instance DequeueStrategy FCFS where

  strategyDequeue (FCFSQueue q) =
    liftIO $
    do i <- listFirst q
       listRemoveFirst q
       return i

-- | An implementation of the 'FCFS' queue strategy.
instance EnqueueStrategy FCFS where

  strategyEnqueue (FCFSQueue q) i = liftIO $ listAddLast q i

-- | An implementation of the 'FCFS' queue strategy.
instance DeletingQueueStrategy FCFS where

  strategyQueueDeleteBy (FCFSQueue q) p = liftIO $ listRemoveBy q p

  strategyQueueContainsBy (FCFSQueue q) p = liftIO $ listContainsBy q p

-- | An implementation of the 'LCFS' queue strategy.
instance QueueStrategy LCFS where

  -- | A queue used by the 'LCFS' strategy.
  newtype StrategyQueue LCFS i = LCFSQueue (DoubleLinkedList i)
  
  newStrategyQueue s = fmap LCFSQueue $ liftIO newList
       
  strategyQueueNull (LCFSQueue q) = liftIO $ listNull q

-- | An implementation of the 'LCFS' queue strategy.
instance DequeueStrategy LCFS where

  strategyDequeue (LCFSQueue q) =
    liftIO $
    do i <- listFirst q
       listRemoveFirst q
       return i

-- | An implementation of the 'LCFS' queue strategy.
instance EnqueueStrategy LCFS where

  strategyEnqueue (LCFSQueue q) i = liftIO $ listInsertFirst q i

-- | An implementation of the 'LCFS' queue strategy.
instance DeletingQueueStrategy LCFS where

  strategyQueueDeleteBy (LCFSQueue q) p = liftIO $ listRemoveBy q p

  strategyQueueContainsBy (LCFSQueue q) p = liftIO $ listContainsBy q p

-- | An implementation of the 'StaticPriorities' queue strategy.
instance QueueStrategy StaticPriorities where

  -- | A queue used by the 'StaticPriorities' strategy.
  newtype StrategyQueue StaticPriorities i = StaticPriorityQueue (PQ.PriorityQueue i)
  
  newStrategyQueue s = fmap StaticPriorityQueue $ liftIO PQ.newQueue

  strategyQueueNull (StaticPriorityQueue q) = liftIO $ PQ.queueNull q

-- | An implementation of the 'StaticPriorities' queue strategy.
instance DequeueStrategy StaticPriorities where

  strategyDequeue (StaticPriorityQueue q) =
    liftIO $
    do (_, i) <- PQ.queueFront q
       PQ.dequeue q
       return i

-- | An implementation of the 'StaticPriorities' queue strategy.
instance PriorityQueueStrategy StaticPriorities Double where

  strategyEnqueueWithPriority (StaticPriorityQueue q) p i = liftIO $ PQ.enqueue q p i

-- | An implementation of the 'StaticPriorities' queue strategy.
instance DeletingQueueStrategy StaticPriorities where

  strategyQueueDeleteBy (StaticPriorityQueue q) p = liftIO $ PQ.queueDeleteBy q p

  strategyQueueContainsBy (StaticPriorityQueue q) p = liftIO $ PQ.queueContainsBy q p

-- | An implementation of the 'SIRO' queue strategy.
instance QueueStrategy SIRO where

  -- | A queue used by the 'SIRO' strategy.
  newtype StrategyQueue SIRO i = SIROQueue (V.Vector i)

  newStrategyQueue s = fmap SIROQueue $ liftIO V.newVector

  strategyQueueNull (SIROQueue q) =
    liftIO $
    do n <- V.vectorCount q
       return (n == 0)

-- | An implementation of the 'SIRO' queue strategy.
instance DequeueStrategy SIRO where

  strategyDequeue (SIROQueue q) =
    liftIO $
    do n <- V.vectorCount q
       i <- getStdRandom (randomR (0, n - 1))
       x <- V.readVector q i
       V.vectorDeleteAt q i
       return x

-- | An implementation of the 'SIRO' queue strategy.
instance EnqueueStrategy SIRO where

  strategyEnqueue (SIROQueue q) i = liftIO $ V.appendVector q i

-- | An implementation of the 'SIRO' queue strategy.
instance DeletingQueueStrategy SIRO where

  strategyQueueDeleteBy (SIROQueue q) p = liftIO $ V.vectorDeleteBy q p

  strategyQueueContainsBy (SIROQueue q) p = liftIO $ V.vectorContainsBy q p
