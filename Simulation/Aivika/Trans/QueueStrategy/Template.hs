
{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Trans.QueueStrategy.Template
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the queue strategies based on template computation.
--
module Simulation.Aivika.Trans.QueueStrategy.Template
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

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Parameter.Random
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.EventQueue
import Simulation.Aivika.Trans.QueueStrategy

import qualified Simulation.Aivika.Trans.DoubleLinkedList as LL
import qualified Simulation.Aivika.Trans.PriorityQueue as PQ
import qualified Simulation.Aivika.Trans.Vector as V

sessionParameter :: TemplateComp m => Simulation m (Session m)
{-# INLINE sessionParameter #-}
sessionParameter = liftParameter simulationSession

instance TemplateComp m => QueueStrategy m FCFS where

  newtype StrategyQueue m FCFS a = TemplateFCFSQueue (LL.DoubleLinkedList m a)

  {-# INLINE newStrategyQueue #-}
  newStrategyQueue s = fmap TemplateFCFSQueue $ sessionParameter >>= liftComp . LL.newList

  {-# INLINE strategyQueueNull #-}
  strategyQueueNull (TemplateFCFSQueue q) = liftComp $ LL.listNull q

instance TemplateComp m => DequeueStrategy m FCFS where

  {-# INLINE strategyDequeue #-}
  strategyDequeue (TemplateFCFSQueue q) =
    liftComp $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

instance TemplateComp m => EnqueueStrategy m FCFS where

  {-# INLINE strategyEnqueue #-}
  strategyEnqueue (TemplateFCFSQueue q) i = liftComp $ LL.listAddLast q i

instance TemplateComp m => QueueStrategy m LCFS where

  newtype StrategyQueue m LCFS a = TemplateLCFSQueue (LL.DoubleLinkedList m a)

  {-# INLINE newStrategyQueue #-}
  newStrategyQueue s = fmap TemplateLCFSQueue $ sessionParameter >>= liftComp . LL.newList
       
  {-# INLINE strategyQueueNull #-}
  strategyQueueNull (TemplateLCFSQueue q) = liftComp $ LL.listNull q

instance TemplateComp m => DequeueStrategy m LCFS where

  {-# INLINE strategyDequeue #-}
  strategyDequeue (TemplateLCFSQueue q) =
    liftComp $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

instance TemplateComp m => EnqueueStrategy m LCFS where

  {-# INLINE strategyEnqueue #-}
  strategyEnqueue (TemplateLCFSQueue q) i = liftComp $ LL.listInsertFirst q i

instance TemplateComp m => QueueStrategy m StaticPriorities where

  newtype StrategyQueue m StaticPriorities a = TemplateStaticPriorityQueue (PQ.PriorityQueue m a)

  {-# INLINE newStrategyQueue #-}
  newStrategyQueue s = fmap TemplateStaticPriorityQueue $ sessionParameter >>= liftComp . PQ.newQueue

  {-# INLINE strategyQueueNull #-}
  strategyQueueNull (TemplateStaticPriorityQueue q) = liftComp $ PQ.queueNull q

instance TemplateComp m => DequeueStrategy m StaticPriorities where

  {-# INLINE strategyDequeue #-}
  strategyDequeue (TemplateStaticPriorityQueue q) =
    liftComp $
    do (_, i) <- PQ.queueFront q
       PQ.dequeue q
       return i

instance TemplateComp m => PriorityQueueStrategy m StaticPriorities Double where

  {-# INLINE strategyEnqueueWithPriority #-}
  strategyEnqueueWithPriority (TemplateStaticPriorityQueue q) p i = liftComp $ PQ.enqueue q p i

instance TemplateComp m => QueueStrategy m SIRO where

  newtype StrategyQueue m SIRO a = TemplateSIROQueue (V.Vector m a)
  
  {-# INLINE newStrategyQueue #-}
  newStrategyQueue s = fmap TemplateSIROQueue $ liftParameter >>= liftComp . V.newVector

  {-# INLINE strategyQueueNull #-}
  strategyQueueNull (TemplateSIROQueue q) =
    liftComp $
    do n <- V.vectorCount q
       return (n == 0)

instance TemplateComp m => DequeueStrategy m SIRO where

  {-# INLINE strategyDequeue #-}
  strategyDequeue (TemplateSIROQueue q) =
    do n <- liftComp $ V.vectorCount q
       i <- liftParameter $ randomUniformInt 0 (n - 1)
       x <- liftComp $ V.readVector q i
       liftComp $ V.vectorDeleteAt q i
       return x

instance TemplateComp m => EnqueueStrategy m SIRO where

  {-# INLINE strategyEnqueue #-}
  strategyEnqueue (TemplateSIROQueue q) i = liftComp $ V.appendVector q i
