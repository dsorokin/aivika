
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
module Simulation.Aivika.Trans.QueueStrategy.Template() where

import Control.Monad.Trans

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.Session.Template
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Comp.Template
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Parameter.Random
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.EventQueue
import Simulation.Aivika.Trans.QueueStrategy

import qualified Simulation.Aivika.Trans.DoubleLinkedList as LL
import qualified Simulation.Aivika.Trans.PriorityQueue as PQ
import qualified Simulation.Aivika.Trans.Vector as V

instance ProtoComp m => QueueStrategy (TemplateComp m) FCFS where

  newtype StrategyQueue (TemplateComp m) FCFS a =
    TemplateFCFSQueue (LL.DoubleLinkedList (TemplateComp m) a)

  {-# INLINE newStrategyQueue #-}
  newStrategyQueue s =
    do TemplateSession sn <- liftParameter simulationSession
       q <- liftComp $ LL.newList sn
       return $ TemplateFCFSQueue q

  {-# INLINE strategyQueueNull #-}
  strategyQueueNull (TemplateFCFSQueue q) =
    liftComp $ LL.listNull q

instance ProtoComp m => DequeueStrategy (TemplateComp m) FCFS where

  {-# INLINE strategyDequeue #-}
  strategyDequeue (TemplateFCFSQueue q) =
    liftComp $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

instance ProtoComp m => EnqueueStrategy (TemplateComp m) FCFS where

  {-# INLINE strategyEnqueue #-}
  strategyEnqueue (TemplateFCFSQueue q) i =
    liftComp $ LL.listAddLast q i

instance ProtoComp m => QueueStrategy (TemplateComp m) LCFS where

  newtype StrategyQueue (TemplateComp m) LCFS a =
    TemplateLCFSQueue (LL.DoubleLinkedList (TemplateComp m) a)

  {-# INLINE newStrategyQueue #-}
  newStrategyQueue s =
    fmap TemplateLCFSQueue $
    liftParameter simulationSession >>=
    liftComp . LL.newList
       
  {-# INLINE strategyQueueNull #-}
  strategyQueueNull (TemplateLCFSQueue q) =
    liftComp $ LL.listNull q

instance ProtoComp m => DequeueStrategy (TemplateComp m) LCFS where

  {-# INLINE strategyDequeue #-}
  strategyDequeue (TemplateLCFSQueue q) =
    liftComp $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

instance ProtoComp m => EnqueueStrategy (TemplateComp m) LCFS where

  {-# INLINE strategyEnqueue #-}
  strategyEnqueue (TemplateLCFSQueue q) i =
    liftComp $ LL.listInsertFirst q i

instance ProtoComp m => QueueStrategy (TemplateComp m) StaticPriorities where

  newtype StrategyQueue (TemplateComp m) StaticPriorities a =
    TemplateStaticPriorityQueue (PQ.PriorityQueue (TemplateComp m) a)

  {-# INLINE newStrategyQueue #-}
  newStrategyQueue s =
    fmap TemplateStaticPriorityQueue $
    liftParameter simulationSession >>=
    liftComp . PQ.newQueue

  {-# INLINE strategyQueueNull #-}
  strategyQueueNull (TemplateStaticPriorityQueue q) =
    liftComp $ PQ.queueNull q

instance ProtoComp m => DequeueStrategy (TemplateComp m) StaticPriorities where

  {-# INLINE strategyDequeue #-}
  strategyDequeue (TemplateStaticPriorityQueue q) =
    liftComp $
    do (_, i) <- PQ.queueFront q
       PQ.dequeue q
       return i

instance ProtoComp m => PriorityQueueStrategy (TemplateComp m) StaticPriorities Double where

  {-# INLINE strategyEnqueueWithPriority #-}
  strategyEnqueueWithPriority (TemplateStaticPriorityQueue q) p i =
    liftComp $ PQ.enqueue q p i

instance ProtoComp m => QueueStrategy (TemplateComp m) SIRO where

  newtype StrategyQueue (TemplateComp m) SIRO a =
    TemplateSIROQueue (V.Vector (TemplateComp m) a)
  
  {-# INLINE newStrategyQueue #-}
  newStrategyQueue s =
    fmap TemplateSIROQueue $
    liftParameter simulationSession >>=
    liftComp . V.newVector

  {-# INLINE strategyQueueNull #-}
  strategyQueueNull (TemplateSIROQueue q) =
    liftComp $
    do n <- V.vectorCount q
       return (n == 0)

instance ProtoComp m => DequeueStrategy (TemplateComp m) SIRO where

  {-# INLINE strategyDequeue #-}
  strategyDequeue (TemplateSIROQueue q) =
    do n <- liftComp $ V.vectorCount q
       i <- liftParameter $ randomUniformInt 0 (n - 1)
       x <- liftComp $ V.readVector q i
       liftComp $ V.vectorDeleteAt q i
       return x

instance ProtoComp m => EnqueueStrategy (TemplateComp m) SIRO where

  {-# INLINE strategyEnqueue #-}
  strategyEnqueue (TemplateSIROQueue q) i =
    liftComp $ V.appendVector q i
