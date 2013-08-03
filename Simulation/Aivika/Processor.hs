
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Processor
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The processor of simulation data.
--
module Simulation.Aivika.Processor
       (-- * Processor Type
        Processor(..),
        -- * Creating Simple Processor
        simpleProcessor,
        -- * Specifying Identifier
        processorUsingId,
        -- * Parallelizing Processors
        processorParallel,
        processorParallelUsingIds,
        processorQueuedParallel,
        processorQueuedParallelUsingIds,
        processorPriorityParallel,
        processorPriorityParallelUsingIds) where

import qualified Control.Category as C
import Control.Arrow

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Process
import Simulation.Aivika.Stream
import Simulation.Aivika.QueueStrategy

-- | Represents a processor of simulation data.
newtype Processor a b =
  Processor { runProcessor :: Stream a -> Stream b
              -- ^ Run the processor.
            }

instance C.Category Processor where

  id  = Processor id

  Processor x . Processor y = Processor (x . y)

-- The implementation is based on article
-- A New Notation for Arrows by Ross Paterson,
-- although my streams are different and they
-- already depend on the Process monad,
-- while the pure streams were considered in the
-- mentioned article.
instance Arrow Processor where

  arr = Processor . mapStream

  first (Processor f) =
    Processor $ \xys ->
    Cons $
    do ~(xs, ys) <- liftSimulation $ unzipStream xys
       runStream $ zipStreamSeq (f xs) ys

  second (Processor f) =
    Processor $ \xys ->
    Cons $
    do ~(xs, ys) <- liftSimulation $ unzipStream xys
       runStream $ zipStreamSeq xs (f ys)

  Processor f *** Processor g =
    Processor $ \xys ->
    Cons $
    do ~(xs, ys) <- liftSimulation $ unzipStream xys
       runStream $ zipStreamParallel (f xs) (g ys)

-- The implementation is based on article
-- A New Notation for Arrows by Ross Paterson,
-- although my streams are different and they
-- already depend on the Process monad,
-- while the pure streams were considered in the
-- mentioned article.
instance ArrowLoop Processor where

  loop (Processor f) =
    Processor $ \xs ->
    Cons $
    do Cons zs <- liftSimulation $
                  simulationLoop (\(xs, ys) ->
                                   unzipStream $ f $ zipStreamSeq xs ys) xs
       zs

simulationLoop :: ((b, d) -> Simulation (c, d)) -> b -> Simulation c
simulationLoop f b =
  mdo (c, d) <- f (b, d)
      return c

-- The implementation is based on article
-- A New Notation for Arrows by Ross Paterson,
-- although my streams are different and they
-- already depend on the Process monad,
-- while the pure streams were considered in the
-- mentioned article.
instance ArrowChoice Processor where

  left (Processor f) =
    Processor $ \xs ->
    Cons $
    do ys <- liftSimulation $ memoStream xs
       runStream $ replaceLeftStream ys (f $ leftStream ys)

  right (Processor f) =
    Processor $ \xs ->
    Cons $
    do ys <- liftSimulation $ memoStream xs
       runStream $ replaceRightStream ys (f $ rightStream ys)
           
instance SimulationLift (Processor a) where
  liftSimulation = Processor . mapStreamM . const . liftSimulation

instance DynamicsLift (Processor a) where
  liftDynamics = Processor . mapStreamM . const . liftDynamics

instance EventLift (Processor a) where
  liftEvent = Processor . mapStreamM . const . liftEvent

instance ProcessLift (Processor a) where
  liftProcess = Processor . mapStreamM . const    -- data first!

-- | Create a simple processor by the specified handling function
-- that runs the discontinuous process for each input value to get the output.
simpleProcessor :: (a -> Process b) -> Processor a b
simpleProcessor = Processor . mapStreamM

-- | Create a processor that will use the specified process identifier.
-- It can be useful to refer to the underlying 'Process' computation which
-- can be passivated, interrupted, canceled and so on. See also the
-- 'processUsingId' function.
processorUsingId :: ProcessId -> Processor a b -> Processor a b
processorUsingId pid (Processor f) =
  Processor $ Cons . processUsingId pid . runStream . f

-- | Launches the specified processors in parallel consuming the same input
-- stream and producing a combined output stream.
--
-- If you don't know what the enqueue strategies to apply, then
-- you will probably need 'FCFS' for the both parameters, or
-- function 'processorParallel' that does namely this.
processorQueuedParallel :: (EnqueueStrategy si qi,
                            EnqueueStrategy so qo)
                           => si
                           -- ^ the strategy applied for enqueuing the input data
                           -> so
                           -- ^ the strategy applied for enqueuing the output data
                           -> [Processor a b]
                           -- ^ the processors to parallelize
                           -> Processor a b
                           -- ^ the parallelized processor
processorQueuedParallel si so ps =
  Processor $ \xs ->
  Cons $
  do let n = length ps
     input <- liftSimulation $ splitStreamQueuing si n xs
     let results = flip map (zip input ps) $ \(input, p) ->
           runProcessor p $ input
         output  = concatQueuedStreams so results
     runStream output

-- | Launches the specified processors in parallel using priorities for combining the output.
processorPriorityParallel :: (EnqueueStrategy si qi,
                              PriorityQueueStrategy so qo po)
                             => si
                             -- ^ the strategy applied for enqueuing the input data
                             -> so
                             -- ^ the strategy applied for enqueuing the output data
                             -> [Processor a (po, b)]
                             -- ^ the processors to parallelize
                             -> Processor a b
                             -- ^ the parallelized processor
processorPriorityParallel si so ps =
  Processor $ \xs ->
  Cons $
  do let n = length ps
     input <- liftSimulation $ splitStreamQueuing si n xs
     let results = flip map (zip input ps) $ \(input, p) ->
           runProcessor p $ input
         output  = concatPriorityStreams so results
     runStream output

-- | Launches the specified processors in parallel using the provided identifiers,
-- consuming the same input stream and producing a combined output stream.
-- Specifying the process indentifiers is useful to refer to the underlying 'Process'
-- computations which can be passivated, interrupted, canceled and so on.
--
-- If you don't know what the enqueue strategies to apply, then
-- you will probably need 'FCFS' for the both parameters, or function
-- 'processorParallelUsingIds' that does namely this.
processorQueuedParallelUsingIds :: (EnqueueStrategy si qi,
                                    EnqueueStrategy so qo)
                                   => si
                                   -- ^ the strategy applied for enqueuing the input data
                                   -> so
                                   -- ^ the strategy applied for enqueuing the output data
                                   -> [(ProcessId, Processor a b)]
                                   -- ^ the processors to parallelize
                                   -> Processor a b
                                   -- ^ the parallelized processor
processorQueuedParallelUsingIds si so ps = processorQueuedParallel si so ps' where
  ps' = map (\(pid, p) -> processorUsingId pid p) ps

-- | Like 'processorPriorityParallel' but allows specifying the process identifiers.
processorPriorityParallelUsingIds :: (EnqueueStrategy si qi,
                                      PriorityQueueStrategy so qo po)
                                     => si
                                     -- ^ the strategy applied for enqueuing the input data
                                     -> so
                                     -- ^ the strategy applied for enqueuing the output data
                                     -> [(ProcessId, Processor a (po, b))]
                                     -- ^ the processors to parallelize
                                     -> Processor a b
                                     -- ^ the parallelized processor
processorPriorityParallelUsingIds si so ps = processorPriorityParallel si so ps' where
  ps' = map (\(pid, p) -> processorUsingId pid p) ps

-- | Launches the processors in parallel consuming the same input stream and producing
-- a combined output stream. This version applies the 'FCFS' strategy both for input
-- and output, which suits the most part of uses cases.
processorParallel :: [Processor a b] -> Processor a b
processorParallel = processorQueuedParallel FCFS FCFS

-- | Launches the processors in parallel using the specified indentifiers, consuming
-- the same input stream and producing a combined output stream. This version applies
-- the 'FCFS' strategy both for input and output, which suits the most part of uses cases.
processorParallelUsingIds :: [(ProcessId, Processor a b)] -> Processor a b
processorParallelUsingIds = processorQueuedParallelUsingIds FCFS FCFS