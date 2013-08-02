
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
       (Processor(..),
        simpleProcessor,
        processorUsingId,
        processorParallel,
        processorParallelUsingIds,
        newRoundRobbinProcessor,
        newRoundRobbinProcessorUsingIds) where

import qualified Control.Category as C
import Control.Arrow

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.Stream

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

-- | Launches the specified processors in parallel.
processorParallel :: [Processor a b] -> Processor a b
processorParallel = undefined

-- | Launches the specified processors in parallel using the provided identifiers.
-- It is useful to refer to the underlying 'Process' computations which can be
-- passivated, interrupted, canceled and so on.
processorParallelUsingIds :: [(ProcessId, Processor a b)] -> Processor a b
processorParallelUsingIds = undefined

-- | Create a new Round-Robbin processor.
newRoundRobbinProcessor :: (a -> (Process Double, Process b))
                           -- ^ A function of input that returns the timeout
                           -- and process which should be performed within
                           -- the specified timeout.
                           -> Simulation (Processor a b)
newRoundRobbinProcessor = undefined

-- | Create a new Round-Robbin processor that uses the specified processor identifiers.
newRoundRobbinProcessorUsingIds :: (a -> (ProcessId, Process Double, Process b))
                                   -- ^ A function of input that returns a new
                                   -- process identifier, the timeout
                                   -- and the process itself which will be launched
                                   -- with the specified identifier and which should
                                   -- be performed within the specified timeout.
                                   -> Simulation (Processor a b)
newRoundRobbinProcessorUsingIds = undefined