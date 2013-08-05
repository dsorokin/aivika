
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
        statefulProcessor,
        -- * Specifying Identifier
        processorUsingId,
        -- * Creating Queue Processor
        queueProcessor,
        queueProcessorLoop,
        -- * Parallelizing Processors
        processorParallel,
        processorParallelUsingIds,
        processorQueuedParallel,
        processorQueuedParallelUsingIds,
        processorPrioritisingOutputParallel,
        processorPrioritisingOutputParallelUsingIds,
        processorPrioritisingInputParallel,
        processorPrioritisingInputParallelUsingIds,
        processorPrioritisingInputOutputParallel,
        processorPrioritisingInputOutputParallelUsingIds) where

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
    do (xs, ys) <- liftSimulation $ unzipStream xys
       runStream $ zipStreamSeq (f xs) ys

  second (Processor f) =
    Processor $ \xys ->
    Cons $
    do (xs, ys) <- liftSimulation $ unzipStream xys
       runStream $ zipStreamSeq xs (f ys)

  Processor f *** Processor g =
    Processor $ \xys ->
    Cons $
    do (xs, ys) <- liftSimulation $ unzipStream xys
       runStream $ zipStreamSeq (f xs) (g ys)

-- N.B.
-- Very probably, Processor is not ArrowLoop,
-- which would be natural as Process is not MonadFix,
-- for the discontinuous process is not irreversible
-- and the time flows in one direction only.
--
-- -- The implementation is based on article
-- -- A New Notation for Arrows by Ross Paterson,
-- -- although my streams are different and they
-- -- already depend on the Process monad,
-- -- while the pure streams were considered in the
-- -- mentioned article.
-- instance ArrowLoop Processor where
-- 
--   loop (Processor f) =
--     Processor $ \xs ->
--     Cons $
--     do Cons zs <- liftSimulation $
--                   simulationLoop (\(xs, ys) ->
--                                    unzipStream $ f $ zipStreamSeq xs ys) xs
--        zs
-- 
-- simulationLoop :: ((b, d) -> Simulation (c, d)) -> b -> Simulation c
-- simulationLoop f b =
--   mdo (c, d) <- f (b, d)
--       return c

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

instance ArrowZero Processor where

  zeroArrow = Processor $ const emptyStream

instance ArrowPlus Processor where

  (Processor f) <+> (Processor g) =
    Processor $ \xs ->
    Cons $
    do [xs1, xs2] <- liftSimulation $ splitStream 2 xs
       runStream $ mergeStreams (f xs1) (g xs2)

-- These instances are meaningless:
-- 
-- instance SimulationLift (Processor a) where
--   liftSimulation = Processor . mapStreamM . const . liftSimulation
-- 
-- instance DynamicsLift (Processor a) where
--   liftDynamics = Processor . mapStreamM . const . liftDynamics
-- 
-- instance EventLift (Processor a) where
--   liftEvent = Processor . mapStreamM . const . liftEvent
-- 
-- instance ProcessLift (Processor a) where
--   liftProcess = Processor . mapStreamM . const    -- data first!

-- | Create a simple processor by the specified handling function
-- that runs the discontinuous process for each input value to get the output.
simpleProcessor :: (a -> Process b) -> Processor a b
simpleProcessor = Processor . mapStreamM

-- | Like 'simpleProcessor' but allows creating a processor that has a state
-- which is passed in to every new iteration.
statefulProcessor :: s -> ((s, a) -> Process (s, b)) -> Processor a b
statefulProcessor s f =
  Processor $ \xs -> Cons $ loop s xs where
    loop s xs =
      do (a, xs') <- runStream xs
         (s', b) <- f (s, a)
         return (b, Cons $ loop s' xs')

-- | Create a processor that will use the specified process identifier.
-- It can be useful to refer to the underlying 'Process' computation which
-- can be passivated, interrupted, canceled and so on. See also the
-- 'processUsingId' function for more details.
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
           runProcessor p input
         output  = concatQueuedStreams so results
     runStream output

-- | Launches the specified processors in parallel using priorities for combining the output.
processorPrioritisingOutputParallel :: (EnqueueStrategy si qi,
                                        PriorityQueueStrategy so qo po)
                                       => si
                                       -- ^ the strategy applied for enqueuing the input data
                                       -> so
                                       -- ^ the strategy applied for enqueuing the output data
                                       -> [Processor a (po, b)]
                                       -- ^ the processors to parallelize
                                       -> Processor a b
                                       -- ^ the parallelized processor
processorPrioritisingOutputParallel si so ps =
  Processor $ \xs ->
  Cons $
  do let n = length ps
     input <- liftSimulation $ splitStreamQueuing si n xs
     let results = flip map (zip input ps) $ \(input, p) ->
           runProcessor p input
         output  = concatPriorityStreams so results
     runStream output

-- | Launches the specified processors in parallel using priorities for consuming the intput.
processorPrioritisingInputParallel :: (PriorityQueueStrategy si qi pi,
                                       EnqueueStrategy so qo)
                                      => si
                                      -- ^ the strategy applied for enqueuing the input data
                                      -> so
                                      -- ^ the strategy applied for enqueuing the output data
                                      -> [(Stream pi, Processor a b)]
                                      -- ^ the streams of input priorities and the processors
                                      -- to parallelize
                                      -> Processor a b
                                      -- ^ the parallelized processor
processorPrioritisingInputParallel si so ps =
  Processor $ \xs ->
  Cons $
  do input <- liftSimulation $ splitStreamPrioritising si (map fst ps) xs
     let results = flip map (zip input ps) $ \(input, (_, p)) ->
           runProcessor p input
         output  = concatQueuedStreams so results
     runStream output

-- | Launches the specified processors in parallel using priorities for consuming
-- the input and combining the output.
processorPrioritisingInputOutputParallel :: (PriorityQueueStrategy si qi pi,
                                             PriorityQueueStrategy so qo po)
                                            => si
                                            -- ^ the strategy applied for enqueuing the input data
                                            -> so
                                            -- ^ the strategy applied for enqueuing the output data
                                            -> [(Stream pi, Processor a (po, b))]
                                            -- ^ the streams of input priorities and the processors
                                            -- to parallelize
                                            -> Processor a b
                                            -- ^ the parallelized processor
processorPrioritisingInputOutputParallel si so ps =
  Processor $ \xs ->
  Cons $
  do input <- liftSimulation $ splitStreamPrioritising si (map fst ps) xs
     let results = flip map (zip input ps) $ \(input, (_, p)) ->
           runProcessor p input
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

-- | Like 'processorPrioritisingOutputParallel' but allows specifying the process identifiers.
processorPrioritisingOutputParallelUsingIds :: (EnqueueStrategy si qi,
                                                PriorityQueueStrategy so qo po)
                                               => si
                                               -- ^ the strategy applied for enqueuing the input data
                                               -> so
                                               -- ^ the strategy applied for enqueuing the output data
                                               -> [(ProcessId, Processor a (po, b))]
                                               -- ^ the processors to parallelize
                                               -> Processor a b
                                               -- ^ the parallelized processor
processorPrioritisingOutputParallelUsingIds si so ps =
  processorPrioritisingOutputParallel si so ps' where
    ps' = map (\(pid, p) -> processorUsingId pid p) ps

-- | Like 'processorPrioritisingInputParallel' but allows specifying the process identifiers.
processorPrioritisingInputParallelUsingIds :: (PriorityQueueStrategy si qi pi,
                                               EnqueueStrategy so qo)
                                              => si
                                              -- ^ the strategy applied for enqueuing the input data
                                              -> so
                                              -- ^ the strategy applied for enqueuing the output data
                                              -> [(ProcessId, Stream pi, Processor a b)]
                                              -- ^ the streams of input priorities and the processors
                                              -- to parallelize
                                              -> Processor a b
                                              -- ^ the parallelized processor
processorPrioritisingInputParallelUsingIds si so ps =
  processorPrioritisingInputParallel si so ps' where
    ps' = map (\(pid, pi, p) -> (pi, processorUsingId pid p)) ps

-- | Like 'processorPrioritisingInputOutputParallel' but allows specifying the process identifiers.
processorPrioritisingInputOutputParallelUsingIds :: (PriorityQueueStrategy si qi pi,
                                                     PriorityQueueStrategy so qo po)
                                                    => si
                                                    -- ^ the strategy applied for enqueuing
                                                    -- the input data
                                                    -> so
                                                    -- ^ the strategy applied for enqueuing
                                                    -- the output data
                                                    -> [(ProcessId, Stream pi, Processor a (po, b))]
                                                    -- ^ the streams of input priorities and
                                                    -- the processors to parallelize
                                                    -> Processor a b
                                                    -- ^ the parallelized processor
processorPrioritisingInputOutputParallelUsingIds si so ps =
  processorPrioritisingInputOutputParallel si so ps' where
    ps' = map (\(pid, pi, p) -> (pi, processorUsingId pid p)) ps

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

-- | Create a processor that usually redirects an input to the queue in one
-- infinite process and then extracts data from it in another process by demand.
--
-- The standard use case is as follows:
--
-- @
--   do q <- newFCFSQueue capacity
--      let p :: Processor Int Int
--          p =
--            queueProcessor
--            (consumeStream $ enqueue q)
--            (repeatProcess $ dequeue q)
--      ...
-- @
--
-- The priority queues can be treated in the same manner additionally using
-- the 'mapStreamM', 'zipStreamSeq' or 'zipStreamParallel' combinators
-- to include the stream or several streams of priorities in the resulting computation. 
queueProcessor :: (Stream a -> Process ())
                  -- ^ a separate process to consume the input 
                  -> Stream b
                  -- ^ the resulting stream of data
                  -> Processor a b
queueProcessor consume output =
  Processor $ \xs ->
  Cons $
  do childProcess (consume xs)
     runStream output

-- | Like 'queueProcessor' but allows creating a loop when some items
-- can be returned to the queue to be processed again.
--
-- A typical use case is as follows:
--
-- @
--   do let capacity = 10
--      q <- newFCFSQueue capacity
--      let p :: Processor d (Either c b)
--          p = ...  -- process and decide what to do
--          qp = queueProcessorLoop
--               (\\a c -> consumeStream (enqueue q) $
--                        mergeStreams a c)
--               (repeatProcess $ dequeue q)
--               p
-- @
--
-- Note that we can decide in what order the two streams are handled when
-- consuming data.
--
-- The priority queues are processed in the same manner, although it may
-- require more glueing code.
queueProcessorLoop :: (Stream a -> Stream c -> Process ())
                      -- ^ consume two streams: the input values of type @a@
                      -- and the values of type @c@ redirected to the queue
                      -- by loop
                      -> Stream d
                      -- ^ the stream of data that may become results
                      -> Processor d (Either c b)
                      -- ^ processs and then decide what values of type @c@
                      -- should be redirected to the queue again
                      -> Processor a b
queueProcessorLoop consume preoutput filter =
  Processor $ \xs ->
  Cons $
  do (reverted, output) <-
       liftSimulation $
       partitionEitherStream $
       runProcessor filter preoutput
     childProcess (consume xs reverted)
     runStream output