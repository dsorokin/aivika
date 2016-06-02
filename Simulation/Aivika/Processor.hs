
-- |
-- Module     : Simulation.Aivika.Processor
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The processor of simulation data.
--
module Simulation.Aivika.Processor
       (-- * Processor Type
        Processor(..),
        -- * Processor Primitives
        emptyProcessor,
        arrProcessor,
        accumProcessor,
        withinProcessor,
        -- * Specifying Identifier
        processorUsingId,
        -- * Prefetch and Delay Processors
        prefetchProcessor,
        delayProcessor,
        -- * Buffer Processor
        bufferProcessor,
        bufferProcessorLoop,
        -- * Processing Queues
        queueProcessor,
        queueProcessorLoopMerging,
        queueProcessorLoopSeq,
        queueProcessorLoopParallel,
        -- * Sequencing Processors
        processorSeq,
        -- * Parallelizing Processors
        processorParallel,
        processorQueuedParallel,
        processorPrioritisingOutputParallel,
        processorPrioritisingInputParallel,
        processorPrioritisingInputOutputParallel,
        -- * Arrival Processor
        arrivalProcessor,
        -- * Utilities
        joinProcessor,
        -- * Failover
        failoverProcessor,
        -- * Integrating with Signals
        signalProcessor,
        processorSignaling,
        -- * Debugging
        traceProcessor) where

import qualified Control.Category as C
import Control.Arrow

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Cont
import Simulation.Aivika.Process
import Simulation.Aivika.Stream
import Simulation.Aivika.QueueStrategy
import Simulation.Aivika.Signal
import Simulation.Aivika.Internal.Arrival

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

-- | A processor that never finishes its work producing an 'emptyStream'.
emptyProcessor :: Processor a b
emptyProcessor = Processor $ const emptyStream

-- | Create a simple processor by the specified handling function
-- that runs the discontinuous process for each input value to get the output.
arrProcessor :: (a -> Process b) -> Processor a b
arrProcessor = Processor . mapStreamM

-- | Accumulator that outputs a value determined by the supplied function.
accumProcessor :: (acc -> a -> Process (acc, b)) -> acc -> Processor a b
accumProcessor f acc = Processor $ accumStream f acc

-- | Involve the computation with side effect when processing a stream of data.
withinProcessor :: Process () -> Processor a a
withinProcessor m =
  Processor $
  mapStreamM $ \a ->
  do { m; return a }

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
processorQueuedParallel :: (EnqueueStrategy si,
                            EnqueueStrategy so)
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
     input <- liftSimulation $ splitStreamQueueing si n xs
     let results = flip map (zip input ps) $ \(input, p) ->
           runProcessor p input
         output  = concatQueuedStreams so results
     runStream output

-- | Launches the specified processors in parallel using priorities for combining the output.
processorPrioritisingOutputParallel :: (EnqueueStrategy si,
                                        PriorityQueueStrategy so po)
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
     input <- liftSimulation $ splitStreamQueueing si n xs
     let results = flip map (zip input ps) $ \(input, p) ->
           runProcessor p input
         output  = concatPriorityStreams so results
     runStream output

-- | Launches the specified processors in parallel using priorities for consuming the intput.
processorPrioritisingInputParallel :: (PriorityQueueStrategy si pi,
                                       EnqueueStrategy so)
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
processorPrioritisingInputOutputParallel :: (PriorityQueueStrategy si pi,
                                             PriorityQueueStrategy so po)
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

-- | Launches the processors in parallel consuming the same input stream and producing
-- a combined output stream. This version applies the 'FCFS' strategy both for input
-- and output, which suits the most part of uses cases.
processorParallel :: [Processor a b] -> Processor a b
processorParallel = processorQueuedParallel FCFS FCFS

-- | Launches the processors sequentially using the 'prefetchProcessor' between them
-- to model an autonomous work of each of the processors specified.
processorSeq :: [Processor a a] -> Processor a a
processorSeq []  = emptyProcessor
processorSeq [p] = p
processorSeq (p : ps) = p >>> prefetchProcessor >>> processorSeq ps

-- | Create a buffer processor, where the process from the first argument
-- consumes the input stream but the stream passed in as the second argument
-- and produced usually by some other process is returned as an output.
-- This kind of processor is very useful for modeling the queues.
bufferProcessor :: (Stream a -> Process ())
                   -- ^ a separate process to consume the input 
                   -> Stream b
                   -- ^ the resulting stream of data
                   -> Processor a b
bufferProcessor consume output =
  Processor $ \xs ->
  Cons $
  do spawnProcess (consume xs)
     runStream output

-- | Like 'bufferProcessor' but allows creating a loop when some items
-- can be processed repeatedly. It is very useful for modeling the processors 
-- with queues and loop-backs.
bufferProcessorLoop :: (Stream a -> Stream c -> Process ())
                       -- ^ consume two streams: the input values of type @a@
                       -- and the values of type @c@ returned by the loop
                       -> Stream d
                       -- ^ the stream of data that may become results
                       -> Processor d (Either e b)
                       -- ^ process and then decide what values of type @e@
                       -- should be processed in the loop (this is a condition)
                       -> Processor e c
                       -- ^ process in the loop and then return a value
                       -- of type @c@ to the input again (this is a loop body)
                       -> Processor a b
bufferProcessorLoop consume preoutput cond body =
  Processor $ \xs ->
  Cons $
  do (reverted, output) <-
       liftSimulation $
       partitionEitherStream $
       runProcessor cond preoutput
     spawnProcess 
       (consume xs $ runProcessor body reverted)
     runStream output

-- | Return a processor with help of which we can model the queue.
--
-- Although the function doesn't refer to the queue directly, its main use case
-- is namely a processing of the queue. The first argument should be the enqueueing
-- operation, while the second argument should be the opposite dequeueing operation.
--
-- The reason is as follows. There are many possible combinations how the queues
-- can be modeled. There is no sense to enumerate all them creating a separate function
-- for each case. We can just use combinators to define exactly what we need.
--
-- So, the queue can lose the input items if the queue is full, or the input process
-- can suspend while the queue is full, or we can use priorities for enqueueing,
-- storing and dequeueing the items in different combinations. There are so many use
-- cases!
--
-- There is a hope that this function along with other similar functions from this
-- module is sufficient to cover the most important cases. Even if it is not sufficient
-- then you can use a more generic function 'bufferProcessor' which this function is
-- based on. In case of need, you can even write your own function from scratch. It is
-- quite easy actually.
queueProcessor :: (a -> Process ())
                  -- ^ enqueue the input item and wait
                  -- while the queue is full if required
                  -- so that there were no hanging items
                  -> Process b
                  -- ^ dequeue an output item
                  -> Processor a b
                  -- ^ the buffering processor
queueProcessor enqueue dequeue =
  bufferProcessor
  (consumeStream enqueue)
  (repeatProcess dequeue)

-- | Like 'queueProcessor' creates a queue processor but with a loop when some items 
-- can be processed and then added to the queue again. Also it allows specifying 
-- how two input streams of data can be merged.
queueProcessorLoopMerging :: (Stream a -> Stream d -> Stream e)
                             -- ^ merge two streams: the input values of type @a@
                             -- and the values of type @d@ returned by the loop
                             -> (e -> Process ())
                             -- ^ enqueue the input item and wait
                             -- while the queue is full if required
                             -- so that there were no hanging items
                             -> Process c
                             -- ^ dequeue an item for the further processing
                             -> Processor c (Either f b)
                             -- ^ process and then decide what values of type @f@
                             -- should be processed in the loop (this is a condition)
                             -> Processor f d
                             -- ^ process in the loop and then return a value
                             -- of type @d@ to the queue again (this is a loop body)
                             -> Processor a b
                             -- ^ the buffering processor
queueProcessorLoopMerging merge enqueue dequeue =
  bufferProcessorLoop
  (\bs cs ->
    consumeStream enqueue $
    merge bs cs)
  (repeatProcess dequeue)

-- | Like 'queueProcessorLoopMerging' creates a queue processor with a loop when
-- some items can be processed and then added to the queue again. Only it sequentially 
-- merges two input streams of data: one stream that come from the external source and 
-- another stream of data returned by the loop. The first stream has a priority over 
-- the second one.
queueProcessorLoopSeq :: (a -> Process ())
                         -- ^ enqueue the input item and wait
                         -- while the queue is full if required
                         -- so that there were no hanging items
                         -> Process c
                         -- ^ dequeue an item for the further processing
                         -> Processor c (Either e b)
                         -- ^ process and then decide what values of type @e@
                         -- should be processed in the loop (this is a condition)
                         -> Processor e a
                         -- ^ process in the loop and then return a value
                         -- of type @a@ to the queue again (this is a loop body)
                         -> Processor a b
                         -- ^ the buffering processor
queueProcessorLoopSeq =
  queueProcessorLoopMerging mergeStreams

-- | Like 'queueProcessorLoopMerging' creates a queue processor with a loop when
-- some items can be processed and then added to the queue again. Only it runs two 
-- simultaneous processes to enqueue the input streams of data: one stream that come 
-- from the external source and another stream of data returned by the loop.
queueProcessorLoopParallel :: (a -> Process ())
                              -- ^ enqueue the input item and wait
                              -- while the queue is full if required
                              -- so that there were no hanging items
                              -> Process c
                              -- ^ dequeue an item for the further processing
                              -> Processor c (Either e b)
                              -- ^ process and then decide what values of type @e@
                              -- should be processed in the loop (this is a condition)
                              -> Processor e a
                              -- ^ process in the loop and then return a value
                              -- of type @a@ to the queue again (this is a loop body)
                              -> Processor a b
                              -- ^ the buffering processor
queueProcessorLoopParallel enqueue dequeue =
  bufferProcessorLoop
  (\bs cs ->
    do spawnProcess $
         consumeStream enqueue bs
       spawnProcess $
         consumeStream enqueue cs)
  (repeatProcess dequeue)

-- | This is a prefetch processor that requests for one more data item from 
-- the input in advance while the latest item is not yet fully processed in 
-- the chain of streams, usually by other processors.
--
-- You can think of this as the prefetched processor could place its latest 
-- data item in some temporary space for later use, which is very useful 
-- for modeling a sequence of separate and independent work places.
prefetchProcessor :: Processor a a
prefetchProcessor = Processor prefetchStream

-- | Convert the specified signal transform to a processor.
--
-- The processor may return data with delay as the values are requested by demand.
-- Consider using the 'arrivalSignal' function to provide with the information
-- about the time points at which the signal was actually triggered.
--
-- The point is that the 'Stream' used in the 'Processor' is requested outside, 
-- while the 'Signal' is triggered inside. They are different by nature. 
-- The former is passive, while the latter is active.
--
-- Cancel the processor's process to unsubscribe from the signals provided.
signalProcessor :: (Signal a -> Signal b) -> Processor a b
signalProcessor f =
  Processor $ \xs ->
  Cons $
  do sa <- streamSignal xs
     sb <- signalStream (f sa)
     runStream sb

-- | Convert the specified processor to a signal transform. 
--
-- The processor may return data with delay as the values are requested by demand.
-- Consider using the 'arrivalSignal' function to provide with the information
-- about the time points at which the signal was actually triggered.
--
-- The point is that the 'Stream' used in the 'Processor' is requested outside, 
-- while the 'Signal' is triggered inside. They are different by nature.
-- The former is passive, while the latter is active.
--
-- Cancel the returned process to unsubscribe from the signal specified.
processorSignaling :: Processor a b -> Signal a -> Process (Signal b)
processorSignaling (Processor f) sa =
  do xs <- signalStream sa
     let ys = f xs
     streamSignal ys

-- | A processor that adds the information about the time points at which 
-- the original stream items were received by demand.
arrivalProcessor :: Processor a (Arrival a)
arrivalProcessor = Processor arrivalStream

-- | A processor that delays the input stream by one step using the specified initial value.
delayProcessor :: a -> Processor a a
delayProcessor a0 = Processor $ delayStream a0

-- | Removes one level of the computation, projecting its bound processor into the outer level.
joinProcessor :: Process (Processor a b) -> Processor a b
joinProcessor m =
  Processor $ \xs ->
  Cons $
  do Processor f <- m
     runStream $ f xs

-- | Takes the next processor from the list after the current processor fails because of cancelling the underlying process.
failoverProcessor :: [Processor a b] -> Processor a b
failoverProcessor ps =
  Processor $ \xs -> failoverStream [runProcessor p xs | p <- ps]

-- | Show the debug messages with the current simulation time.
traceProcessor :: Maybe String
                  -- ^ the request message
                  -> Maybe String
                  -- ^ the response message
                  -> Processor a b
                  -- ^ a processor
                  -> Processor a b
traceProcessor request response (Processor f) =
  Processor $ traceStream request response . f 
