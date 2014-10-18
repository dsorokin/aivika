
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Processor
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The processor of simulation data.
--
module Simulation.Aivika.Trans.Processor
       (-- * Processor Type
        Processor(..),
        -- * Processor Primitives
        emptyProcessor,
        arrProcessor,
        accumProcessor,
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
        -- * Integrating with Signals
        signalProcessor,
        processorSignaling) where

import qualified Control.Category as C
import Control.Arrow

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Cont
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Stream
import Simulation.Aivika.Trans.QueueStrategy
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Internal.Arrival

-- | Represents a processor of simulation data.
newtype Processor m a b =
  Processor { runProcessor :: Stream m a -> Stream m b
              -- ^ Run the processor.
            }

instance C.Category (Processor m) where

  {-# INLINE id #-}
  id  = Processor id

  {-# INLINE (.) #-}
  Processor x . Processor y = Processor (x . y)

-- The implementation is based on article
-- A New Notation for Arrows by Ross Paterson,
-- although my streams are different and they
-- already depend on the Process monad,
-- while the pure streams were considered in the
-- mentioned article.
  
instance Comp m => Arrow (Processor m) where

  {-# INLINABLE arr #-}
  {-# SPECIALISE arr :: (b -> c) -> Processor IO b c #-}
  arr = Processor . mapStream

  {-# INLINABLE first #-}
  {-# SPECIALISE first :: Processor IO b c -> Processor IO (b, d) (c, d) #-}
  first (Processor f) =
    Processor $ \xys ->
    Cons $
    do (xs, ys) <- liftSimulation $ unzipStream xys
       runStream $ zipStreamSeq (f xs) ys

  {-# INLINABLE second #-}
  {-# SPECIALISE second :: Processor IO b c -> Processor IO (d, b) (d, c) #-}
  second (Processor f) =
    Processor $ \xys ->
    Cons $
    do (xs, ys) <- liftSimulation $ unzipStream xys
       runStream $ zipStreamSeq xs (f ys)

  {-# INLINABLE (***) #-}
  {-# SPECIALISE (***) :: Processor IO b c -> Processor IO b' c' -> Processor IO (b, b') (c, c') #-}
  Processor f *** Processor g =
    Processor $ \xys ->
    Cons $
    do (xs, ys) <- liftSimulation $ unzipStream xys
       runStream $ zipStreamSeq (f xs) (g ys)

  {-# INLINABLE (&&&) #-}
  {-# SPECIALISE (&&&) :: Processor IO b c -> Processor IO b c' -> Processor IO b (c, c') #-}
  Processor f &&& Processor g =
    Processor $ \xs -> zipStreamSeq (f xs) (g xs)

instance Comp m => ArrowChoice (Processor m) where

  {-# INLINABLE left #-}
  {-# SPECIALISE left :: Processor IO b c -> Processor IO (Either b d) (Either c d) #-}
  left (Processor f) =
    Processor $ \xs ->
    Cons $
    do ys <- liftSimulation $ memoStream xs
       runStream $ replaceLeftStream ys (f $ leftStream ys)

  {-# INLINABLE right #-}
  {-# SPECIALISE right :: Processor IO b c -> Processor IO (Either d b) (Either d c) #-}
  right (Processor f) =
    Processor $ \xs ->
    Cons $
    do ys <- liftSimulation $ memoStream xs
       runStream $ replaceRightStream ys (f $ rightStream ys)

instance Comp m => ArrowZero (Processor m) where

  {-# INLINABLE zeroArrow #-}
  {-# SPECIALISE zeroArrow :: Processor IO b c #-}
  zeroArrow = emptyProcessor

instance Comp m => ArrowPlus (Processor m) where

  {-# INLINABLE (<+>) #-}
  {-# SPECIALISE (<+>) :: Processor IO b c -> Processor IO b c -> Processor IO b c #-}
  (Processor f) <+> (Processor g) =
    Processor $ \xs ->
    Cons $
    do [xs1, xs2] <- liftSimulation $ splitStream 2 xs
       runStream $ mergeStreams (f xs1) (g xs2)

-- | A processor that never finishes its work producing an 'emptyStream'.
emptyProcessor :: Comp m => Processor m a b
{-# INLINABLE emptyProcessor #-}
{-# SPECIALISE emptyProcessor :: Processor IO a b #-}
emptyProcessor = Processor $ const emptyStream

-- | Create a simple processor by the specified handling function
-- that runs the discontinuous process for each input value to get the output.
arrProcessor :: Comp m => (a -> Process m b) -> Processor m a b
{-# INLINABLE arrProcessor #-}
{-# SPECIALISE arrProcessor :: (a -> Process IO b) -> Processor IO a b #-}
arrProcessor = Processor . mapStreamM

-- | Accumulator that outputs a value determined by the supplied function.
accumProcessor :: Comp m => (acc -> a -> Process m (acc, b)) -> acc -> Processor m a b
{-# INLINABLE accumProcessor #-}
{-# SPECIALISE accumProcessor :: (acc -> a -> Process IO (acc, b)) -> acc -> Processor IO a b #-}
accumProcessor f acc =
  Processor $ \xs -> Cons $ loop xs acc where
    loop xs acc =
      do (a, xs') <- runStream xs
         (acc', b) <- f acc a
         return (b, Cons $ loop xs' acc') 

-- | Create a processor that will use the specified process identifier.
-- It can be useful to refer to the underlying 'Process' computation which
-- can be passivated, interrupted, canceled and so on. See also the
-- 'processUsingId' function for more details.
processorUsingId :: Comp m => ProcessId m -> Processor m a b -> Processor m a b
{-# INLINABLE processorUsingId #-}
{-# SPECIALISE processorUsingId :: ProcessId IO -> Processor IO a b -> Processor IO a b #-}
processorUsingId pid (Processor f) =
  Processor $ Cons . processUsingId pid . runStream . f

-- | Launches the specified processors in parallel consuming the same input
-- stream and producing a combined output stream.
--
-- If you don't know what the enqueue strategies to apply, then
-- you will probably need 'FCFS' for the both parameters, or
-- function 'processorParallel' that does namely this.
processorQueuedParallel :: (Comp m,
                            EnqueueStrategy m si,
                            EnqueueStrategy m so)
                           => si
                           -- ^ the strategy applied for enqueuing the input data
                           -> so
                           -- ^ the strategy applied for enqueuing the output data
                           -> [Processor m a b]
                           -- ^ the processors to parallelize
                           -> Processor m a b
                           -- ^ the parallelized processor
{-# INLINABLE processorQueuedParallel #-}
{-# SPECIALISE processorQueuedParallel :: (EnqueueStrategy IO si, EnqueueStrategy IO so) => si -> so -> [Processor IO a b] -> Processor IO a b #-}
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
processorPrioritisingOutputParallel :: (Comp m,
                                        EnqueueStrategy m si,
                                        PriorityQueueStrategy m so po)
                                       => si
                                       -- ^ the strategy applied for enqueuing the input data
                                       -> so
                                       -- ^ the strategy applied for enqueuing the output data
                                       -> [Processor m a (po, b)]
                                       -- ^ the processors to parallelize
                                       -> Processor m a b
                                       -- ^ the parallelized processor
{-# INLINABLE processorPrioritisingOutputParallel #-}
{-# SPECIALISE processorPrioritisingOutputParallel :: (EnqueueStrategy IO si, PriorityQueueStrategy IO so po) => si -> so -> [Processor IO a (po, b)] -> Processor IO a b #-}
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
processorPrioritisingInputParallel :: (Comp m,
                                       PriorityQueueStrategy m si pi,
                                       EnqueueStrategy m so)
                                      => si
                                      -- ^ the strategy applied for enqueuing the input data
                                      -> so
                                      -- ^ the strategy applied for enqueuing the output data
                                      -> [(Stream m pi, Processor m a b)]
                                      -- ^ the streams of input priorities and the processors
                                      -- to parallelize
                                      -> Processor m a b
                                      -- ^ the parallelized processor
{-# INLINABLE processorPrioritisingInputParallel #-}
{-# SPECIALISE processorPrioritisingInputParallel :: (PriorityQueueStrategy IO si pi, EnqueueStrategy IO so) => si -> so -> [(Stream IO pi, Processor IO a b)] -> Processor IO a b #-}
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
processorPrioritisingInputOutputParallel :: (Comp m,
                                             PriorityQueueStrategy m si pi,
                                             PriorityQueueStrategy m so po)
                                            => si
                                            -- ^ the strategy applied for enqueuing the input data
                                            -> so
                                            -- ^ the strategy applied for enqueuing the output data
                                            -> [(Stream m pi, Processor m a (po, b))]
                                            -- ^ the streams of input priorities and the processors
                                            -- to parallelize
                                            -> Processor m a b
                                            -- ^ the parallelized processor
{-# INLINABLE processorPrioritisingInputOutputParallel #-}
{-# SPECIALISE processorPrioritisingInputOutputParallel :: (PriorityQueueStrategy IO si pi, PriorityQueueStrategy IO so po) => si -> so -> [(Stream IO pi, Processor IO a (po, b))] -> Processor IO a b #-}
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
processorParallel :: Comp m => [Processor m a b] -> Processor m a b
{-# INLINABLE processorParallel #-}
{-# SPECIALISE processorParallel :: [Processor IO a b] -> Processor IO a b #-}
processorParallel = processorQueuedParallel FCFS FCFS

-- | Launches the processors sequentially using the 'prefetchProcessor' between them
-- to model an autonomous work of each of the processors specified.
processorSeq :: Comp m => [Processor m a a] -> Processor m a a
{-# INLINABLE processorSeq #-}
{-# SPECIALISE processorSeq :: [Processor IO a a] -> Processor IO a a #-}
processorSeq []  = emptyProcessor
processorSeq [p] = p
processorSeq (p : ps) = p >>> prefetchProcessor >>> processorSeq ps

-- | Create a buffer processor, where the process from the first argument
-- consumes the input stream but the stream passed in as the second argument
-- and produced usually by some other process is returned as an output.
-- This kind of processor is very useful for modeling the queues.
bufferProcessor :: Comp m
                   => (Stream m a -> Process m ())
                   -- ^ a separate process to consume the input 
                   -> Stream m b
                   -- ^ the resulting stream of data
                   -> Processor m a b
{-# INLINABLE bufferProcessor #-}
{-# SPECIALISE bufferProcessor :: (Stream IO a -> Process IO ()) -> Stream IO b -> Processor IO a b #-}
bufferProcessor consume output =
  Processor $ \xs ->
  Cons $
  do spawnProcess CancelTogether (consume xs)
     runStream output

-- | Like 'bufferProcessor' but allows creating a loop when some items
-- can be processed repeatedly. It is very useful for modeling the processors 
-- with queues and loop-backs.
bufferProcessorLoop :: Comp m
                       => (Stream m a -> Stream m c -> Process m ())
                       -- ^ consume two streams: the input values of type @a@
                       -- and the values of type @c@ returned by the loop
                       -> Stream m d
                       -- ^ the stream of data that may become results
                       -> Processor m d (Either e b)
                       -- ^ process and then decide what values of type @e@
                       -- should be processed in the loop (this is a condition)
                       -> Processor m e c
                       -- ^ process in the loop and then return a value
                       -- of type @c@ to the input again (this is a loop body)
                       -> Processor m a b
{-# INLINABLE bufferProcessorLoop #-}
{-# SPECIALISE bufferProcessorLoop :: (Stream IO a -> Stream IO c -> Process IO ()) -> Stream IO d -> Processor IO d (Either e b) -> Processor IO e c -> Processor IO a b #-}
bufferProcessorLoop consume preoutput cond body =
  Processor $ \xs ->
  Cons $
  do (reverted, output) <-
       liftSimulation $
       partitionEitherStream $
       runProcessor cond preoutput
     spawnProcess CancelTogether 
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
queueProcessor :: Comp m =>
                  (a -> Process m ())
                  -- ^ enqueue the input item and wait
                  -- while the queue is full if required
                  -- so that there were no hanging items
                  -> Process m b
                  -- ^ dequeue an output item
                  -> Processor m a b
                  -- ^ the buffering processor
{-# INLINABLE queueProcessor #-}
{-# SPECIALISE queueProcessor :: (a -> Process IO ()) -> Process IO b -> Processor IO a b #-}
queueProcessor enqueue dequeue =
  bufferProcessor
  (consumeStream enqueue)
  (repeatProcess dequeue)

-- | Like 'queueProcessor' creates a queue processor but with a loop when some items 
-- can be processed and then added to the queue again. Also it allows specifying 
-- how two input streams of data can be merged.
queueProcessorLoopMerging :: Comp m
                             => (Stream m a -> Stream m d -> Stream m e)
                             -- ^ merge two streams: the input values of type @a@
                             -- and the values of type @d@ returned by the loop
                             -> (e -> Process m ())
                             -- ^ enqueue the input item and wait
                             -- while the queue is full if required
                             -- so that there were no hanging items
                             -> Process m c
                             -- ^ dequeue an item for the further processing
                             -> Processor m c (Either f b)
                             -- ^ process and then decide what values of type @f@
                             -- should be processed in the loop (this is a condition)
                             -> Processor m f d
                             -- ^ process in the loop and then return a value
                             -- of type @d@ to the queue again (this is a loop body)
                             -> Processor m a b
                             -- ^ the buffering processor
{-# INLINABLE queueProcessorLoopMerging #-}
{-# SPECIALISE queueProcessorLoopMerging :: (Stream IO a -> Stream IO d -> Stream IO e) -> (e -> Process IO ()) -> Process IO c -> Processor IO c (Either f b) -> Processor IO f d -> Processor IO a b #-}
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
queueProcessorLoopSeq :: Comp m
                         => (a -> Process m ())
                         -- ^ enqueue the input item and wait
                         -- while the queue is full if required
                         -- so that there were no hanging items
                         -> Process m c
                         -- ^ dequeue an item for the further processing
                         -> Processor m c (Either e b)
                         -- ^ process and then decide what values of type @e@
                         -- should be processed in the loop (this is a condition)
                         -> Processor m e a
                         -- ^ process in the loop and then return a value
                         -- of type @a@ to the queue again (this is a loop body)
                         -> Processor m a b
                         -- ^ the buffering processor
{-# INLINABLE queueProcessorLoopSeq #-}
{-# SPECIALISE queueProcessorLoopSeq :: (a -> Process IO ()) -> Process IO c -> Processor IO c (Either e b) -> Processor IO e a -> Processor IO a b #-}
queueProcessorLoopSeq =
  queueProcessorLoopMerging mergeStreams

-- | Like 'queueProcessorLoopMerging' creates a queue processor with a loop when
-- some items can be processed and then added to the queue again. Only it runs two 
-- simultaneous processes to enqueue the input streams of data: one stream that come 
-- from the external source and another stream of data returned by the loop.
queueProcessorLoopParallel :: Comp m
                              => (a -> Process m ())
                              -- ^ enqueue the input item and wait
                              -- while the queue is full if required
                              -- so that there were no hanging items
                              -> Process m c
                              -- ^ dequeue an item for the further processing
                              -> Processor m c (Either e b)
                              -- ^ process and then decide what values of type @e@
                              -- should be processed in the loop (this is a condition)
                              -> Processor m e a
                              -- ^ process in the loop and then return a value
                              -- of type @a@ to the queue again (this is a loop body)
                              -> Processor m a b
                              -- ^ the buffering processor
{-# INLINABLE queueProcessorLoopParallel #-}
{-# SPECIALISE queueProcessorLoopParallel :: (a -> Process IO ()) -> Process IO c -> Processor IO c (Either e b) -> Processor IO e a -> Processor IO a b #-}
queueProcessorLoopParallel enqueue dequeue =
  bufferProcessorLoop
  (\bs cs ->
    do spawnProcess CancelTogether $
         consumeStream enqueue bs
       spawnProcess CancelTogether $
         consumeStream enqueue cs)
  (repeatProcess dequeue)

-- | This is a prefetch processor that requests for one more data item from 
-- the input in advance while the latest item is not yet fully processed in 
-- the chain of streams, usually by other processors.
--
-- You can think of this as the prefetched processor could place its latest 
-- data item in some temporary space for later use, which is very useful 
-- for modeling a sequence of separate and independent work places.
prefetchProcessor :: Comp m => Processor m a a
{-# INLINABLE prefetchProcessor #-}
{-# SPECIALISE prefetchProcessor :: Processor IO a a #-}
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
signalProcessor :: Comp m => (Signal m a -> Signal m b) -> Processor m a b
{-# INLINABLE signalProcessor #-}
{-# SPECIALISE signalProcessor :: (Signal IO a -> Signal IO b) -> Processor IO a b #-}
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
processorSignaling :: Comp m => Processor m a b -> Signal m a -> Process m (Signal m b)
{-# INLINABLE processorSignaling #-}
{-# SPECIALISE processorSignaling :: Processor IO a b -> Signal IO a -> Process IO (Signal IO b) #-}
processorSignaling (Processor f) sa =
  do xs <- signalStream sa
     let ys = f xs
     streamSignal ys

-- | A processor that adds the information about the time points at which 
-- the original stream items were received by demand.
arrivalProcessor :: Comp m => Processor m a (Arrival a)
{-# INLINABLE arrivalProcessor #-}
{-# SPECIALISE arrivalProcessor :: Processor IO a (Arrival a) #-}
arrivalProcessor = Processor arrivalStream

-- | A processor that delays the input stream by one step using the specified initial value.
delayProcessor :: Comp m => a -> Processor m a a
{-# INLINABLE delayProcessor #-}
{-# SPECIALISE delayProcessor :: a -> Processor IO a a #-}
delayProcessor a0 = Processor $ delayStream a0
