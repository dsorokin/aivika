
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Stream
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The infinite stream of data in time.
--
module Simulation.Aivika.Trans.Stream
       (-- * Stream Type
        Stream(..),
        -- * Merging and Splitting Stream
        emptyStream,
        mergeStreams,
        mergeQueuedStreams,
        mergePriorityStreams,
        concatStreams,
        concatQueuedStreams,
        concatPriorityStreams,
        splitStream,
        splitStreamQueueing,
        splitStreamPrioritising,
        -- * Specifying Identifier
        streamUsingId,
        -- * Prefetching and Delaying Stream
        prefetchStream,
        delayStream,
        -- * Stream Arriving
        arrivalStream,
        -- * Memoizing, Zipping and Uzipping Stream
        memoStream,
        zipStreamSeq,
        zipStreamParallel,
        zip3StreamSeq,
        zip3StreamParallel,
        unzipStream,
        streamSeq,
        streamParallel,
        -- * Consuming and Sinking Stream
        consumeStream,
        sinkStream,
        -- * Useful Combinators
        repeatProcess,
        mapStream,
        mapStreamM,
        apStream,
        apStreamM,
        filterStream,
        filterStreamM,
        -- * Integrating with Signals
        signalStream,
        streamSignal,
        -- * Utilities
        leftStream,
        rightStream,
        replaceLeftStream,
        replaceRightStream,
        partitionEitherStream) where

import Data.Maybe
import Data.Monoid

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Cont
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Resource
import Simulation.Aivika.Trans.QueueStrategy
import Simulation.Aivika.Trans.Queue.Infinite
import Simulation.Aivika.Trans.Internal.Arrival

-- | Represents an infinite stream of data in time,
-- some kind of never-ending cons cell.
newtype Stream m a = Cons { runStream :: Process m (a, Stream m a)
                            -- ^ Run the stream.
                          }

instance Comp m => Functor (Stream m) where

  {-# INLINABLE fmap #-}
  {-# SPECIALISE fmap :: (a -> b) -> Stream IO a -> Stream IO b #-}
  fmap = mapStream

instance Comp m => Applicative (Stream m) where

  {-# INLINABLE pure #-}
  {-# SPECIALISE pure :: a -> Stream IO a #-}
  pure a = let y = Cons (return (a, y)) in y
  
  {-# INLINABLE (<*>) #-}
  {-# SPECIALISE (<*>) :: Stream IO (a -> b) -> Stream IO a -> Stream IO b #-}
  (<*>) = apStream

instance Comp m => Monoid (Stream m a) where

  {-# INLINABLE mempty #-}
  {-# SPECIALISE mempty :: Stream IO a #-}
  mempty  = emptyStream

  {-# INLINABLE mappend #-}
  {-# SPECIALISE mappend :: Stream IO a -> Stream IO a -> Stream IO a #-}
  mappend = mergeStreams

  {-# INLINABLE mconcat #-}
  {-# SPECIALISE mconcat :: [Stream IO a] -> Stream IO a #-}
  mconcat = concatStreams

-- | Create a stream that will use the specified process identifier.
-- It can be useful to refer to the underlying 'Process' computation which
-- can be passivated, interrupted, canceled and so on. See also the
-- 'processUsingId' function for more details.
streamUsingId :: Comp m => ProcessId m -> Stream m a -> Stream m a
{-# INLINABLE streamUsingId #-}
{-# SPECIALISE streamUsingId :: ProcessId IO -> Stream IO a -> Stream IO a #-}
streamUsingId pid (Cons s) =
  Cons $ processUsingId pid s

-- | Memoize the stream so that it would always return the same data
-- within the simulation run.
memoStream :: Comp m => Stream m a -> Simulation m (Stream m a)
{-# INLINABLE memoStream #-}
{-# SPECIALISE memoStream :: Stream IO a -> Simulation IO (Stream IO a) #-}
memoStream (Cons s) =
  do p <- memoProcess $
          do ~(x, xs) <- s
             xs' <- liftSimulation $ memoStream xs
             return (x, xs')
     return (Cons p)

-- | Zip two streams trying to get data sequentially.
zipStreamSeq :: Comp m => Stream m a -> Stream m b -> Stream m (a, b)
{-# INLINABLE zipStreamSeq #-}
{-# SPECIALISE zipStreamSeq :: Stream IO a -> Stream IO b -> Stream IO (a, b) #-}
zipStreamSeq (Cons sa) (Cons sb) = Cons y where
  y = do ~(x, xs) <- sa
         ~(y, ys) <- sb
         return ((x, y), zipStreamSeq xs ys)

-- | Zip two streams trying to get data as soon as possible,
-- launching the sub-processes in parallel.
zipStreamParallel :: Comp m => Stream m a -> Stream m b -> Stream m (a, b)
{-# INLINABLE zipStreamParallel #-}
{-# SPECIALISE zipStreamParallel :: Stream IO a -> Stream IO b -> Stream IO (a, b) #-}
zipStreamParallel (Cons sa) (Cons sb) = Cons y where
  y = do ~((x, xs), (y, ys)) <- zipProcessParallel sa sb
         return ((x, y), zipStreamParallel xs ys)

-- | Zip three streams trying to get data sequentially.
zip3StreamSeq :: Comp m => Stream m a -> Stream m b -> Stream m c -> Stream m (a, b, c)
{-# INLINABLE zip3StreamSeq #-}
{-# SPECIALISE zip3StreamSeq :: Stream IO a -> Stream IO b -> Stream IO c -> Stream IO (a, b, c) #-}
zip3StreamSeq (Cons sa) (Cons sb) (Cons sc) = Cons y where
  y = do ~(x, xs) <- sa
         ~(y, ys) <- sb
         ~(z, zs) <- sc
         return ((x, y, z), zip3StreamSeq xs ys zs)

-- | Zip three streams trying to get data as soon as possible,
-- launching the sub-processes in parallel.
zip3StreamParallel :: Comp m => Stream m a -> Stream m b -> Stream m c -> Stream m (a, b, c)
{-# INLINABLE zip3StreamParallel #-}
{-# SPECIALISE zip3StreamParallel :: Stream IO a -> Stream IO b -> Stream IO c -> Stream IO (a, b, c) #-}
zip3StreamParallel (Cons sa) (Cons sb) (Cons sc) = Cons y where
  y = do ~((x, xs), (y, ys), (z, zs)) <- zip3ProcessParallel sa sb sc
         return ((x, y, z), zip3StreamParallel xs ys zs)

-- | Unzip the stream.
unzipStream :: Comp m => Stream m (a, b) -> Simulation m (Stream m a, Stream m b)
{-# INLINABLE unzipStream #-}
{-# SPECIALISE unzipStream :: Stream IO (a, b) -> Simulation IO (Stream IO a, Stream IO b) #-}
unzipStream s =
  do s' <- memoStream s
     let sa = mapStream fst s'
         sb = mapStream snd s'
     return (sa, sb)

-- | To form each new portion of data for the output stream,
-- read data sequentially from the input streams.
--
-- This is a generalization of 'zipStreamSeq'.
streamSeq :: Comp m => [Stream m a] -> Stream m [a]
{-# INLINABLE streamSeq #-}
{-# SPECIALISE streamSeq :: [Stream IO a] -> Stream IO [a] #-}
streamSeq xs = Cons y where
  y = do ps <- forM xs runStream
         return (map fst ps, streamSeq $ map snd ps)

-- | To form each new portion of data for the output stream,
-- read data from the input streams in parallel.
--
-- This is a generalization of 'zipStreamParallel'.
streamParallel :: Comp m => [Stream m a] -> Stream m [a]
{-# INLINABLE streamParallel #-}
{-# SPECIALISE streamParallel :: [Stream IO a] -> Stream IO [a] #-}
streamParallel xs = Cons y where
  y = do ps <- processParallel $ map runStream xs
         return (map fst ps, streamParallel $ map snd ps)

-- | Return a stream of values generated by the specified process.
repeatProcess :: Comp m => Process m a -> Stream m a
{-# INLINABLE repeatProcess #-}
{-# SPECIALISE repeatProcess :: Process IO a -> Stream IO a #-}
repeatProcess p = Cons y where
  y = do a <- p
         return (a, repeatProcess p)

-- | Map the stream according the specified function.
mapStream :: Comp m => (a -> b) -> Stream m a -> Stream m b
{-# INLINABLE mapStream #-}
{-# SPECIALISE mapStream :: (a -> b) -> Stream IO a -> Stream IO b #-}
mapStream f (Cons s) = Cons y where
  y = do (a, xs) <- s
         return (f a, mapStream f xs)

-- | Compose the stream.
mapStreamM :: Comp m => (a -> Process m b) -> Stream m a -> Stream m b
{-# INLINABLE mapStreamM #-}
{-# SPECIALISE mapStreamM :: (a -> Process IO b) -> Stream IO a -> Stream IO b #-}
mapStreamM f (Cons s) = Cons y where
  y = do (a, xs) <- s
         b <- f a
         return (b, mapStreamM f xs)

-- | Sequential application.
apStream :: Comp m => Stream m (a -> b) -> Stream m a -> Stream m b
{-# INLINABLE apStream #-}
{-# SPECIALISE apStream :: Stream IO (a -> b) -> Stream IO a -> Stream IO b #-}
apStream (Cons sf) (Cons sa) = Cons y where
  y = do (f, sf') <- sf
         (a, sa') <- sa
         return (f a, apStream sf' sa')

-- | Sequential application.
apStreamM :: Comp m => Stream m (a -> Process m b) -> Stream m a -> Stream m b
{-# INLINABLE apStreamM #-}
{-# SPECIALISE apStreamM :: Stream IO (a -> Process IO b) -> Stream IO a -> Stream IO b #-}
apStreamM (Cons sf) (Cons sa) = Cons y where
  y = do (f, sf') <- sf
         (a, sa') <- sa
         x <- f a
         return (x, apStreamM sf' sa')

-- | Filter only those data values that satisfy to the specified predicate.
filterStream :: Comp m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINABLE filterStream #-}
{-# SPECIALISE filterStream :: (a -> Bool) -> Stream IO a -> Stream IO a #-}
filterStream p (Cons s) = Cons y where
  y = do (a, xs) <- s
         if p a
           then return (a, filterStream p xs)
           else let Cons z = filterStream p xs in z

-- | Filter only those data values that satisfy to the specified predicate.
filterStreamM :: Comp m => (a -> Process m Bool) -> Stream m a -> Stream m a
{-# INLINABLE filterStreamM #-}
{-# SPECIALISE filterStreamM :: (a -> Process IO Bool) -> Stream IO a -> Stream IO a #-}
filterStreamM p (Cons s) = Cons y where
  y = do (a, xs) <- s
         b <- p a
         if b
           then return (a, filterStreamM p xs)
           else let Cons z = filterStreamM p xs in z

-- | The stream of 'Left' values.
leftStream :: Comp m => Stream m (Either a b) -> Stream m a
{-# INLINABLE leftStream #-}
{-# SPECIALISE leftStream :: Stream IO (Either a b) -> Stream IO a #-}
leftStream (Cons s) = Cons y where
  y = do (a, xs) <- s
         case a of
           Left a  -> return (a, leftStream xs)
           Right _ -> let Cons z = leftStream xs in z

-- | The stream of 'Right' values.
rightStream :: Comp m => Stream m (Either a b) -> Stream m b
{-# INLINABLE rightStream #-}
{-# SPECIALISE rightStream :: Stream IO (Either a b) -> Stream IO b #-}
rightStream (Cons s) = Cons y where
  y = do (a, xs) <- s
         case a of
           Left _  -> let Cons z = rightStream xs in z
           Right a -> return (a, rightStream xs)

-- | Replace the 'Left' values.
replaceLeftStream :: Comp m => Stream m (Either a b) -> Stream m c -> Stream m (Either c b)
{-# INLINABLE replaceLeftStream #-}
{-# SPECIALISE replaceLeftStream :: Stream IO (Either a b) -> Stream IO c -> Stream IO (Either c b) #-}
replaceLeftStream (Cons sab) (ys0 @ ~(Cons sc)) = Cons z where
  z = do (a, xs) <- sab
         case a of
           Left _ ->
             do (b, ys) <- sc
                return (Left b, replaceLeftStream xs ys)
           Right a ->
             return (Right a, replaceLeftStream xs ys0)

-- | Replace the 'Right' values.
replaceRightStream :: Comp m => Stream m (Either a b) -> Stream m c -> Stream m (Either a c)
{-# INLINABLE replaceRightStream #-}
{-# SPECIALISE replaceRightStream :: Stream IO (Either a b) -> Stream IO c -> Stream IO (Either a c) #-}
replaceRightStream (Cons sab) (ys0 @ ~(Cons sc)) = Cons z where
  z = do (a, xs) <- sab
         case a of
           Right _ ->
             do (b, ys) <- sc
                return (Right b, replaceRightStream xs ys)
           Left a ->
             return (Left a, replaceRightStream xs ys0)

-- | Partition the stream of 'Either' values into two streams.
partitionEitherStream :: Comp m => Stream m (Either a b) -> Simulation m (Stream m a, Stream m b)
{-# INLINABLE partitionEitherStream #-}
{-# SPECIALISE partitionEitherStream :: Stream IO (Either a b) -> Simulation IO (Stream IO a, Stream IO b) #-}
partitionEitherStream s =
  do s' <- memoStream s
     return (leftStream s', rightStream s')

-- | Split the input stream into the specified number of output streams
-- after applying the 'FCFS' strategy for enqueuing the output requests.
splitStream :: Comp m => Int -> Stream m a -> Simulation m [Stream m a]
{-# INLINABLE splitStream #-}
{-# SPECIALISE splitStream :: Int -> Stream IO a -> Simulation IO [Stream IO a] #-}
splitStream = splitStreamQueueing FCFS

-- | Split the input stream into the specified number of output streams.
--
-- If you don't know what the strategy to apply, then you probably
-- need the 'FCFS' strategy, or function 'splitStream' that
-- does namely this.
splitStreamQueueing :: (Comp m, EnqueueStrategy m s)
                       => s
                       -- ^ the strategy applied for enqueuing the output requests
                       -> Int
                       -- ^ the number of output streams
                       -> Stream m a
                       -- ^ the input stream
                       -> Simulation m [Stream m a]
                       -- ^ the splitted output streams
{-# INLINABLE splitStreamQueueing #-}
{-# SPECIALISE splitStreamQueueing :: EnqueueStrategy IO s => s -> Int -> Stream IO a -> Simulation IO [Stream IO a] #-}
splitStreamQueueing s n x =
  do session <- liftParameter simulationSession
     ref <- liftComp $ newProtoRef session x
     res <- newResource s 1
     let reader =
           usingResource res $
           do p <- liftComp $ readProtoRef ref
              (a, xs) <- runStream p
              liftComp $ writeProtoRef ref xs
              return a
     return $ map (\i -> repeatProcess reader) [1..n]

-- | Split the input stream into a list of output streams
-- using the specified priorities.
splitStreamPrioritising :: (Comp m, PriorityQueueStrategy m s p)
                           => s
                           -- ^ the strategy applied for enqueuing the output requests
                           -> [Stream m p]
                           -- ^ the streams of priorities
                           -> Stream m a
                           -- ^ the input stream
                           -> Simulation m [Stream m a]
                           -- ^ the splitted output streams
{-# INLINABLE splitStreamPrioritising #-}
{-# SPECIALISE splitStreamPrioritising :: PriorityQueueStrategy IO s p => s -> [Stream IO p] -> Stream IO a -> Simulation IO [Stream IO a] #-}
splitStreamPrioritising s ps x =
  do session <- liftParameter simulationSession
     ref <- liftComp $ newProtoRef session x
     res <- newResource s 1
     let stream (Cons p) = Cons z where
           z = do (p', ps) <- p
                  a <- usingResourceWithPriority res p' $
                       do p <- liftComp $ readProtoRef ref
                          (a, xs) <- runStream p
                          liftComp $ writeProtoRef ref xs
                          return a
                  return (a, stream ps)
     return $ map stream ps

-- | Concatenate the input streams applying the 'FCFS' strategy and
-- producing one output stream.
concatStreams :: Comp m => [Stream m a] -> Stream m a
{-# INLINABLE concatStreams #-}
{-# SPECIALISE concatStreams :: [Stream IO a] -> Stream IO a #-}
concatStreams = concatQueuedStreams FCFS

-- | Concatenate the input streams producing one output stream.
--
-- If you don't know what the strategy to apply, then you probably
-- need the 'FCFS' strategy, or function 'concatStreams' that
-- does namely this.
concatQueuedStreams :: (Comp m, EnqueueStrategy m s)
                       => s
                       -- ^ the strategy applied for enqueuing the input data
                       -> [Stream m a]
                       -- ^ the input stream
                       -> Stream m a
                       -- ^ the combined output stream
{-# INLINABLE concatQueuedStreams #-}
{-# SPECIALISE concatQueuedStreams :: EnqueueStrategy IO s => s -> [Stream IO a] -> Stream IO a #-}
concatQueuedStreams s streams = Cons z where
  z = do reading <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         writing <- liftSimulation $ newResourceWithMaxCount s 1 (Just 1)
         conting <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         session <- liftParameter simulationSession
         ref <- liftComp $ newProtoRef session Nothing
         let writer p =
               do (a, xs) <- runStream p
                  requestResource writing
                  liftComp $ writeProtoRef ref (Just a)
                  releaseResource reading
                  requestResource conting
                  writer xs
             reader =
               do requestResource reading
                  Just a <- liftComp $ readProtoRef ref
                  liftComp $ writeProtoRef ref Nothing
                  releaseResource writing
                  return a
         forM_ streams $ spawnProcess CancelTogether . writer
         a <- reader
         let xs = repeatProcess (releaseResource conting >> reader)
         return (a, xs)

-- | Concatenate the input priority streams producing one output stream.
concatPriorityStreams :: (Comp m, PriorityQueueStrategy m s p)
                         => s
                         -- ^ the strategy applied for enqueuing the input data
                         -> [Stream m (p, a)]
                         -- ^ the input stream
                         -> Stream m a
                         -- ^ the combined output stream
{-# INLINABLE concatPriorityStreams #-}
{-# SPECIALISE concatPriorityStreams :: PriorityQueueStrategy IO s p => s -> [Stream IO (p, a)] -> Stream IO a #-}
concatPriorityStreams s streams = Cons z where
  z = do reading <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         writing <- liftSimulation $ newResourceWithMaxCount s 1 (Just 1)
         conting <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         session <- liftParameter simulationSession
         ref <- liftComp $ newProtoRef session Nothing
         let writer p =
               do ((priority, a), xs) <- runStream p
                  requestResourceWithPriority writing priority
                  liftComp $ writeProtoRef ref (Just a)
                  releaseResource reading
                  requestResource conting
                  writer xs
             reader =
               do requestResource reading
                  Just a <- liftComp $ readProtoRef ref
                  liftComp $ writeProtoRef ref Nothing
                  releaseResource writing
                  return a
         forM_ streams $ spawnProcess CancelTogether . writer
         a <- reader
         let xs = repeatProcess (releaseResource conting >> reader)
         return (a, xs)

-- | Merge two streams applying the 'FCFS' strategy for enqueuing the input data.
mergeStreams :: Comp m => Stream m a -> Stream m a -> Stream m a
{-# INLINABLE mergeStreams #-}
{-# SPECIALISE mergeStreams :: Stream IO a -> Stream IO a -> Stream IO a #-}
mergeStreams = mergeQueuedStreams FCFS

-- | Merge two streams.
--
-- If you don't know what the strategy to apply, then you probably
-- need the 'FCFS' strategy, or function 'mergeStreams' that
-- does namely this.
mergeQueuedStreams :: (Comp m, EnqueueStrategy m s)
                      => s
                      -- ^ the strategy applied for enqueuing the input data
                      -> Stream m a
                      -- ^ the fist input stream
                      -> Stream m a
                      -- ^ the second input stream
                      -> Stream m a
                      -- ^ the output combined stream
{-# INLINABLE mergeQueuedStreams #-}
{-# SPECIALISE mergeQueuedStreams :: EnqueueStrategy IO s => s -> Stream IO a -> Stream IO a -> Stream IO a #-}
mergeQueuedStreams s x y = concatQueuedStreams s [x, y]

-- | Merge two priority streams.
mergePriorityStreams :: (Comp m, PriorityQueueStrategy m s p)
                        => s
                        -- ^ the strategy applied for enqueuing the input data
                        -> Stream m (p, a)
                        -- ^ the fist input stream
                        -> Stream m (p, a)
                        -- ^ the second input stream
                        -> Stream m a
                        -- ^ the output combined stream
{-# INLINABLE mergePriorityStreams #-}
{-# SPECIALISE mergePriorityStreams :: PriorityQueueStrategy IO s p => s -> Stream IO (p, a) -> Stream IO (p, a) -> Stream IO a #-}
mergePriorityStreams s x y = concatPriorityStreams s [x, y]

-- | An empty stream that never returns data.
emptyStream :: Comp m => Stream m a
{-# INLINABLE emptyStream #-}
{-# SPECIALISE emptyStream :: Stream IO a #-}
emptyStream = Cons neverProcess

-- | Consume the stream. It returns a process that infinitely reads data
-- from the stream and then redirects them to the provided function.
-- It is useful for modeling the process of enqueueing data in the queue
-- from the input stream.
consumeStream :: Comp m => (a -> Process m ()) -> Stream m a -> Process m ()
{-# INLINABLE consumeStream #-}
{-# SPECIALISE consumeStream :: (a -> Process IO ()) -> Stream IO a -> Process IO () #-}
consumeStream f = p where
  p (Cons s) = do (a, xs) <- s
                  f a
                  p xs

-- | Sink the stream. It returns a process that infinitely reads data
-- from the stream. The resulting computation can be a moving force
-- to simulate the whole system of the interconnected streams and
-- processors.
sinkStream :: Comp m => Stream m a -> Process m ()
{-# INLINABLE sinkStream #-}
{-# SPECIALISE sinkStream :: Stream IO a -> Process IO () #-}
sinkStream = p where
  p (Cons s) = do (a, xs) <- s
                  p xs
  
-- | Prefetch the input stream requesting for one more data item in advance 
-- while the last received item is not yet fully processed in the chain of 
-- streams, usually by the processors.
--
-- You can think of this as the prefetched stream could place its latest 
-- data item in some temporary space for later use, which is very useful 
-- for modeling a sequence of separate and independent work places.
prefetchStream :: Comp m => Stream m a -> Stream m a
{-# INLINABLE prefetchStream #-}
{-# SPECIALISE prefetchStream :: Stream IO a -> Stream IO a #-}
prefetchStream s = Cons z where
  z = do reading <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         writing <- liftSimulation $ newResourceWithMaxCount FCFS 1 (Just 1)
         session <- liftParameter simulationSession
         ref <- liftComp $ newProtoRef session Nothing
         let writer p =
               do (a, xs) <- runStream p
                  requestResource writing
                  liftComp $ writeProtoRef ref (Just a)
                  releaseResource reading
                  writer xs
             reader =
               do requestResource reading
                  Just a <- liftComp $ readProtoRef ref
                  liftComp $ writeProtoRef ref Nothing
                  releaseResource writing
                  return a
         spawnProcess CancelTogether $ writer s
         runStream $ repeatProcess reader

-- | Return a stream of values triggered by the specified signal.
--
-- Since the time at which the values of the stream are requested for may differ from
-- the time at which the signal is triggered, it can be useful to apply the 'arrivalSignal'
-- function to add the information about the time points at which the signal was 
-- actually received.
--
-- The point is that the 'Stream' is requested outside, while the 'Signal' is triggered
-- inside. They are different by nature. The former is passive, while the latter is active.
--
-- The resulting stream may be a root of space leak as it uses an internal queue to store
-- the values received from the signal. The oldest value is dequeued each time we request
-- the stream and it is returned within the computation.
--
-- Cancel the stream's process to unsubscribe from the specified signal.
signalStream :: Comp m => Signal m a -> Process m (Stream m a)
{-# INLINABLE signalStream #-}
{-# SPECIALISE signalStream :: Signal IO a -> Process IO (Stream IO a) #-}
signalStream s =
  do q <- liftEvent newFCFSQueue
     h <- liftEvent $
          handleSignal s $ 
          enqueue q
     whenCancellingProcess $ disposeEvent h
     return $ repeatProcess $ dequeue q

-- | Return a computation of the signal that triggers values from the specified stream,
-- each time the next value of the stream is received within the underlying 'Process' 
-- computation.
--
-- Cancel the returned process to stop reading from the specified stream. 
streamSignal :: Comp m => Stream m a -> Process m (Signal m a)
{-# INLINABLE streamSignal #-}
{-# SPECIALISE streamSignal :: Stream IO a -> Process IO (Signal IO a) #-}
streamSignal z =
  do s <- liftSimulation newSignalSource
     spawnProcess CancelTogether $
       consumeStream (liftEvent . triggerSignal s) z
     return $ publishSignal s

-- | Transform a stream so that the resulting stream returns a sequence of arrivals
-- saving the information about the time points at which the original stream items 
-- were received by demand.
arrivalStream :: Comp m => Stream m a -> Stream m (Arrival a)
{-# INLINABLE arrivalStream #-}
{-# SPECIALISE arrivalStream :: Stream IO a -> Stream IO (Arrival a) #-}
arrivalStream s = Cons $ loop s Nothing where
  loop s t0 = do (a, xs) <- runStream s
                 t <- liftDynamics time
                 let b = Arrival { arrivalValue = a,
                                   arrivalTime  = t,
                                   arrivalDelay =
                                     case t0 of
                                       Nothing -> Nothing
                                       Just t0 -> Just (t - t0) }
                 return (b, Cons $ loop xs (Just t))

-- | Delay the stream by one step using the specified initial value.
delayStream :: Comp m => a -> Stream m a -> Stream m a
{-# INLINABLE delayStream #-}
{-# SPECIALISE delayStream :: a -> Stream IO a -> Stream IO a #-}
delayStream a0 s = Cons $ return (a0, s)
