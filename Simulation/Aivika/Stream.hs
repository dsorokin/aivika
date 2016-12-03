
-- |
-- Module     : Simulation.Aivika.Stream
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The infinite stream of data in time.
--
module Simulation.Aivika.Stream
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
        splitStreamFiltering,
        splitStreamFilteringQueueing,
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
        accumStream,
        apStream,
        apStreamM,
        filterStream,
        filterStreamM,
        takeStream,
        takeStreamWhile,
        takeStreamWhileM,
        dropStream,
        dropStreamWhile,
        dropStreamWhileM,
        singletonStream,
        joinStream,
        -- * Failover
        failoverStream,
        -- * Integrating with Signals
        signalStream,
        streamSignal,
        -- * Utilities
        leftStream,
        rightStream,
        replaceLeftStream,
        replaceRightStream,
        partitionEitherStream,
        -- * Assemblying Streams
        cloneStream,
        firstArrivalStream,
        lastArrivalStream,
        assembleAccumStream,
        -- * Debugging
        traceStream) where

import Data.IORef
import Data.Maybe
import Data.Monoid

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Composite
import Simulation.Aivika.Cont
import Simulation.Aivika.Process
import Simulation.Aivika.Signal
import Simulation.Aivika.Resource.Base
import Simulation.Aivika.QueueStrategy
import Simulation.Aivika.Queue.Infinite.Base
import Simulation.Aivika.Internal.Arrival

-- | Represents an infinite stream of data in time,
-- some kind of the cons cell.
newtype Stream a = Cons { runStream :: Process (a, Stream a)
                          -- ^ Run the stream.
                        }

instance Functor Stream where
  
  fmap = mapStream

instance Applicative Stream where

  pure a = let y = Cons (return (a, y)) in y
  
  (<*>) = apStream

instance Alternative Stream where

  empty = emptyStream

  (<|>) = mergeStreams

instance Monoid (Stream a) where

  mempty  = emptyStream

  mappend = mergeStreams

  mconcat = concatStreams

-- | Create a stream that will use the specified process identifier.
-- It can be useful to refer to the underlying 'Process' computation which
-- can be passivated, interrupted, canceled and so on. See also the
-- 'processUsingId' function for more details.
streamUsingId :: ProcessId -> Stream a -> Stream a
streamUsingId pid (Cons s) =
  Cons $ processUsingId pid s

-- | Memoize the stream so that it would always return the same data
-- within the simulation run.
memoStream :: Stream a -> Simulation (Stream a)
memoStream (Cons s) =
  do p <- memoProcess $
          do ~(x, xs) <- s
             xs' <- liftSimulation $ memoStream xs
             return (x, xs')
     return (Cons p)

-- | Zip two streams trying to get data sequentially.
zipStreamSeq :: Stream a -> Stream b -> Stream (a, b)
zipStreamSeq (Cons sa) (Cons sb) = Cons y where
  y = do ~(x, xs) <- sa
         ~(y, ys) <- sb
         return ((x, y), zipStreamSeq xs ys)

-- | Zip two streams trying to get data as soon as possible,
-- launching the sub-processes in parallel.
zipStreamParallel :: Stream a -> Stream b -> Stream (a, b)
zipStreamParallel (Cons sa) (Cons sb) = Cons y where
  y = do ~((x, xs), (y, ys)) <- zipProcessParallel sa sb
         return ((x, y), zipStreamParallel xs ys)

-- | Zip three streams trying to get data sequentially.
zip3StreamSeq :: Stream a -> Stream b -> Stream c -> Stream (a, b, c)
zip3StreamSeq (Cons sa) (Cons sb) (Cons sc) = Cons y where
  y = do ~(x, xs) <- sa
         ~(y, ys) <- sb
         ~(z, zs) <- sc
         return ((x, y, z), zip3StreamSeq xs ys zs)

-- | Zip three streams trying to get data as soon as possible,
-- launching the sub-processes in parallel.
zip3StreamParallel :: Stream a -> Stream b -> Stream c -> Stream (a, b, c)
zip3StreamParallel (Cons sa) (Cons sb) (Cons sc) = Cons y where
  y = do ~((x, xs), (y, ys), (z, zs)) <- zip3ProcessParallel sa sb sc
         return ((x, y, z), zip3StreamParallel xs ys zs)

-- | Unzip the stream.
unzipStream :: Stream (a, b) -> Simulation (Stream a, Stream b)
unzipStream s =
  do s' <- memoStream s
     let sa = mapStream fst s'
         sb = mapStream snd s'
     return (sa, sb)

-- | To form each new portion of data for the output stream,
-- read data sequentially from the input streams.
--
-- This is a generalization of 'zipStreamSeq'.
streamSeq :: [Stream a] -> Stream [a]
streamSeq xs = Cons y where
  y = do ps <- forM xs runStream
         return (map fst ps, streamSeq $ map snd ps)

-- | To form each new portion of data for the output stream,
-- read data from the input streams in parallel.
--
-- This is a generalization of 'zipStreamParallel'.
streamParallel :: [Stream a] -> Stream [a]
streamParallel xs = Cons y where
  y = do ps <- processParallel $ map runStream xs
         return (map fst ps, streamParallel $ map snd ps)

-- | Return a stream of values generated by the specified process.
repeatProcess :: Process a -> Stream a
repeatProcess p = Cons y where
  y = do a <- p
         return (a, repeatProcess p)

-- | Map the stream according the specified function.
mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f (Cons s) = Cons y where
  y = do (a, xs) <- s
         return (f a, mapStream f xs)

-- | Compose the stream.
mapStreamM :: (a -> Process b) -> Stream a -> Stream b
mapStreamM f (Cons s) = Cons y where
  y = do (a, xs) <- s
         b <- f a
         return (b, mapStreamM f xs)

-- | Accumulator that outputs a value determined by the supplied function.
accumStream :: (acc -> a -> Process (acc, b)) -> acc -> Stream a -> Stream b
accumStream f acc xs = Cons $ loop xs acc where
  loop (Cons s) acc =
    do (a, xs) <- s
       (acc', b) <- f acc a
       return (b, Cons $ loop xs acc') 

-- | Sequential application.
apStream :: Stream (a -> b) -> Stream a -> Stream b
apStream (Cons sf) (Cons sa) = Cons y where
  y = do (f, sf') <- sf
         (a, sa') <- sa
         return (f a, apStream sf' sa')

-- | Sequential application.
apStreamM :: Stream (a -> Process b) -> Stream a -> Stream b
apStreamM (Cons sf) (Cons sa) = Cons y where
  y = do (f, sf') <- sf
         (a, sa') <- sa
         x <- f a
         return (x, apStreamM sf' sa')

-- | Filter only those data values that satisfy to the specified predicate.
filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream p (Cons s) = Cons y where
  y = do (a, xs) <- s
         if p a
           then return (a, filterStream p xs)
           else let Cons z = filterStream p xs in z

-- | Filter only those data values that satisfy to the specified predicate.
filterStreamM :: (a -> Process Bool) -> Stream a -> Stream a
filterStreamM p (Cons s) = Cons y where
  y = do (a, xs) <- s
         b <- p a
         if b
           then return (a, filterStreamM p xs)
           else let Cons z = filterStreamM p xs in z

-- | The stream of 'Left' values.
leftStream :: Stream (Either a b) -> Stream a
leftStream (Cons s) = Cons y where
  y = do (a, xs) <- s
         case a of
           Left a  -> return (a, leftStream xs)
           Right _ -> let Cons z = leftStream xs in z

-- | The stream of 'Right' values.
rightStream :: Stream (Either a b) -> Stream b
rightStream (Cons s) = Cons y where
  y = do (a, xs) <- s
         case a of
           Left _  -> let Cons z = rightStream xs in z
           Right a -> return (a, rightStream xs)

-- | Replace the 'Left' values.
replaceLeftStream :: Stream (Either a b) -> Stream c -> Stream (Either c b)
replaceLeftStream (Cons sab) (ys0 @ ~(Cons sc)) = Cons z where
  z = do (a, xs) <- sab
         case a of
           Left _ ->
             do (b, ys) <- sc
                return (Left b, replaceLeftStream xs ys)
           Right a ->
             return (Right a, replaceLeftStream xs ys0)

-- | Replace the 'Right' values.
replaceRightStream :: Stream (Either a b) -> Stream c -> Stream (Either a c)
replaceRightStream (Cons sab) (ys0 @ ~(Cons sc)) = Cons z where
  z = do (a, xs) <- sab
         case a of
           Right _ ->
             do (b, ys) <- sc
                return (Right b, replaceRightStream xs ys)
           Left a ->
             return (Left a, replaceRightStream xs ys0)

-- | Partition the stream of 'Either' values into two streams.
partitionEitherStream :: Stream (Either a b) -> Simulation (Stream a, Stream b)
partitionEitherStream s =
  do s' <- memoStream s
     return (leftStream s', rightStream s')

-- | Split the input stream into the specified number of output streams
-- after applying the 'FCFS' strategy for enqueuing the output requests.
splitStream :: Int -> Stream a -> Simulation [Stream a]
splitStream = splitStreamQueueing FCFS

-- | Split the input stream into the specified number of output streams.
--
-- If you don't know what the strategy to apply, then you probably
-- need the 'FCFS' strategy, or function 'splitStream' that
-- does namely this.
splitStreamQueueing :: EnqueueStrategy s
                       => s
                       -- ^ the strategy applied for enqueuing the output requests
                       -> Int
                       -- ^ the number of output streams
                       -> Stream a
                       -- ^ the input stream
                       -> Simulation [Stream a]
                       -- ^ the splitted output streams
splitStreamQueueing s n x =
  do ref <- liftIO $ newIORef x
     res <- newResource s 1
     let reader =
           usingResource res $
           do p <- liftIO $ readIORef ref
              (a, xs) <- runStream p
              liftIO $ writeIORef ref xs
              return a
     return $ map (\i -> repeatProcess reader) [1..n]

-- | Split the input stream into a list of output streams
-- using the specified priorities.
splitStreamPrioritising :: PriorityQueueStrategy s p
                           => s
                           -- ^ the strategy applied for enqueuing the output requests
                           -> [Stream p]
                           -- ^ the streams of priorities
                           -> Stream a
                           -- ^ the input stream
                           -> Simulation [Stream a]
                           -- ^ the splitted output streams
splitStreamPrioritising s ps x =
  do ref <- liftIO $ newIORef x
     res <- newResource s 1
     let stream (Cons p) = Cons z where
           z = do (p', ps) <- p
                  a <- usingResourceWithPriority res p' $
                       do p <- liftIO $ readIORef ref
                          (a, xs) <- runStream p
                          liftIO $ writeIORef ref xs
                          return a
                  return (a, stream ps)
     return $ map stream ps

-- | Split the input stream into the specified number of output streams
-- after filtering and applying the 'FCFS' strategy for enqueuing the output requests.
splitStreamFiltering :: [a -> Event Bool] -> Stream a -> Simulation [Stream a]
splitStreamFiltering = splitStreamFilteringQueueing FCFS

-- | Split the input stream into the specified number of output streams after filtering.
--
-- If you don't know what the strategy to apply, then you probably
-- need the 'FCFS' strategy, or function 'splitStreamFiltering' that
-- does namely this.
splitStreamFilteringQueueing :: EnqueueStrategy s
                                => s
                                -- ^ the strategy applied for enqueuing the output requests
                                -> [a -> Event Bool]
                                -- ^ the filters for output streams
                                -> Stream a
                                -- ^ the input stream
                                -> Simulation [Stream a]
                                -- ^ the splitted output streams
splitStreamFilteringQueueing s preds x =
  do ref <- liftIO $ newIORef x
     res <- newResource s 1
     let reader pred =
           do a <-
                usingResource res $
                do p <- liftIO $ readIORef ref
                   (a, xs) <- runStream p
                   liftEvent $
                     do f <- pred a
                        if f
                          then do liftIO $ writeIORef ref xs
                                  return $ Just a
                          else do liftIO $ writeIORef ref $ Cons (return (a, xs))
                                  return Nothing
              case a of
                Just a  -> return a
                Nothing -> reader pred
     return $ map (repeatProcess . reader) preds

-- | Concatenate the input streams applying the 'FCFS' strategy and
-- producing one output stream.
concatStreams :: [Stream a] -> Stream a
concatStreams = concatQueuedStreams FCFS

-- | Concatenate the input streams producing one output stream.
--
-- If you don't know what the strategy to apply, then you probably
-- need the 'FCFS' strategy, or function 'concatStreams' that
-- does namely this.
concatQueuedStreams :: EnqueueStrategy s
                       => s
                       -- ^ the strategy applied for enqueuing the input data
                       -> [Stream a]
                       -- ^ the input stream
                       -> Stream a
                       -- ^ the combined output stream
concatQueuedStreams s streams = Cons z where
  z = do reading <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         writing <- liftSimulation $ newResourceWithMaxCount s 1 (Just 1)
         conting <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         ref <- liftIO $ newIORef Nothing
         let writer p =
               do (a, xs) <- runStream p
                  requestResource writing
                  liftIO $ writeIORef ref (Just a)
                  releaseResource reading
                  requestResource conting
                  writer xs
             reader =
               do requestResource reading
                  Just a <- liftIO $ readIORef ref
                  liftIO $ writeIORef ref Nothing
                  releaseResource writing
                  return a
         forM_ streams $ spawnProcess . writer
         a <- reader
         let xs = repeatProcess (releaseResource conting >> reader)
         return (a, xs)

-- | Concatenate the input priority streams producing one output stream.
concatPriorityStreams :: PriorityQueueStrategy s p
                         => s
                         -- ^ the strategy applied for enqueuing the input data
                         -> [Stream (p, a)]
                         -- ^ the input stream
                         -> Stream a
                         -- ^ the combined output stream
concatPriorityStreams s streams = Cons z where
  z = do reading <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         writing <- liftSimulation $ newResourceWithMaxCount s 1 (Just 1)
         conting <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         ref <- liftIO $ newIORef Nothing
         let writer p =
               do ((priority, a), xs) <- runStream p
                  requestResourceWithPriority writing priority
                  liftIO $ writeIORef ref (Just a)
                  releaseResource reading
                  requestResource conting
                  writer xs
             reader =
               do requestResource reading
                  Just a <- liftIO $ readIORef ref
                  liftIO $ writeIORef ref Nothing
                  releaseResource writing
                  return a
         forM_ streams $ spawnProcess . writer
         a <- reader
         let xs = repeatProcess (releaseResource conting >> reader)
         return (a, xs)

-- | Merge two streams applying the 'FCFS' strategy for enqueuing the input data.
mergeStreams :: Stream a -> Stream a -> Stream a
mergeStreams = mergeQueuedStreams FCFS

-- | Merge two streams.
--
-- If you don't know what the strategy to apply, then you probably
-- need the 'FCFS' strategy, or function 'mergeStreams' that
-- does namely this.
mergeQueuedStreams :: EnqueueStrategy s
                      => s
                      -- ^ the strategy applied for enqueuing the input data
                      -> Stream a
                      -- ^ the fist input stream
                      -> Stream a
                      -- ^ the second input stream
                      -> Stream a
                      -- ^ the output combined stream
mergeQueuedStreams s x y = concatQueuedStreams s [x, y]

-- | Merge two priority streams.
mergePriorityStreams :: PriorityQueueStrategy s p
                        => s
                        -- ^ the strategy applied for enqueuing the input data
                        -> Stream (p, a)
                        -- ^ the fist input stream
                        -> Stream (p, a)
                        -- ^ the second input stream
                        -> Stream a
                        -- ^ the output combined stream
mergePriorityStreams s x y = concatPriorityStreams s [x, y]

-- | An empty stream that never returns data.
emptyStream :: Stream a
emptyStream = Cons neverProcess

-- | Consume the stream. It returns a process that infinitely reads data
-- from the stream and then redirects them to the provided function.
-- It is useful for modeling the process of enqueueing data in the queue
-- from the input stream.
consumeStream :: (a -> Process ()) -> Stream a -> Process ()
consumeStream f = p where
  p (Cons s) = do (a, xs) <- s
                  f a
                  p xs

-- | Sink the stream. It returns a process that infinitely reads data
-- from the stream. The resulting computation can be a moving force
-- to simulate the whole system of the interconnected streams and
-- processors.
sinkStream :: Stream a -> Process ()
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
prefetchStream :: Stream a -> Stream a
prefetchStream s = Cons z where
  z = do reading <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         writing <- liftSimulation $ newResourceWithMaxCount FCFS 1 (Just 1)
         ref <- liftIO $ newIORef Nothing
         let writer p =
               do (a, xs) <- runStream p
                  requestResource writing
                  liftIO $ writeIORef ref (Just a)
                  releaseResource reading
                  writer xs
             reader =
               do requestResource reading
                  Just a <- liftIO $ readIORef ref
                  liftIO $ writeIORef ref Nothing
                  releaseResource writing
                  return a
         spawnProcess $ writer s
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
signalStream :: Signal a -> Composite (Stream a)
signalStream s =
  do q <- liftSimulation newFCFSQueue
     h <- liftEvent $
          handleSignal s $ enqueue q
     disposableComposite h
     return $ repeatProcess $ dequeue q

-- | Return a computation of the disposable signal that triggers values from the specified stream,
-- each time the next value of the stream is received within the underlying 'Process' 
-- computation.
streamSignal :: Stream a -> Composite (Signal a)
streamSignal z =
  do s <- liftSimulation newSignalSource
     pid <- liftSimulation newProcessId
     liftEvent $
       runProcessUsingId pid $
       consumeStream (liftEvent . triggerSignal s) z
     disposableComposite $
       DisposableEvent $
       cancelProcessWithId pid
     return $ publishSignal s
  
-- | Transform a stream so that the resulting stream returns a sequence of arrivals
-- saving the information about the time points at which the original stream items 
-- were received by demand.
arrivalStream :: Stream a -> Stream (Arrival a)
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
delayStream :: a -> Stream a -> Stream a
delayStream a0 s = Cons $ return (a0, s)

-- | Return a stream consisting of exactly one element and inifinite tail.
singletonStream :: a -> Stream a
singletonStream a = Cons $ return (a, emptyStream)

-- | Removes one level of the computation, projecting its bound stream into the outer level.
joinStream :: Process (Stream a) -> Stream a
joinStream m = Cons $ m >>= runStream

-- | Takes the next stream from the list after the current stream fails because of cancelling the underlying process.
failoverStream :: [Stream a] -> Stream a
failoverStream ps = Cons z where
  z = do reading <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         writing <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         ref <- liftIO $ newIORef Nothing
         pid <- processId
         let writer p =
               do requestResource writing
                  pid' <- processId
                  (a, xs) <-
                    finallyProcess (runStream p) $
                    liftEvent $
                    do cancelled' <- processCancelled pid'
                       when cancelled' $
                         releaseResourceWithinEvent writing
                  liftIO $ writeIORef ref (Just a)
                  releaseResource reading
                  writer xs
             reader =
               do releaseResource writing
                  requestResource reading
                  Just a <- liftIO $ readIORef ref
                  liftIO $ writeIORef ref Nothing
                  return a
             loop [] = return ()
             loop (p: ps) =
               do pid' <- processId
                  h' <- liftEvent $
                        handleSignal (processCancelling pid) $ \() ->
                        cancelProcessWithId pid'
                  finallyProcess (writer p) $
                    liftEvent $
                    do disposeEvent h'
                       cancelled <- processCancelled pid
                       unless cancelled $
                         do cancelled' <- processCancelled pid'
                            unless cancelled' $
                              error "Expected the sub-process to be cancelled: failoverStream"
                            runProcess $ loop ps
         liftEvent $ runProcess $ loop ps
         runStream $ repeatProcess reader

-- | Return the prefix of the stream of the specified length.
takeStream :: Int -> Stream a -> Stream a
takeStream n s
  | n <= 0    = emptyStream
  | otherwise =
    Cons $
    do (a, xs) <- runStream s
       return (a, takeStream (n - 1) xs)

-- | Return the longest prefix of the stream of elements that satisfy the predicate.
takeStreamWhile :: (a -> Bool) -> Stream a -> Stream a
takeStreamWhile p s =
  Cons $
  do (a, xs) <- runStream s
     if p a
       then return (a, takeStreamWhile p xs)
       else neverProcess

-- | Return the longest prefix of the stream of elements that satisfy the computation.
takeStreamWhileM :: (a -> Process Bool) -> Stream a -> Stream a
takeStreamWhileM p s =
  Cons $
  do (a, xs) <- runStream s
     f <- p a
     if f
       then return (a, takeStreamWhileM p xs)
       else neverProcess

-- | Return the suffix of the stream after the specified first elements.
dropStream :: Int -> Stream a -> Stream a
dropStream n s
  | n <= 0    = s
  | otherwise =
    Cons $
    do (a, xs) <- runStream s
       runStream $ dropStream (n - 1) xs

-- | Return the suffix of the stream of elements remaining after 'takeStreamWhile'.
dropStreamWhile :: (a -> Bool) -> Stream a -> Stream a
dropStreamWhile p s =
  Cons $
  do (a, xs) <- runStream s
     if p a
       then runStream $ dropStreamWhile p xs
       else return (a, xs)

-- | Return the suffix of the stream of elements remaining after 'takeStreamWhileM'.
dropStreamWhileM :: (a -> Process Bool) -> Stream a -> Stream a
dropStreamWhileM p s =
  Cons $
  do (a, xs) <- runStream s
     f <- p a
     if f
       then runStream $ dropStreamWhileM p xs
       else return (a, xs)

-- | Create the specified number of equivalent clones of the input stream.
cloneStream :: Int -> Stream a -> Simulation [Stream a]
cloneStream n s =
  do qs  <- forM [1..n] $ \i -> newFCFSQueue
     rs  <- newFCFSResource 1
     ref <- liftIO $ newIORef s
     let reader m q =
           do a <- liftEvent $ tryDequeue q
              case a of
                Just a  -> return a
                Nothing ->
                  usingResource rs $
                  do a <- liftEvent $ tryDequeue q
                     case a of
                       Just a  -> return a
                       Nothing ->
                         do s <- liftIO $ readIORef ref
                            (a, xs) <- runStream s
                            liftIO $ writeIORef ref xs
                            forM_ (zip [1..] qs) $ \(i, q) ->
                              unless (i == m) $
                              liftEvent $ enqueue q a
                            return a
     forM (zip [1..] qs) $ \(i, q) ->
       return $ repeatProcess $ reader i q

-- | Return a stream of first arrivals after assembling the specified number of elements.
firstArrivalStream :: Int -> Stream a -> Stream a
firstArrivalStream n s = assembleAccumStream f (1, Nothing) s
  where f (i, a0) a =
          let a0' = Just $ fromMaybe a a0
          in if i `mod` n == 0
             then return ((1, Nothing), a0')
             else return ((i + 1, a0'), Nothing)

-- | Return a stream of last arrivals after assembling the specified number of elements.
lastArrivalStream :: Int -> Stream a -> Stream a
lastArrivalStream n s = assembleAccumStream f 1 s
  where f i a =
          if i `mod` n == 0
          then return (1, Just a)
          else return (i + 1, Nothing)

-- | Assemble an accumulated stream using the supplied function.
assembleAccumStream :: (acc -> a -> Process (acc, Maybe b)) -> acc -> Stream a -> Stream b
assembleAccumStream f acc s =
  mapStream fromJust $
  filterStream isJust $
  accumStream f acc s

-- | Show the debug messages with the current simulation time.
traceStream :: Maybe String
               -- ^ the request message
               -> Maybe String
               -- ^ the response message
               -> Stream a
               -- ^ a stream
               -> Stream a
traceStream request response s = Cons $ loop s where
  loop s = do (a, xs) <-
                case request of
                  Nothing -> runStream s
                  Just message ->
                    traceProcess message $
                    runStream s
              case response of
                Nothing -> return (a, Cons $ loop xs)
                Just message ->
                  traceProcess message $
                  return (a, Cons $ loop xs)
