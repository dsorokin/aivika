
-- |
-- Module     : Simulation.Aivika.Worker
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- It defines the data structure that models the worker.
module Simulation.Aivika.Worker
       (-- * Worker
        Worker,
        newWorker,
        newWorkerUsingId,
        newWorkerWithState,
        newWorkerWithStateUsingId,
        workerProcessId,
        workerInitState,
        workerState,
        -- * Worker's Processor
        workerProcessor,
        -- * Worker Properties and Activities
        workerTotalFreeTime,
        workerTotalEffortTime,
        workerTotalTimeInLock,
        workerFreeTime,
        workerEffortTime,
        workerTimeInLock,
        workerFreeTimeFactor,
        workerEffortTimeFactor,
        workerTimeInLockFactor,
        -- * Worker's Summary
        workerSummary,
        -- * Signals
        workerReleased,
        workerLoaded,
        workerProduced) where

import Data.IORef
import Data.Monoid
import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Observable
import Simulation.Aivika.Signal
import Simulation.Aivika.Resource
import Simulation.Aivika.Process
import Simulation.Aivika.Processor
import Simulation.Aivika.Stream
import Simulation.Aivika.Statistics

-- | It models a worker that takes @a@ and produces @b@ having state @s@.
data Worker s a b =
  Worker { workerProcessId :: ProcessId,
           -- ^ The process identifier of the worker.
           workerInitState :: s,
           -- ^ The initial state of the worker.
           workerStateRef :: IORef s,
           -- ^ The current state of the worker.
           workerProduce :: (s, a) -> Process (s, b),
           -- ^ Produce @b@ by specified @a@.
           workerTotalFreeTimeRef :: IORef Double,
           -- ^ The counted total free time.
           workerTotalEffortTimeRef :: IORef Double,
           -- ^ The counted total effort time.
           workerTotalTimeInLockRef :: IORef Double,
           -- ^ The counted total time spent in the lock.
           workerFreeTimeRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the free time.
           workerEffortTimeRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the effort time.
           workerTimeInLockRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the time spent in the lock.
           workerLoadedSource :: SignalSource a,
           -- ^ A signal raised when the worker is loaded with a new task.
           workerProducedSource :: SignalSource (a, b),
           -- ^ A signal raised when the worker produces the result.
           workerReleasedSource :: SignalSource ()
           -- ^ A signal raised when the worked is released either
           -- after producing the result or in the outset before
           -- proceeding to the first task.
         }

-- | Create a new worker that can produce output @b@ by input @a@.
newWorker :: (a -> Process b)
             -- ^ produce an output by the specified input
             -> Simulation (Worker () a b)
newWorker produce =
  do pid <- newProcessId
     newWorkerUsingId pid produce

-- | Create with the specified identifier a new worker that can produce
-- output @b@ by input @a@.
newWorkerUsingId :: ProcessId
                    -- ^ the idenitifier for the internal process
                    -> (a -> Process b)
                    -- ^ produce an output by the specified input
                    -> Simulation (Worker () a b)
newWorkerUsingId pid produce =
  newWorkerWithStateUsingId pid () $ \(s, a) ->
  do b <- produce a
     return (s, b)

-- | Create a new worker that can produce output @b@ by input @a@
-- starting from state @s@.
newWorkerWithState :: s
                      -- ^ the initial state
                      -> ((s, a) -> Process (s, b))
                      -- ^ produce an output by the specified input
                      -- and update the state 
                      -> Simulation (Worker s a b)
newWorkerWithState state produce =
  do pid <- newProcessId
     newWorkerWithStateUsingId pid state produce

-- | Create with the specified identifier a new worker that can produce
-- output @b@ by input @a@ starting from state @s@.
newWorkerWithStateUsingId :: ProcessId
                             -- ^ the identifier for the internal process
                             -> s
                             -- ^ the initial state
                             -> ((s, a) -> Process (s, b))
                             -- ^ produce an output by the specified input
                             -- and update the state
                             -> Simulation (Worker s a b)
newWorkerWithStateUsingId pid state produce =
  do r0 <- liftIO $ newIORef state
     r1 <- liftIO $ newIORef 0
     r2 <- liftIO $ newIORef 0
     r3 <- liftIO $ newIORef 0
     r4 <- liftIO $ newIORef emptySamplingStats
     r5 <- liftIO $ newIORef emptySamplingStats
     r6 <- liftIO $ newIORef emptySamplingStats
     s1 <- newSignalSource
     s2 <- newSignalSource
     s3 <- newSignalSource
     return Worker { workerProcessId = pid,
                     workerInitState = state,
                     workerStateRef = r0,
                     workerProduce = produce,
                     workerTotalFreeTimeRef = r1,
                     workerTotalEffortTimeRef = r2,
                     workerTotalTimeInLockRef = r3,
                     workerFreeTimeRef = r4,
                     workerEffortTimeRef = r5,
                     workerTimeInLockRef = r6,
                     workerLoadedSource = s1,
                     workerProducedSource = s2,
                     workerReleasedSource = s3 }

-- | Return a processor by the specified worker.
--
-- You cannot use more than one processor for the worker. The worker is bound up
-- with the process identifier, which is either specified explicitly or generated
-- when creating a worker. This identifier cannot be used twice when running
-- the processor; otherwise, a run time error will be raised.
workerProcessor :: Worker s a b -> Processor a b
workerProcessor w =
  Processor $ \xs ->
  Cons $
  do loadingRes   <- liftSimulation $ newFCFSResourceWithMaxCount 1 (Just 1)
     producingRes <- liftSimulation $ newFCFSResourceWithMaxCount 0 (Just 1)
     production   <- liftIO $ newIORef Nothing
     let worker s t' xs =
           do requestResource loadingRes
              t0 <- liftDynamics time
              liftEvent $
                do liftIO $
                     do modifyIORef (workerTotalTimeInLockRef w) (+ (t0 - t'))
                        modifyIORef (workerTimeInLockRef w) $
                          addSamplingStats (t0 - t')
                   triggerSignal (workerReleasedSource w) ()
              -- get input
              (a, xs') <- runStream xs
              t1 <- liftDynamics time
              liftEvent $
                do liftIO $
                     do modifyIORef (workerTotalFreeTimeRef w) (+ (t1 - t0))
                        modifyIORef (workerFreeTimeRef w) $
                          addSamplingStats (t1 - t0)
                   triggerSignal (workerLoadedSource w) a
              -- produce
              (s', b) <- workerProduce w (s, a)
              t2 <- liftDynamics time
              liftEvent $
                do liftIO $
                     do writeIORef production (Just b)
                        writeIORef (workerStateRef w) s'
                        modifyIORef (workerTotalEffortTimeRef w) (+ (t2 - t1))
                        modifyIORef (workerEffortTimeRef w) $
                          addSamplingStats (t2 - t1)
                   releaseResourceWithinEvent producingRes
                   triggerSignal (workerProducedSource w) (a, b)
              worker s' t2 xs'
         writer =
           Cons $
           do requestResource producingRes
              Just b <- liftIO $ readIORef production
              liftIO $ writeIORef production Nothing
              releaseResource loadingRes
              return (b, writer)
     t' <- liftDynamics time
     childProcessUsingId (workerProcessId w) $
       worker (workerInitState w) t' xs
     runStream writer

-- | Return the current state of the worker.
workerState :: Worker s a b -> Observable s
workerState w =
  let read = Event $ \p -> readIORef (workerStateRef w)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (workerProduced w) }

-- | Return the counted total free time for the worker.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerTotalFreeTime :: Worker s a b -> Observable Double
workerTotalFreeTime w =
  let read = Event $ \p -> readIORef (workerTotalFreeTimeRef w)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (workerLoaded w) }

-- | Return the counted total effort time that the worker spent on all tasks.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerTotalEffortTime :: Worker s a b -> Observable Double
workerTotalEffortTime w =
  let read = Event $ \p -> readIORef (workerTotalEffortTimeRef w)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (workerProduced w) }

-- | Return the counted total time that the worker spent awaiting in the lock state.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerTotalTimeInLock :: Worker s a b -> Observable Double
workerTotalTimeInLock w =
  let read = Event $ \p -> readIORef (workerTotalTimeInLockRef w)
  in Observable { readObservable = read,
                  observableChanged_ = workerReleased w }

-- | Return the statistics of the free time for the worker.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerFreeTime :: Worker s a b -> Observable (SamplingStats Double)
workerFreeTime w =
  let read = Event $ \p -> readIORef (workerFreeTimeRef w)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (workerLoaded w) }

-- | Return the statistics of the effort time that the worker spent on the task.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerEffortTime :: Worker s a b -> Observable (SamplingStats Double)
workerEffortTime w =
  let read = Event $ \p -> readIORef (workerEffortTimeRef w)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (workerProduced w) }

-- | Return the statistics of the time that the worker spent awaiting in the lock state.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerTimeInLock :: Worker s a b -> Observable (SamplingStats Double)
workerTimeInLock w =
  let read = Event $ \p -> readIORef (workerTimeInLockRef w)
  in Observable { readObservable = read,
                  observableChanged_ = workerReleased w }

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the worker was free awaiting for the next task and was not locking
-- at the same time.
--
-- This factor is calculated as
--
-- @
--   totalFreeTime \/ (totalFreeTime + totalEfforTime + totalTimeInLock)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
workerFreeTimeFactor :: Worker s a b -> Observable Double
workerFreeTimeFactor w =
  let read =
        Event $ \p ->
        do x1 <- readIORef (workerTotalFreeTimeRef w)
           x2 <- readIORef (workerTotalEffortTimeRef w)
           x3 <- readIORef (workerTotalTimeInLockRef w)
           return (x1 / (x1 + x2 + x3))
      signal =
        mapSignal (const ()) (workerLoaded w) <>
        mapSignal (const ()) (workerProduced w) <>
        workerReleased w
  in Observable { readObservable = read,
                  observableChanged_ = signal }

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the worker was busy with direct doing his/her work.
--
-- This factor is calculated as
--
-- @
--   totalEffortTime \/ (totalFreeTime + totalEfforTime + totalTimeInLock)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
workerEffortTimeFactor :: Worker s a b -> Observable Double
workerEffortTimeFactor w =
  let read =
        Event $ \p ->
        do x1 <- readIORef (workerTotalFreeTimeRef w)
           x2 <- readIORef (workerTotalEffortTimeRef w)
           x3 <- readIORef (workerTotalTimeInLockRef w)
           return (x2 / (x1 + x2 + x3))
      signal =
        mapSignal (const ()) (workerLoaded w) <>
        mapSignal (const ()) (workerProduced w) <>
        workerReleased w
  in Observable { readObservable = read,
                  observableChanged_ = signal }

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the worker was locked right after he finished his/her task but not was able
-- to pass the production for the further processing.
--
-- This factor is calculated as
--
-- @
--   totalTimeInLock \/ (totalFreeTime + totalEfforTime + totalTimeInLock)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
workerTimeInLockFactor :: Worker s a b -> Observable Double
workerTimeInLockFactor w =
  let read =
        Event $ \p ->
        do x1 <- readIORef (workerTotalFreeTimeRef w)
           x2 <- readIORef (workerTotalEffortTimeRef w)
           x3 <- readIORef (workerTotalTimeInLockRef w)
           return (x3 / (x1 + x2 + x3))
      signal =
        mapSignal (const ()) (workerLoaded w) <>
        mapSignal (const ()) (workerProduced w) <>
        workerReleased w
  in Observable { readObservable = read,
                  observableChanged_ = signal }

-- | Raised when the worker is loaded with a new task.
workerLoaded :: Worker s a b -> Signal a
workerLoaded = publishSignal . workerLoadedSource

-- | Raised when the worker has just produced the result.
workerProduced :: Worker s a b -> Signal (a, b)
workerProduced = publishSignal . workerProducedSource

-- | Raised when the worker is released either after producing
-- the result and the further releasing from the lock or in the outset before
-- proceeding to the first task.
workerReleased :: Worker s a b -> Signal ()
workerReleased = publishSignal . workerReleasedSource

-- | Return the summary for the worker with desciption of its
-- properties and activities using the specified indent.
workerSummary :: Worker s a b -> Int -> Observable ShowS
workerSummary w indent =
  let read =
        Event $ \p ->
        do tx1 <- readIORef (workerTotalFreeTimeRef w)
           tx2 <- readIORef (workerTotalEffortTimeRef w)
           tx3 <- readIORef (workerTotalTimeInLockRef w)
           let xf1 = tx1 / (tx1 + tx2 + tx3)
               xf2 = tx2 / (tx1 + tx2 + tx3)
               xf3 = tx3 / (tx1 + tx2 + tx3)
           xs1 <- readIORef (workerFreeTimeRef w)
           xs2 <- readIORef (workerEffortTimeRef w)
           xs3 <- readIORef (workerTimeInLockRef w)
           let tab = replicate indent ' '
           return $
             showString tab .
             showString "total free time = " . shows tx1 .
             showString "\n" .
             showString tab .
             showString "total effort time = " . shows tx2 .
             showString "\n" .
             showString tab .
             showString "total time in the lock = " . shows tx3 .
             showString "\n\n" .
             showString tab .
             showString "free time factor (from 0 to 1) = " . shows xf1 .
             showString "\n" .
             showString tab .
             showString "effort time factor (from 0 to 1) = " . shows xf2 .
             showString "\n" .
             showString tab .
             showString "factor for the time spent in lock (from 0 to 1) = " . shows xf3 .
             showString "\n\n" .
             showString tab .
             showString "free time:\n\n" .
             samplingStatsSummary xs1 (2 + indent) .
             showString "\n\n" .
             showString tab .
             showString "effort time:\n\n" .
             samplingStatsSummary xs2 (2 + indent) .
             showString "\n\n" .
             showString tab .
             showString "time spent in the lock:\n\n" .
             samplingStatsSummary xs3 (2 + indent)
      signal =
        mapSignal (const ()) (workerLoaded w) <>
        mapSignal (const ()) (workerProduced w) <>
        workerReleased w
  in Observable { readObservable = read,
                  observableChanged_ = signal }
