
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
        workerProcessId,
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

-- | It models a worker that takes @a@ and produces @b@.
data Worker a b =
  Worker { workerProcessId :: ProcessId,
           -- ^ The process identifier of the worker.
           workerProduce :: a -> Process b,
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

-- | Create a new worker that can produce output @b@ by the input @a@.
newWorker :: (a -> Process b) -> Simulation (Worker a b)
newWorker produce =
  do pid <- newProcessId
     newWorkerUsingId pid produce

-- | Create with the specified identifier a new worker that can produce
-- output @b@ by the input @a@.
newWorkerUsingId :: ProcessId -> (a -> Process b) -> Simulation (Worker a b)
newWorkerUsingId pid produce =
  do r1 <- liftIO $ newIORef 0
     r2 <- liftIO $ newIORef 0
     r3 <- liftIO $ newIORef 0
     r4 <- liftIO $ newIORef emptySamplingStats
     r5 <- liftIO $ newIORef emptySamplingStats
     r6 <- liftIO $ newIORef emptySamplingStats
     s1 <- newSignalSource
     s2 <- newSignalSource
     s3 <- newSignalSource
     return Worker { workerProcessId = pid,
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
workerProcessor :: Worker a b -> Processor a b
workerProcessor w =
  Processor $ \xs ->
  Cons $
  do loadingRes   <- liftSimulation $ newFCFSResourceWithMaxCount 1 (Just 1)
     producingRes <- liftSimulation $ newFCFSResourceWithMaxCount 0 (Just 1)
     production   <- liftIO $ newIORef Nothing
     let worker t' xs =
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
              b <- workerProduce w a
              t2 <- liftDynamics time
              liftEvent $
                do liftIO $
                     do writeIORef production (Just b)
                        modifyIORef (workerTotalEffortTimeRef w) (+ (t2 - t1))
                        modifyIORef (workerEffortTimeRef w) $
                          addSamplingStats (t2 - t1)
                   releaseResourceWithinEvent producingRes
                   triggerSignal (workerProducedSource w) (a, b)
              worker t2 xs'
         writer =
           Cons $
           do requestResource producingRes
              Just b <- liftIO $ readIORef production
              liftIO $ writeIORef production Nothing
              releaseResource loadingRes
              return (b, writer)
     t' <- liftDynamics time
     childProcessUsingId (workerProcessId w) $ worker t' xs
     runStream writer

-- | Return the counted total free time for the worker.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerTotalFreeTime :: Worker a b -> Observable Double
workerTotalFreeTime w =
  let read = Event $ \p -> readIORef (workerTotalFreeTimeRef w)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (workerLoaded w) }

-- | Return the counted total effort time that the worker spent on all tasks.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerTotalEffortTime :: Worker a b -> Observable Double
workerTotalEffortTime w =
  let read = Event $ \p -> readIORef (workerTotalEffortTimeRef w)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (workerProduced w) }

-- | Return the counted total time that the worker spent awaiting in the lock state.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerTotalTimeInLock :: Worker a b -> Observable Double
workerTotalTimeInLock w =
  let read = Event $ \p -> readIORef (workerTotalTimeInLockRef w)
  in Observable { readObservable = read,
                  observableChanged_ = workerReleased w }

-- | Return the statistics of the free time for the worker.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerFreeTime :: Worker a b -> Observable (SamplingStats Double)
workerFreeTime w =
  let read = Event $ \p -> readIORef (workerFreeTimeRef w)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (workerLoaded w) }

-- | Return the statistics of the effort time that the worker spent on the task.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerEffortTime :: Worker a b -> Observable (SamplingStats Double)
workerEffortTime w =
  let read = Event $ \p -> readIORef (workerEffortTimeRef w)
  in Observable { readObservable = read,
                  observableChanged_ =
                    mapSignal (const ()) (workerProduced w) }

-- | Return the statistics of the time that the worker spent awaiting in the lock state.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
workerTimeInLock :: Worker a b -> Observable (SamplingStats Double)
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
workerFreeTimeFactor :: Worker a b -> Observable Double
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
workerEffortTimeFactor :: Worker a b -> Observable Double
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
workerTimeInLockFactor :: Worker a b -> Observable Double
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
workerLoaded :: Worker a b -> Signal a
workerLoaded = publishSignal . workerLoadedSource

-- | Raised when the worker has just produced the result.
workerProduced :: Worker a b -> Signal (a, b)
workerProduced = publishSignal . workerProducedSource

-- | Raised when the worker is released either after producing
-- the result and the further releasing from the lock or in the outset before
-- proceeding to the first task.
workerReleased :: Worker a b -> Signal ()
workerReleased = publishSignal . workerReleasedSource
