
-- |
-- Module     : Simulation.Aivika.Process
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- A value in the 'Process' monad represents a discontinuous process that 
-- can suspend in any simulation time point and then resume later in the same 
-- or another time point. 
-- 
-- The process of this type can involve the 'Event', 'Dynamics' and 'Simulation'
-- computations. Moreover, a value in the @Process@ monad can be run within
-- the @Event@ computation.
--
-- A value of the 'ProcessId' type is just an identifier of such a process.
--
module Simulation.Aivika.Process
       (-- * Process Monad
        ProcessId,
        Process,
        ProcessLift(..),
        -- * Running Process
        runProcess,
        runProcessUsingId,
        runProcessInStartTime,
        runProcessInStartTimeUsingId,
        runProcessInStopTime,
        runProcessInStopTimeUsingId,
        -- * Forking Process
        forkProcess,
        forkProcessUsingId,
        -- * Enqueuing Process
        enqueueProcess,
        enqueueProcessUsingId,
        enqueueProcessWithStartTime,
        enqueueProcessWithStartTimeUsingId,
        enqueueProcessWithStopTime,
        enqueueProcessWithStopTimeUsingId,
        -- * Creating Process Identifier
        newProcessId,
        newProcessIdWithCatch,
        processId,
        processIdWithCatch,
        processUsingId,
        -- * Holding, Interrupting, Passivating and Canceling Process
        holdProcess,
        interruptProcess,
        processInterrupted,
        passivateProcess,
        processPassive,
        reactivateProcess,
        cancelProcess,
        processCanceled,
        -- * Parallelizing Processes
        processParallel,
        processParallelUsingIds,
        processParallel_,
        processParallelUsingIds_,
        -- * Exception Handling
        catchProcess,
        finallyProcess,
        throwProcess,
        -- * Utilities
        zipProcessParallel,
        unzipProcess,
        memoProcess) where

import Data.IORef

import Control.Monad.Trans

import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.Signal

-- | Zip two parallel processes waiting for the both.
zipProcessParallel :: Process a -> Process b -> Process (a, b)
zipProcessParallel x y =
  do [Left a, Right b] <- processParallel [fmap Left x, fmap Right y]
     return (a, b)

-- | Unzip the process using memoization so that the both returned
-- processes could be applied independently, although they will refer
-- to the same pair of values.
unzipProcess :: Process (a, b) -> Simulation (Process a, Process b)
unzipProcess xy =
  do xy' <- memoProcess xy
     return (fmap fst xy', fmap snd xy')

-- | Memoize the process always returning the same value.
memoProcess :: Process a -> Simulation (Process a)
memoProcess x =
  do started  <- liftIO $ newIORef False
     computed <- newSignalSource
     value    <- liftIO $ newIORef Nothing
     return $
       do v <- liftIO $ readIORef value
          case v of
            Just v -> return v
            Nothing ->
              do f <- liftIO $ readIORef started
                 case f of
                   True ->
                     do awaitSignal $ publishSignal computed
                        Just a <- liftIO $ readIORef value
                        return a
                   False ->
                     do liftIO $ writeIORef started True
                        a <- x    -- compute only once!
                        liftIO $ writeIORef value (Just a)
                        liftEvent $ triggerSignal computed ()
                        return a
