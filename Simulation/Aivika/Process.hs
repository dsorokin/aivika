
-- |
-- Module     : Simulation.Aivika.Process
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
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
-- The characteristic property of the @Process@ type is function 'holdProcess'
-- that suspends the current process for the specified time interval.
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
        -- * Spawning Processes
        spawnProcess,
        spawnProcessUsingId,
        spawnProcessWith,
        spawnProcessUsingIdWith,
        -- * Enqueueing Process
        enqueueProcess,
        enqueueProcessUsingId,
        -- * Creating Process Identifier
        newProcessId,
        processId,
        processUsingId,
        -- * Holding, Interrupting, Passivating and Canceling Process
        holdProcess,
        interruptProcess,
        processInterrupted,
        passivateProcess,
        processPassive,
        reactivateProcess,
        cancelProcessWithId,
        cancelProcess,
        processCancelled,
        processCancelling,
        whenCancellingProcess,
        -- * Awaiting Signal
        processAwait,
        -- * Preemption
        processPreemptionBeginning,
        processPreemptionEnding,
        -- * Yield of Process
        processYield,
        -- * Process Timeout
        timeoutProcess,
        timeoutProcessUsingId,
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
        zip3ProcessParallel,
        unzipProcess,
        -- * Memoizing Process
        memoProcess,
        -- * Never Ending Process
        neverProcess,
        -- * Retrying Computation
        retryProcess,
        -- * Debugging
        traceProcess) where

import Simulation.Aivika.Internal.Process
