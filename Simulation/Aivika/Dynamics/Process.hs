
-- |
-- Module     : Simulation.Aivika.Dynamics.Process
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- A value in the 'Process' monad represents a discontinuous process that 
-- can suspend in any simulation time point and then resume later in the same 
-- or another time point. 
-- 
-- The process of this type behaves like a dynamic process too. So, any value 
-- in the 'Dynamics' monad can be lifted to the Process monad. Moreover, 
-- a value in the Process monad can be run in the Dynamics monad.
--
-- A value of the 'ProcessID' type is just an identifier of such a process.
--
module Simulation.Aivika.Dynamics.Process
       (ProcessID,
        Process,
        processQueue,
        newProcessID,
        newProcessIDWithCatch,
        holdProcess,
        interruptProcess,
        processInterrupted,
        passivateProcess,
        processPassive,
        reactivateProcess,
        processID,
        cancelProcess,
        processCanceled,
        runProcess,
        runProcessNow,
        catchProcess,
        finallyProcess,
        throwProcess) where

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Process
