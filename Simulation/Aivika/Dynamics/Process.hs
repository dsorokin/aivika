
-- |
-- Module     : Simulation.Aivika.Dynamics.Process
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- A value in the 'Process' monad represents a discontinuous process that 
-- can suspend and resume at any time. It behaves like a dynamic process too. 
-- Any value in the 'Dynamics' monad can be lifted to the Process monad. 
-- Moreover, a value in the Process monad can be run in the Dynamics monad.
--
-- A value of the 'ProcessID' type is just an identifier of such a process.
--
module Simulation.Aivika.Dynamics.Process
       (ProcessID,
        Process,
        processQueue,
        newProcessID,
        holdProcess,
        passivateProcess,
        processPassive,
        reactivateProcess,
        processID,
        runProcess) where

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Process
