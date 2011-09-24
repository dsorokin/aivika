
-- |
-- Module     : Simulation.Aivika.Dynamics.Cont
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- The 'Cont' monad looks somewhere like the standard ContT monad transformer 
-- parameterized by the 'Dynamics' monad, although this analogy is not strong. 
-- The main idea is to represent the continuation as a dynamic process varying 
-- in time.
--
module Simulation.Aivika.Dynamics.Cont
       (Cont,
        runCont) where

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Cont
