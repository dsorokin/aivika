
-- |
-- Module     : Simulation.Aivika.Dynamics.Cont
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The 'Cont' monad is a variation of the standard Cont monad 
-- and F# async workflow, where the result of applying 
-- the continuation is a dynamic process.
--
module Simulation.Aivika.Dynamics.Cont
       (Cont) where

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Cont
