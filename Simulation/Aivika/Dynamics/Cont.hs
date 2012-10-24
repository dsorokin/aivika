
-- |
-- Module     : Simulation.Aivika.Dynamics.Cont
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- The 'Cont' monad is a variation of the standard Cont monad, where
-- the result of applying the continuation is a dynamic process.
--
module Simulation.Aivika.Dynamics.Cont
       (Cont) where

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Cont
