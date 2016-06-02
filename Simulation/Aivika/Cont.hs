
-- |
-- Module     : Simulation.Aivika.Cont
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The 'Cont' monad is a variation of the standard Cont monad 
-- and F# async workflow, where the result of applying 
-- the continuations is the 'Event' computation.
--
module Simulation.Aivika.Cont
       (ContCancellation(..),
        Cont) where

import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Cont
