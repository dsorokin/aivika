
-- |
-- Module     : Simulation.Aivika.Trans.Cont
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The 'Cont' monad is a variation of the standard Cont monad 
-- and F# async workflow, where the result of applying 
-- the continuations is the 'Event' computation.
--
module Simulation.Aivika.Trans.Cont
       (ContCancellation(..),
        Cont) where

import Simulation.Aivika.Trans.Internal.Cont
