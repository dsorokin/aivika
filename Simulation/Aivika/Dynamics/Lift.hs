
-- |
-- Module     : Simulation.Aivika.Dynamics.Lift
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module defines the 'liftD' function that allows embedding
-- the 'Dynamics' computation.
--
module Simulation.Aivika.Dynamics.Lift (Lift(..)) where

import Simulation.Aivika.Dynamics

-- | The 'Lift' class defines a type which the 'Dynamics' 
-- computation can be lifted to.
class Lift m where
  -- | Lift the computation.
  liftD :: Dynamics a -> m a
