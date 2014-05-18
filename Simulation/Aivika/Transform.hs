
-- |
-- Module     : Simulation.Aivika.Transform
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines a transform of one time varying function to another
-- usually specified in the integration time points and then interpolated in
-- other time points with help of one of the memoization functions
-- like 'memo0Dynamics'.
--
module Simulation.Aivika.Transform
       (Transform(..)) where

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Memo

-- | The transform of one time varying function to another usually
-- specified in the integration time points and then interpolated in
-- other time points with help of one of the memoization functions
-- like 'memo0Dynamics'.
--
newtype Transform a b =
  Transform { runTransform :: Dynamics a -> Simulation (Dynamics b)
              -- ^ Run the transform.
            }
