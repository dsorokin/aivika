
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Transform.Memo.Unboxed
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines the unboxed memoization transforms. The memoization creates such 'Dynamics'
-- computations, which values are cached in the integration time points. Then
-- these values are interpolated in all other time points.
--

module Simulation.Aivika.Transform.Memo.Unboxed
       (memoTransform,
        memo0Transform) where

import Simulation.Aivika.Parameter
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Extra
import Simulation.Aivika.Dynamics.Memo.Unboxed
import Simulation.Aivika.Transform
import Simulation.Aivika.Unboxed

-- | A transform that memoizes and order the computation in the integration time points
-- using the interpolation that knows of the Runge-Kutta method. The values are
-- calculated sequentially starting from 'starttime'.
memoTransform :: Unboxed e => Transform e e
memoTransform = Transform memoDynamics 

-- | A transform that memoizes and order the computation in the integration time points using 
-- the 'discreteDynamics' interpolation. It consumes less memory than the 'memoTransform'
-- computation but it is not aware of the Runge-Kutta method. There is a subtle
-- difference when we request for values in the intermediate time points
-- that are used by this method to integrate. In general case you should 
-- prefer the 'memo0Transform' computation above 'memoTransform'.
memo0Transform :: Unboxed e => Transform e e
memo0Transform =  Transform memo0Dynamics
