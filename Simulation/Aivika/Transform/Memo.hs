
-- |
-- Module     : Simulation.Aivika.Transform.Memo
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines memoization transforms. The memoization creates such 'Dynamics'
-- computations, which values are cached in the integration time points. Then
-- these values are interpolated in all other time points.
--

module Simulation.Aivika.Transform.Memo
       (memoTransform,
        memo0Transform,
        iteratingTransform) where

import Simulation.Aivika.Parameter
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Extra
import Simulation.Aivika.Dynamics.Memo
import Simulation.Aivika.Transform

-- | A transform that memoizes and order the computation in the integration time points
-- using the interpolation that knows of the Runge-Kutta method. The values are
-- calculated sequentially starting from 'starttime'.
memoTransform :: Transform e e
memoTransform = Transform memoDynamics 

-- | A transform that memoizes and order the computation in the integration time points using 
-- the 'discreteDynamics' interpolation. It consumes less memory than the 'memoTransform'
-- computation but it is not aware of the Runge-Kutta method. There is a subtle
-- difference when we request for values in the intermediate time points
-- that are used by this method to integrate. In general case you should 
-- prefer the 'memo0Transform' computation above 'memoTransform'.
memo0Transform :: Transform e e
memo0Transform =  Transform memo0Dynamics

-- | A transform that iterates sequentially the dynamic process with side effects in 
-- the integration time points. It is equivalent to the 'memo0Transform' computation
-- but significantly more efficient, for the internal array is not created.
iteratingTransform :: Transform () ()
iteratingTransform = Transform iterateDynamics
