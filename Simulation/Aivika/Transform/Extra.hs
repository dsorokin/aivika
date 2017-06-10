
-- |
-- Module     : Simulation.Aivika.Transform.Extra
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines auxiliary computations such as interpolation ones
-- that complement the memoization, for example. There are scan computations too.
--

module Simulation.Aivika.Transform.Extra
       (-- * Interpolation
        initTransform,
        discreteTransform,
        interpolatingTransform,
        -- * Scans
        scanTransform,
        scan1Transform) where

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Extra
import Simulation.Aivika.Transform
import Simulation.Aivika.Transform.Memo

-- | A transform that returns the initial value.
initTransform :: Transform a a
initTransform = Transform $ return . initDynamics

-- | A transform that discretizes the computation in the integration time points.
discreteTransform :: Transform a a
discreteTransform = Transform $ return . discreteDynamics

-- | A tranform that interpolates the computation based on the integration time points only.
-- Unlike the 'discreteTransform' computation it knows about the intermediate 
-- time points that are used in the Runge-Kutta method.
interpolatingTransform :: Transform a a
interpolatingTransform = Transform $ return . interpolateDynamics 

-- | Like the standard 'scanl1' function but applied to values in 
-- the integration time points. The accumulator values are transformed
-- according to the second argument, which should be either  
-- 'memo0Transform' or its unboxed version.
scan1Transform :: (a -> a -> a) -> Transform a a -> Transform a a
scan1Transform f (Transform tr) = Transform $ scan1Dynamics f tr

-- | Like the standard 'scanl' function but applied to values in 
-- the integration time points. The accumulator values are transformed
-- according to the third argument, which should be either
-- 'memo0Transform' or its unboxed version.
scanTransform :: (a -> b -> a) -> a -> Transform a a -> Transform b a
scanTransform f acc (Transform tr) = Transform $ scanDynamics f acc tr
