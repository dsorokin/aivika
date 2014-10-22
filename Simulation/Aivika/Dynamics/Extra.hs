
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Dynamics.Extra
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines auxiliary functions such as interpolation ones
-- that complement the memoization, for example. There are scan functions too.
--

module Simulation.Aivika.Dynamics.Extra
       (-- * Interpolation
        initDynamics,
        discreteDynamics,
        interpolateDynamics,
        -- * Scans
        scanDynamics,
        scan1Dynamics) where

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics

-- | Return the initial value.
initDynamics :: Dynamics a -> Dynamics a
{-# INLINE initDynamics #-}
initDynamics (Dynamics m) =
  Dynamics $ \p ->
  let sc = pointSpecs p
  in m $ p { pointTime = basicTime sc 0 0,
             pointIteration = 0,
             pointPhase = 0 }

-- | Discretize the computation in the integration time points.
discreteDynamics :: Dynamics a -> Dynamics a
{-# INLINE discreteDynamics #-}
discreteDynamics (Dynamics m) =
  Dynamics $ \p ->
  if pointPhase p == 0 then
    m p
  else
    let sc = pointSpecs p
        n  = pointIteration p
    in m $ p { pointTime = basicTime sc n 0,
               pointPhase = 0 }

-- | Interpolate the computation based on the integration time points only.
-- Unlike the 'discreteDynamics' function it knows about the intermediate 
-- time points that are used in the Runge-Kutta method.
interpolateDynamics :: Dynamics a -> Dynamics a
{-# INLINE interpolateDynamics #-}
interpolateDynamics (Dynamics m) = 
  Dynamics $ \p -> 
  if pointPhase p >= 0 then 
    m p
  else 
    let sc = pointSpecs p
        n  = pointIteration p
    in m $ p { pointTime = basicTime sc n 0,
               pointPhase = 0 }

-- | Like the standard 'scanl1' function but applied to values in 
-- the integration time points. The accumulator values are transformed
-- according to the second argument, which should be either function 
-- 'memo0Dynamics' or its unboxed version.
scan1Dynamics :: (a -> a -> a)
                 -> (Dynamics a -> Simulation (Dynamics a))
                 -> (Dynamics a -> Simulation (Dynamics a))
scan1Dynamics f tr m =
  mdo y <- tr $ Dynamics $ \p ->
        case pointIteration p of
          0 -> 
            invokeDynamics p m
          n -> do 
            let sc = pointSpecs p
                ty = basicTime sc (n - 1) 0
                py = p { pointTime = ty, pointIteration = n - 1, pointPhase = 0 }
            s <- invokeDynamics py y
            x <- invokeDynamics p m
            return $! f s x
      return y

-- | Like the standard 'scanl' function but applied to values in 
-- the integration time points. The accumulator values are transformed
-- according to the third argument, which should be either function
-- 'memo0Dynamics' or its unboxed version.
scanDynamics :: (a -> b -> a)
                -> a
                -> (Dynamics a -> Simulation (Dynamics a))
                -> (Dynamics b -> Simulation (Dynamics a))
scanDynamics f acc tr m =
  mdo y <- tr $ Dynamics $ \p ->
        case pointIteration p of
          0 -> do
            x <- invokeDynamics p m
            return $! f acc x
          n -> do 
            let sc = pointSpecs p
                ty = basicTime sc (n - 1) 0
                py = p { pointTime = ty, pointIteration = n - 1, pointPhase = 0 }
            s <- invokeDynamics py y
            x <- invokeDynamics p m
            return $! f s x
      return y
