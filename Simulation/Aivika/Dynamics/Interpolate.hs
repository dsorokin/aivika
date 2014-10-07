
-- |
-- Module     : Simulation.Aivika.Dynamics.Interpolate
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines interpolation functions.
-- These functions complement the memoization.
--

module Simulation.Aivika.Dynamics.Interpolate
       (initDynamics,
        discreteDynamics,
        interpolateDynamics) where

import Simulation.Aivika.Internal.Specs
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
