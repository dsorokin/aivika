
-- |
-- Module     : Simulation.Aivika.Dynamics.Internal.Interpolate
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module defines interpolation functions.
-- These functions complement the memoization, possibly except for 
-- the 'initD' function which is useful to get an initial 
-- value of any dynamic process.
--

module Simulation.Aivika.Dynamics.Internal.Interpolate
       (initD,
        discrete,
        interpolate) where

import Simulation.Aivika.Dynamics.Internal.Dynamics

-- | Return the initial value.
initD :: Dynamics a -> Dynamics a
{-# INLINE initD #-}
initD (Dynamics m) =
  Dynamics $ \p ->
  if pointIteration p == 0 && pointPhase p == 0 then
    m p
  else
    let sc = pointSpecs p
    in m $ p { pointTime = basicTime sc 0 0,
               pointIteration = 0,
               pointPhase = 0 } 

-- | Discretize the computation in the integration time points.
discrete :: Dynamics a -> Dynamics a
{-# INLINE discrete #-}
discrete (Dynamics m) =
  Dynamics $ \p ->
  if pointPhase p == 0 then
    m p
  else
    let sc = pointSpecs p
        n  = pointIteration p
    in m $ p { pointTime = basicTime sc n 0,
               pointPhase = 0 }

-- | Interpolate the computation based on the integration time points only.
-- Unlike the 'discrete' function it knows about the intermediate time points 
-- that are used in the Runge-Kutta method.
interpolate :: Dynamics a -> Dynamics a
{-# INLINE interpolate #-}
interpolate (Dynamics m) = 
  Dynamics $ \p -> 
  if pointPhase p >= 0 then 
    m p
  else 
    let sc = pointSpecs p
        n  = pointIteration p
    in m $ p { pointTime = basicTime sc n 0,
               pointPhase = 0 }
