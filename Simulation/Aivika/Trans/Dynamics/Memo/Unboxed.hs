
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Dynamics.Memo.Unboxed
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the unboxed memo functions. The memoization creates such 'DynamicsT'
-- computations, which values are cached in the integration time points. Then
-- these values are interpolated in all other time points.
--

module Simulation.Aivika.Trans.Dynamics.Memo.Unboxed
       (memoDynamics,
        memo0Dynamics) where

import Control.Monad

import Simulation.Aivika.Trans.Internal.ProtoRef
import Simulation.Aivika.Trans.Internal.ProtoArray
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.MonadSim
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Dynamics.Interpolate
import Simulation.Aivika.Trans.Unboxed

-- | Memoize and order the computation in the integration time points using 
-- the interpolation that knows of the Runge-Kutta method. The values are
-- calculated sequentially starting from 'starttime'.
memoDynamics :: (Unboxed m e, MonadSim m) => DynamicsT m e -> SimulationT m (DynamicsT m e)
{-# INLINABLE memoDynamics #-}
memoDynamics (Dynamics m) = 
  Simulation $ \r ->
  do let sc = runSpecs r
         s  = runSession r
         (phl, phu) = integPhaseBnds sc
         (nl, nu)   = integIterationBnds sc
     arr   <- newProtoUArray_ s ((phl, nl), (phu, nu))
     nref  <- newProtoRef s 0
     phref <- newProtoRef s 0
     let r p =
           do let sc  = pointSpecs p
                  n   = pointIteration p
                  ph  = pointPhase p
                  phu = integPhaseHiBnd sc 
                  loop n' ph' = 
                    if (n' > n) || ((n' == n) && (ph' > ph)) 
                    then 
                      readProtoUArray arr (ph, n)
                    else 
                      let p' = p { pointIteration = n', 
                                   pointPhase = ph',
                                   pointTime = basicTime sc n' ph' }
                      in do a <- m p'
                            a `seq` writeProtoUArray arr (ph', n') a
                            if ph' >= phu 
                              then do writeProtoRef phref 0
                                      writeProtoRef nref (n' + 1)
                                      loop (n' + 1) 0
                              else do writeProtoRef phref (ph' + 1)
                                      loop n' (ph' + 1)
              n'  <- readProtoRef nref
              ph' <- readProtoRef phref
              loop n' ph'
     return $ interpolateDynamics $ Dynamics r

-- | Memoize and order the computation in the integration time points using 
-- the 'discreteDynamics' interpolation. It consumes less memory than the 'memoDynamics'
-- function but it is not aware of the Runge-Kutta method. There is a subtle
-- difference when we request for values in the intermediate time points
-- that are used by this method to integrate. In general case you should 
-- prefer the 'memo0Dynamics' function above 'memoDynamics'.
memo0Dynamics :: (Unboxed m e, MonadSim m) => DynamicsT m e -> SimulationT m (DynamicsT m e)
{-# INLINABLE memo0Dynamics #-}
memo0Dynamics (Dynamics m) = 
  Simulation $ \r ->
  do let sc   = runSpecs r
         s    = runSession r
         bnds = integIterationBnds sc
     arr  <- newProtoUArray_ s bnds
     nref <- newProtoRef s 0
     let r p =
           do let sc = pointSpecs p
                  n  = pointIteration p
                  loop n' = 
                    if n' > n
                    then 
                      readProtoUArray arr n
                    else 
                      let p' = p { pointIteration = n', pointPhase = 0,
                                   pointTime = basicTime sc n' 0 }
                      in do a <- m p'
                            a `seq` writeProtoUArray arr n' a
                            writeProtoRef nref (n' + 1)
                            loop (n' + 1)
              n' <- readProtoRef nref
              loop n'
     return $ discreteDynamics $ Dynamics r
