
-- |
-- Module     : Simulation.Aivika.Trans.Dynamics.Memo
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines memo functions. The memoization creates such 'Dynamics'
-- computations, which values are cached in the integration time points. Then
-- these values are interpolated in all other time points.
--

module Simulation.Aivika.Trans.Dynamics.Memo
       (memoDynamics,
        memo0Dynamics,
        iterateDynamics,
        unzipDynamics,
        unzip0Dynamics) where

import Control.Monad

import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.ProtoArray
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Dynamics.Interpolate

-- | Memoize and order the computation in the integration time points using 
-- the interpolation that knows of the Runge-Kutta method. The values are
-- calculated sequentially starting from 'starttime'.
memoDynamics :: Comp m => Dynamics m e -> Simulation m (Dynamics m e)
{-# INLINABLE memoDynamics #-}
memoDynamics (Dynamics m) = 
  Simulation $ \r ->
  do let sc  = runSpecs r
         s   = runSession r
         phs = 1 + integPhaseHiBnd sc
         ns  = 1 + integIterationHiBnd sc
     arr   <- newProtoArray_ s (ns * phs)
     nref  <- newProtoRef s 0
     phref <- newProtoRef s 0
     let r p = 
           do let n  = pointIteration p
                  ph = pointPhase p
                  i  = n * phs + ph
                  loop n' ph' = 
                    if (n' > n) || ((n' == n) && (ph' > ph)) 
                    then 
                      readProtoArray arr i
                    else 
                      let p' = p { pointIteration = n', pointPhase = ph',
                                   pointTime = basicTime sc n' ph' }
                          i' = n' * phs + ph'
                      in do a <- m p'
                            a `seq` writeProtoArray arr i' a
                            if ph' >= phs - 1 
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
memo0Dynamics :: Comp m => Dynamics m e -> Simulation m (Dynamics m e)
{-# INLINABLE memo0Dynamics #-}
memo0Dynamics (Dynamics m) = 
  Simulation $ \r ->
  do let sc = runSpecs r
         s  = runSession r
         ns = 1 + integIterationHiBnd sc
     arr  <- newProtoArray_ s ns
     nref <- newProtoRef s 0
     let r p =
           do let sc = pointSpecs p
                  n  = pointIteration p
                  loop n' = 
                    if n' > n
                    then 
                      readProtoArray arr n
                    else 
                      let p' = p { pointIteration = n', pointPhase = 0,
                                   pointTime = basicTime sc n' 0 }
                      in do a <- m p'
                            a `seq` writeProtoArray arr n' a
                            writeProtoRef nref (n' + 1)
                            loop (n' + 1)
              n' <- readProtoRef nref
              loop n'
     return $ discreteDynamics $ Dynamics r

-- | Iterate sequentially the dynamic process with side effects in 
-- the integration time points. It is equivalent to a call of the
-- 'memo0Dynamics' function but significantly more efficient, for the array 
-- is not created.
iterateDynamics :: Comp m => Dynamics m () -> Simulation m (Dynamics m ())
{-# INLINABLE iterateDynamics #-}
iterateDynamics (Dynamics m) = 
  Simulation $ \r ->
  do let sc = runSpecs r
         s  = runSession r
     nref <- newProtoRef s 0
     let r p =
           do let sc = pointSpecs p
                  n  = pointIteration p
                  loop n' = 
                    unless (n' > n) $
                    let p' = p { pointIteration = n', pointPhase = 0,
                                 pointTime = basicTime sc n' 0 }
                    in do a <- m p'
                          a `seq` writeProtoRef nref (n' + 1)
                          loop (n' + 1)
              n' <- readProtoRef nref
              loop n'
     return $ discreteDynamics $ Dynamics r

-- | Memoize and unzip the computation of pairs, applying the 'memoDynamics' function.
unzipDynamics :: Comp m => Dynamics m (a, b) -> Simulation m (Dynamics m a, Dynamics m b)
{-# INLINABLE unzipDynamics #-}
unzipDynamics m =
  Simulation $ \r ->
  do m' <- invokeSimulation r (memoDynamics m)
     let ma =
           Dynamics $ \p ->
           do (a, _) <- invokeDynamics p m'
              return a
         mb =
           Dynamics $ \p ->
           do (_, b) <- invokeDynamics p m'
              return b
     return (ma, mb)

-- | Memoize and unzip the computation of pairs, applying the 'memo0Dynamics' function.
unzip0Dynamics :: Comp m => Dynamics m (a, b) -> Simulation m (Dynamics m a, Dynamics m b)
{-# INLINABLE unzip0Dynamics #-}
unzip0Dynamics m =
  Simulation $ \r ->
  do m' <- invokeSimulation r (memo0Dynamics m)
     let ma =
           Dynamics $ \p ->
           do (a, _) <- invokeDynamics p m'
              return a
         mb =
           Dynamics $ \p ->
           do (_, b) <- invokeDynamics p m'
              return b
     return (ma, mb)
