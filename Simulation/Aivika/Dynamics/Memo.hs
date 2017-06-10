
-- |
-- Module     : Simulation.Aivika.Dynamics.Memo
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines memo functions. The memoization creates such 'Dynamics'
-- computations, which values are cached in the integration time points. Then
-- these values are interpolated in all other time points.
--

module Simulation.Aivika.Dynamics.Memo
       (memoDynamics,
        memo0Dynamics,
        iterateDynamics,
        unzipDynamics,
        unzip0Dynamics) where

import Data.Array
import Data.Array.IO.Safe
import Data.IORef
import Control.Monad

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Dynamics.Extra

-- | Create a boxed array with default values.
newBoxedArray_ :: Ix i => (i, i) -> IO (IOArray i e)
newBoxedArray_ = newArray_

-- | Memoize and order the computation in the integration time points using 
-- the interpolation that knows of the Runge-Kutta method. The values are
-- calculated sequentially starting from 'starttime'.
memoDynamics :: Dynamics e -> Simulation (Dynamics e)
{-# INLINE memoDynamics #-}
memoDynamics (Dynamics m) = 
  Simulation $ \r ->
  do let sc = runSpecs r
         (phl, phu) = integPhaseBnds sc
         (nl, nu)   = integIterationBnds sc
     arr   <- newBoxedArray_ ((phl, nl), (phu, nu))
     nref  <- newIORef 0
     phref <- newIORef 0
     let r p = 
           do let sc  = pointSpecs p
                  n   = pointIteration p
                  ph  = pointPhase p
                  phu = integPhaseHiBnd sc 
                  loop n' ph' = 
                    if (n' > n) || ((n' == n) && (ph' > ph)) 
                    then 
                      readArray arr (ph, n)
                    else 
                      let p' = p { pointIteration = n', pointPhase = ph',
                                   pointTime = basicTime sc n' ph' }
                      in do a <- m p'
                            a `seq` writeArray arr (ph', n') a
                            if ph' >= phu 
                              then do writeIORef phref 0
                                      writeIORef nref (n' + 1)
                                      loop (n' + 1) 0
                              else do writeIORef phref (ph' + 1)
                                      loop n' (ph' + 1)
              n'  <- readIORef nref
              ph' <- readIORef phref
              loop n' ph'
     return $ interpolateDynamics $ Dynamics r

-- | Memoize and order the computation in the integration time points using 
-- the 'discreteDynamics' interpolation. It consumes less memory than the 'memoDynamics'
-- function but it is not aware of the Runge-Kutta method. There is a subtle
-- difference when we request for values in the intermediate time points
-- that are used by this method to integrate. In general case you should 
-- prefer the 'memo0Dynamics' function above 'memoDynamics'.
memo0Dynamics :: Dynamics e -> Simulation (Dynamics e)
{-# INLINE memo0Dynamics #-}
memo0Dynamics (Dynamics m) = 
  Simulation $ \r ->
  do let sc   = runSpecs r
         bnds = integIterationBnds sc
     arr  <- newBoxedArray_ bnds
     nref <- newIORef 0
     let r p =
           do let sc = pointSpecs p
                  n  = pointIteration p
                  loop n' = 
                    if n' > n
                    then 
                      readArray arr n
                    else 
                      let p' = p { pointIteration = n', pointPhase = 0,
                                   pointTime = basicTime sc n' 0 }
                      in do a <- m p'
                            a `seq` writeArray arr n' a
                            writeIORef nref (n' + 1)
                            loop (n' + 1)
              n' <- readIORef nref
              loop n'
     return $ discreteDynamics $ Dynamics r

-- | Iterate sequentially the dynamic process with side effects in 
-- the integration time points. It is equivalent to a call of the
-- 'memo0Dynamics' function but significantly more efficient, for the array 
-- is not created.
iterateDynamics :: Dynamics () -> Simulation (Dynamics ())
{-# INLINE iterateDynamics #-}
iterateDynamics (Dynamics m) = 
  Simulation $ \r ->
  do let sc = runSpecs r
     nref <- newIORef 0
     let r p =
           do let sc = pointSpecs p
                  n  = pointIteration p
                  loop n' = 
                    unless (n' > n) $
                    let p' = p { pointIteration = n', pointPhase = 0,
                                 pointTime = basicTime sc n' 0 }
                    in do a <- m p'
                          a `seq` writeIORef nref (n' + 1)
                          loop (n' + 1)
              n' <- readIORef nref
              loop n'
     return $ discreteDynamics $ Dynamics r

-- | Memoize and unzip the computation of pairs, applying the 'memoDynamics' function.
unzipDynamics :: Dynamics (a, b) -> Simulation (Dynamics a, Dynamics b)
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
unzip0Dynamics :: Dynamics (a, b) -> Simulation (Dynamics a, Dynamics b)
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
