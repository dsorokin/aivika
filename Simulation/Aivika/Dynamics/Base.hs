
-- |
-- Module     : Simulation.Aivika.Dynamics.Base
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module defines mainly the fold functions.
--
module Simulation.Aivika.Dynamics.Base
       (-- * Time parameters
        starttime,
        stoptime,
        dt,
        time,
        -- * Interpolation and Initial Value
        initD,
        discrete,
        interpolate,
        -- * Fold
        foldD1,
        foldD,
        divideD) where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics

--
-- Integration Parameters and Time
--

-- | Return the start simulation time.
starttime :: Dynamics Double
starttime = Dynamics $ return . spcStartTime . pointSpecs

-- | Return the stop simulation time.
stoptime :: Dynamics Double
stoptime = Dynamics $ return . spcStopTime . pointSpecs

-- | Return the integration time step.
dt :: Dynamics Double
dt = Dynamics $ return . spcDT . pointSpecs

-- | Return the current simulation time.
time :: Dynamics Double
time = Dynamics $ return . pointTime 

--
-- Interpolation and Initial Value
--
-- These functions complement the memoization, possibly except for 
-- the initial function which can be also useful to get an initial 
-- value of any dynamic process. See comments to the Memoization 
-- section.
--      

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

--
-- Fold
--

-- | Like the standard 'foldl1' function but applied to values in 
-- the integration time points. The accumulator values are transformed
-- according to the first argument, which should be either function 
-- 'memo0' or 'umemo0'.
foldD1 :: (Dynamics a -> Dynamics (Dynamics a))
         -> (a -> a -> a) 
         -> Dynamics a 
         -> Dynamics (Dynamics a)
foldD1 tr f (Dynamics m) =
  do r <- liftIO $ newIORef m
     let z = Dynamics $ \p ->
           case pointIteration p of
             0 -> 
               m p
             n -> do 
               let sc = pointSpecs p
                   ty = basicTime sc (n - 1) 0
                   py = p { pointTime = ty, pointIteration = n - 1, pointPhase = 0 }
               y <- readIORef r
               s <- y py
               x <- m p
               return $! f s x
     y@(Dynamics m) <- tr z
     liftIO $ writeIORef r m
     return y

-- | Like the standard 'foldl' function but applied to values in 
-- the integration time points. The accumulator values are transformed
-- according to the first argument, which should be either function
-- 'memo0' or 'umemo0'.
foldD :: (Dynamics a -> Dynamics (Dynamics a))
        -> (a -> b -> a) 
        -> a
        -> Dynamics b 
        -> Dynamics (Dynamics a)
foldD tr f acc (Dynamics m) =
  do r <- liftIO $ newIORef $ const $ return acc
     let z = Dynamics $ \p ->
           case pointIteration p of
             0 -> do
               x <- m p
               return $! f acc x
             n -> do 
               let sc = pointSpecs p
                   ty = basicTime sc (n - 1) 0
                   py = p { pointTime = ty, pointIteration = n - 1, pointPhase = 0 }
               y <- readIORef r
               s <- y py
               x <- m p
               return $! f s x
     y@(Dynamics m) <- tr z
     liftIO $ writeIORef r m
     return y

-- | Divide the values in integration time points by the number of
-- the current iteration. It is useful for statistic functions in
-- combination with the fold. For example, it is used when we calculate
-- the mean value or variance.
divideD :: Dynamics Double -> Dynamics Double
divideD (Dynamics m) = 
  discrete $ Dynamics $ \p ->
  do a <- m p
     return $ a / fromInteger (toInteger (pointIteration p + 1))
