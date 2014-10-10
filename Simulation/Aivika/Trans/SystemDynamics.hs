
{-# LANGUAGE BangPatterns, RecursiveDo, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.SystemDynamics
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines integrals and other functions of System Dynamics.
--

module Simulation.Aivika.Trans.SystemDynamics
       (-- * Equality and Ordering
        (.==.),
        (./=.),
        (.<.),
        (.>=.),
        (.>.),
        (.<=.),
        maxDynamics,
        minDynamics,
        ifDynamics,
        -- * Ordinary Differential Equations
        integ,
        smoothI,
        smooth,
        smooth3I,
        smooth3,
        smoothNI,
        smoothN,
        delay1I,
        delay1,
        delay3I,
        delay3,
        delayNI,
        delayN,
        forecast,
        trend,
        -- * Difference Equations
        diffsum,
        -- * Table Functions
        lookupDynamics,
        lookupStepwiseDynamics,
        -- * Discrete Functions
        delay,
        delayI,
        step,
        pulse,
        pulseP,
        ramp,
        -- * Financial Functions
        npv,
        npve) where

import Data.Array

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix

import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Dynamics.Interpolate
import Simulation.Aivika.Trans.MonadSim
import Simulation.Aivika.Trans.Unboxed
import Simulation.Aivika.Table

import qualified Simulation.Aivika.Trans.Dynamics.Memo as M
import qualified Simulation.Aivika.Trans.Dynamics.Memo.Unboxed as MU

--
-- Equality and Ordering
--

-- | Compare for equality.
(.==.) :: (MonadSim m, Eq a) => DynamicsT m a -> DynamicsT m a -> DynamicsT m Bool
{-# INLINE (.==.) #-}
(.==.) = liftM2 (==)

-- | Compare for inequality.
(./=.) :: (MonadSim m, Eq a) => DynamicsT m a -> DynamicsT m a -> DynamicsT m Bool
{-# INLINE (./=.) #-}
(./=.) = liftM2 (/=)

-- | Compare for ordering.
(.<.) :: (MonadSim m, Ord a) => DynamicsT m a -> DynamicsT m a -> DynamicsT m Bool
{-# INLINE (.<.) #-}
(.<.) = liftM2 (<)

-- | Compare for ordering.
(.>=.) :: (MonadSim m, Ord a) => DynamicsT m a -> DynamicsT m a -> DynamicsT m Bool
{-# INLINE (.>=.) #-}
(.>=.) = liftM2 (>=)

-- | Compare for ordering.
(.>.) :: (MonadSim m, Ord a) => DynamicsT m a -> DynamicsT m a -> DynamicsT m Bool
{-# INLINE (.>.) #-}
(.>.) = liftM2 (>)

-- | Compare for ordering.
(.<=.) :: (MonadSim m, Ord a) => DynamicsT m a -> DynamicsT m a -> DynamicsT m Bool
{-# INLINE (.<=.) #-}
(.<=.) = liftM2 (<=)

-- | Return the maximum.
maxDynamics :: (MonadSim m, Ord a) => DynamicsT m a -> DynamicsT m a -> DynamicsT m a
{-# INLINE maxDynamics #-}
maxDynamics = liftM2 max

-- | Return the minimum.
minDynamics :: (MonadSim m, Ord a) => DynamicsT m a -> DynamicsT m a -> DynamicsT m a
{-# INLINE minDynamics #-}
minDynamics = liftM2 min

-- | Implement the if-then-else operator.
ifDynamics :: MonadSim m => DynamicsT m Bool -> DynamicsT m a -> DynamicsT m a -> DynamicsT m a
{-# INLINE ifDynamics #-}
ifDynamics cond x y =
  do a <- cond
     if a then x else y

--
-- Ordinary Differential Equations
--

integEuler :: MonadSim m
              => DynamicsT m Double
              -> DynamicsT m Double 
              -> DynamicsT m Double 
              -> PointT m
              -> m Double
{-# INLINABLE integEuler #-}
integEuler (Dynamics f) (Dynamics i) (Dynamics y) p = 
  case pointIteration p of
    0 -> 
      i p
    n -> do 
      let sc = pointSpecs p
          ty = basicTime sc (n - 1) 0
          py = p { pointTime = ty, pointIteration = n - 1, pointPhase = 0 }
      a <- y py
      b <- f py
      let !v = a + spcDT (pointSpecs p) * b
      return v

integRK2 :: MonadSim m
            => DynamicsT m Double
            -> DynamicsT m Double
            -> DynamicsT m Double
            -> PointT m
            -> m Double
{-# INLINABLE integRK2 #-}
integRK2 (Dynamics f) (Dynamics i) (Dynamics y) p =
  case pointPhase p of
    0 -> case pointIteration p of
      0 ->
        i p
      n -> do
        let sc = pointSpecs p
            ty = basicTime sc (n - 1) 0
            t1 = ty
            t2 = basicTime sc (n - 1) 1
            py = p { pointTime = ty, pointIteration = n - 1, pointPhase = 0 }
            p1 = py
            p2 = p { pointTime = t2, pointIteration = n - 1, pointPhase = 1 }
        vy <- y py
        k1 <- f p1
        k2 <- f p2
        let !v = vy + spcDT sc / 2.0 * (k1 + k2)
        return v
    1 -> do
      let sc = pointSpecs p
          n  = pointIteration p
          ty = basicTime sc n 0
          t1 = ty
          py = p { pointTime = ty, pointIteration = n, pointPhase = 0 }
          p1 = py
      vy <- y py
      k1 <- f p1
      let !v = vy + spcDT sc * k1
      return v
    _ -> 
      error "Incorrect phase: integRK2"

integRK4 :: MonadSim m
            => DynamicsT m Double
            -> DynamicsT m Double
            -> DynamicsT m Double
            -> PointT m
            -> m Double
{-# INLINABLE integRK4 #-}
integRK4 (Dynamics f) (Dynamics i) (Dynamics y) p =
  case pointPhase p of
    0 -> case pointIteration p of
      0 -> 
        i p
      n -> do
        let sc = pointSpecs p
            ty = basicTime sc (n - 1) 0
            t1 = ty
            t2 = basicTime sc (n - 1) 1
            t3 = basicTime sc (n - 1) 2
            t4 = basicTime sc (n - 1) 3
            py = p { pointTime = ty, pointIteration = n - 1, pointPhase = 0 }
            p1 = py
            p2 = p { pointTime = t2, pointIteration = n - 1, pointPhase = 1 }
            p3 = p { pointTime = t3, pointIteration = n - 1, pointPhase = 2 }
            p4 = p { pointTime = t4, pointIteration = n - 1, pointPhase = 3 }
        vy <- y py
        k1 <- f p1
        k2 <- f p2
        k3 <- f p3
        k4 <- f p4
        let !v = vy + spcDT sc / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
        return v
    1 -> do
      let sc = pointSpecs p
          n  = pointIteration p
          ty = basicTime sc n 0
          t1 = ty
          py = p { pointTime = ty, pointIteration = n, pointPhase = 0 }
          p1 = py
      vy <- y py
      k1 <- f p1
      let !v = vy + spcDT sc / 2.0 * k1
      return v
    2 -> do
      let sc = pointSpecs p
          n  = pointIteration p
          ty = basicTime sc n 0
          t2 = basicTime sc n 1
          py = p { pointTime = ty, pointIteration = n, pointPhase = 0 }
          p2 = p { pointTime = t2, pointIteration = n, pointPhase = 1 }
      vy <- y py
      k2 <- f p2
      let !v = vy + spcDT sc / 2.0 * k2
      return v
    3 -> do
      let sc = pointSpecs p
          n  = pointIteration p
          ty = basicTime sc n 0
          t3 = basicTime sc n 2
          py = p { pointTime = ty, pointIteration = n, pointPhase = 0 }
          p3 = p { pointTime = t3, pointIteration = n, pointPhase = 2 }
      vy <- y py
      k3 <- f p3
      let !v = vy + spcDT sc * k3
      return v
    _ -> 
      error "Incorrect phase: integRK4"

-- | Return an integral with the specified derivative and initial value.
--
-- To create a loopback, you should use the recursive do-notation.
-- It allows defining the differential equations unordered as
-- in mathematics:
--
-- @
-- model :: Simulation [Double]
-- model = 
--   mdo a <- integ (- ka * a) 100
--       b <- integ (ka * a - kb * b) 0
--       c <- integ (kb * b) 0
--       let ka = 1
--           kb = 1
--       runDynamicsInStopTime $ sequence [a, b, c]
-- @
integ :: (MonadSim m, MonadFix m)
         => DynamicsT m Double                  -- ^ the derivative
         -> DynamicsT m Double                  -- ^ the initial value
         -> SimulationT m (DynamicsT m Double)  -- ^ the integral
{-# INLINABLE integ #-}
integ diff i =
  mdo y <- MU.memoDynamics z
      z <- Simulation $ \r ->
        case spcMethod (runSpecs r) of
          Euler -> return $ Dynamics $ integEuler diff i y
          RungeKutta2 -> return $ Dynamics $ integRK2 diff i y
          RungeKutta4 -> return $ Dynamics $ integRK4 diff i y
      return y

-- | Return the first order exponential smooth.
--
-- To create a loopback, you should use the recursive do-notation
-- with help of which the function itself is defined:
--
-- @
-- smoothI x t i =
--   mdo y <- integ ((x - y) \/ t) i
--       return y
-- @     
smoothI :: (MonadSim m, MonadFix m)
           => DynamicsT m Double                  -- ^ the value to smooth over time
           -> DynamicsT m Double                  -- ^ time
           -> DynamicsT m Double                  -- ^ the initial value
           -> SimulationT m (DynamicsT m Double)  -- ^ the first order exponential smooth
{-# INLINABLE smoothI #-}
smoothI x t i =
  mdo y <- integ ((x - y) / t) i
      return y

-- | Return the first order exponential smooth.
--
-- This is a simplified version of the 'smoothI' function
-- without specifing the initial value.
smooth :: (MonadSim m, MonadFix m)
          => DynamicsT m Double                  -- ^ the value to smooth over time
          -> DynamicsT m Double                  -- ^ time
          -> SimulationT m (DynamicsT m Double)  -- ^ the first order exponential smooth
{-# INLINABLE smooth #-}
smooth x t = smoothI x t x

-- | Return the third order exponential smooth.
--
-- To create a loopback, you should use the recursive do-notation
-- with help of which the function itself is defined:
--
-- @
-- smooth3I x t i =
--   mdo y  <- integ ((s2 - y) \/ t') i
--       s2 <- integ ((s1 - s2) \/ t') i
--       s1 <- integ ((x - s1) \/ t') i
--       let t' = t \/ 3.0
--       return y
-- @     
smooth3I :: (MonadSim m, MonadFix m)
            => DynamicsT m Double                  -- ^ the value to smooth over time
            -> DynamicsT m Double                  -- ^ time
            -> DynamicsT m Double                  -- ^ the initial value
            -> SimulationT m (DynamicsT m Double)  -- ^ the third order exponential smooth
{-# INLINABLE smooth3I #-}
smooth3I x t i =
  mdo y  <- integ ((s2 - y) / t') i
      s2 <- integ ((s1 - s2) / t') i
      s1 <- integ ((x - s1) / t') i
      let t' = t / 3.0
      return y

-- | Return the third order exponential smooth.
-- 
-- This is a simplified version of the 'smooth3I' function
-- without specifying the initial value.
smooth3 :: (MonadSim m, MonadFix m)
           => DynamicsT m Double                  -- ^ the value to smooth over time
           -> DynamicsT m Double                  -- ^ time
           -> SimulationT m (DynamicsT m Double)  -- ^ the third order exponential smooth
{-# INLINABLE smooth3 #-}
smooth3 x t = smooth3I x t x

-- | Return the n'th order exponential smooth.
--
-- The result is not discrete in that sense that it may change within the integration time
-- interval depending on the integration method used. Probably, you should apply
-- the 'discreteDynamics' function to the result if you want to achieve an effect when
-- the value is not changed within the time interval, which is used sometimes.
smoothNI :: (MonadSim m, MonadFix m)
            => DynamicsT m Double                  -- ^ the value to smooth over time
            -> DynamicsT m Double                  -- ^ time
            -> Int                                 -- ^ the order
            -> DynamicsT m Double                  -- ^ the initial value
            -> SimulationT m (DynamicsT m Double)  -- ^ the n'th order exponential smooth
{-# INLINABLE smoothNI #-}
smoothNI x t n i =
  mdo s <- forM [1 .. n] $ \k ->
        if k == 1
        then integ ((x - a ! 1) / t') i
        else integ ((a ! (k - 1) - a ! k) / t') i
      let a  = listArray (1, n) s 
          t' = t / fromIntegral n
      return $ a ! n

-- | Return the n'th order exponential smooth.
--
-- This is a simplified version of the 'smoothNI' function
-- without specifying the initial value.
smoothN :: (MonadSim m, MonadFix m)
           => DynamicsT m Double                  -- ^ the value to smooth over time
           -> DynamicsT m Double                  -- ^ time
           -> Int                                 -- ^ the order
           -> SimulationT m (DynamicsT m Double)  -- ^ the n'th order exponential smooth
{-# INLINABLE smoothN #-}
smoothN x t n = smoothNI x t n x

-- | Return the first order exponential delay.
--
-- To create a loopback, you should use the recursive do-notation
-- with help of which the function itself is defined:
--
-- @
-- delay1I x t i =
--   mdo y <- integ (x - y \/ t) (i * t)
--       return $ y \/ t
-- @     
delay1I :: (MonadSim m, MonadFix m)
           => DynamicsT m Double                  -- ^ the value to conserve
           -> DynamicsT m Double                  -- ^ time
           -> DynamicsT m Double                  -- ^ the initial value
           -> SimulationT m (DynamicsT m Double)  -- ^ the first order exponential delay
{-# INLINABLE delay1I #-}
delay1I x t i =
  mdo y <- integ (x - y / t) (i * t)
      return $ y / t

-- | Return the first order exponential delay.
--
-- This is a simplified version of the 'delay1I' function
-- without specifying the initial value.
delay1 :: (MonadSim m, MonadFix m)
          => DynamicsT m Double                  -- ^ the value to conserve
          -> DynamicsT m Double                  -- ^ time
          -> SimulationT m (DynamicsT m Double)  -- ^ the first order exponential delay
{-# INLINABLE delay1 #-}
delay1 x t = delay1I x t x

-- | Return the third order exponential delay.
delay3I :: (MonadSim m, MonadFix m)
           => DynamicsT m Double                  -- ^ the value to conserve
           -> DynamicsT m Double                  -- ^ time
           -> DynamicsT m Double                  -- ^ the initial value
           -> SimulationT m (DynamicsT m Double)  -- ^ the third order exponential delay
{-# INLINABLE delay3I #-}
delay3I x t i =
  mdo y  <- integ (s2 / t' - y / t') (i * t')
      s2 <- integ (s1 / t' - s2 / t') (i * t')
      s1 <- integ (x - s1 / t') (i * t')
      let t' = t / 3.0
      return $ y / t'         

-- | Return the third order exponential delay.
--
-- This is a simplified version of the 'delay3I' function
-- without specifying the initial value.
delay3 :: (MonadSim m, MonadFix m)
          => DynamicsT m Double                  -- ^ the value to conserve
          -> DynamicsT m Double                  -- ^ time
          -> SimulationT m (DynamicsT m Double)  -- ^ the third order exponential delay
{-# INLINABLE delay3 #-}
delay3 x t = delay3I x t x

-- | Return the n'th order exponential delay.
delayNI :: (MonadSim m, MonadFix m)
           => DynamicsT m Double                  -- ^ the value to conserve
           -> DynamicsT m Double                  -- ^ time
           -> Int                                 -- ^ the order
           -> DynamicsT m Double                  -- ^ the initial value
           -> SimulationT m (DynamicsT m Double)  -- ^ the n'th order exponential delay
{-# INLINABLE delayNI #-}
delayNI x t n i =
  mdo s <- forM [1 .. n] $ \k ->
        if k == 1
        then integ (x - (a ! 1) / t') (i * t')
        else integ ((a ! (k - 1)) / t' - (a ! k) / t') (i * t')
      let a  = listArray (1, n) s
          t' = t / fromIntegral n
      return $ (a ! n) / t'

-- | Return the n'th order exponential delay.
--
-- This is a simplified version of the 'delayNI' function
-- without specifying the initial value.
delayN :: (MonadSim m, MonadFix m)
          => DynamicsT m Double                  -- ^ the value to conserve
          -> DynamicsT m Double                  -- ^ time
          -> Int                                 -- ^ the order
          -> SimulationT m (DynamicsT m Double)  -- ^ the n'th order exponential delay
{-# INLINABLE delayN #-}
delayN x t n = delayNI x t n x

-- | Return the forecast.
--
-- The function has the following definition:
--
-- @
-- forecast x at hz =
--   do y <- smooth x at
--      return $ x * (1.0 + (x \/ y - 1.0) \/ at * hz)
-- @
forecast :: (MonadSim m, MonadFix m)
            => DynamicsT m Double                  -- ^ the value to forecast
            -> DynamicsT m Double                  -- ^ the average time
            -> DynamicsT m Double                  -- ^ the time horizon
            -> SimulationT m (DynamicsT m Double)  -- ^ the forecast
{-# INLINABLE forecast #-}
forecast x at hz =
  do y <- smooth x at
     return $ x * (1.0 + (x / y - 1.0) / at * hz)

-- | Return the trend.
--
-- The function has the following definition:
--
-- @
-- trend x at i =
--   do y <- smoothI x at (x \/ (1.0 + i * at))
--      return $ (x \/ y - 1.0) \/ at
-- @
trend :: (MonadSim m, MonadFix m)
         => DynamicsT m Double                  -- ^ the value for which the trend is calculated
         -> DynamicsT m Double                  -- ^ the average time
         -> DynamicsT m Double                  -- ^ the initial value
         -> SimulationT m (DynamicsT m Double)  -- ^ the fractional change rate
{-# INLINABLE trend #-}
trend x at i =
  do y <- smoothI x at (x / (1.0 + i * at))
     return $ (x / y - 1.0) / at

--
-- Difference Equations
--

-- | Retun the sum for the difference equation.
-- It is like an integral returned by the 'integ' function, only now
-- the difference is used instead of derivative.
--
-- As usual, to create a loopback, you should use the recursive do-notation.
diffsum :: (MonadSim m, MonadFix m,
            Unboxed m a, Num a)
           => DynamicsT m a                  -- ^ the difference
           -> DynamicsT m a                  -- ^ the initial value
           -> SimulationT m (DynamicsT m a)  -- ^ the sum
{-# INLINABLE diffsum #-}
diffsum (Dynamics diff) (Dynamics i) =
  mdo y <-
        MU.memo0Dynamics $
        Dynamics $ \p ->
        case pointIteration p of
          0 -> i p
          n -> do 
            let Dynamics m = y
                sc = pointSpecs p
                ty = basicTime sc (n - 1) 0
                py = p { pointTime = ty, 
                         pointIteration = n - 1, 
                         pointPhase = 0 }
            a <- m py
            b <- diff py
            let !v = a + b
            return v
      return y

--
-- Table Functions
--

-- | Lookup @x@ in a table of pairs @(x, y)@ using linear interpolation.
lookupDynamics :: MonadSim m => DynamicsT m Double -> Array Int (Double, Double) -> DynamicsT m Double
{-# INLINABLE lookupDynamics #-}
lookupDynamics (Dynamics m) tbl =
  Dynamics $ \p ->
  do a <- m p
     return $ tableLookup a tbl

-- | Lookup @x@ in a table of pairs @(x, y)@ using stepwise function.
lookupStepwiseDynamics :: MonadSim m => DynamicsT m Double -> Array Int (Double, Double) -> DynamicsT m Double
{-# INLINABLE lookupStepwiseDynamics #-}
lookupStepwiseDynamics (Dynamics m) tbl =
  Dynamics $ \p ->
  do a <- m p
     return $ tableLookupStepwise a tbl

--
-- Discrete Functions
--

-- | Return the delayed value using the specified lag time.
delay :: MonadSim m
         => DynamicsT m a          -- ^ the value to delay
         -> DynamicsT m Double     -- ^ the lag time
         -> DynamicsT m a          -- ^ the delayed value
{-# INLINABLE delay #-}
delay (Dynamics x) (Dynamics d) = discreteDynamics $ Dynamics r 
  where
    r p = do 
      let t  = pointTime p
          sc = pointSpecs p
          n  = pointIteration p
      a <- d p
      let t' = t - a
          n' = fromIntegral $ floor $ (t' - spcStartTime sc) / spcDT sc
          y | n' < 0    = x $ p { pointTime = spcStartTime sc,
                                  pointIteration = 0, 
                                  pointPhase = 0 }
            | n' < n    = x $ p { pointTime = t',
                                  pointIteration = n',
                                  pointPhase = -1 }
            | n' > n    = error $
                          "Cannot return the future data: delay. " ++
                          "The lag time cannot be negative."
            | otherwise = error $
                          "Cannot return the current data: delay. " ++
                          "The lag time is too small."
      y

-- | Return the delayed value using the specified lag time and initial value.
-- Because of the latter, it allows creating a loop back.
delayI :: MonadSim m
          => DynamicsT m a                    -- ^ the value to delay
          -> DynamicsT m Double               -- ^ the lag time
          -> DynamicsT m a                    -- ^ the initial value
          -> SimulationT m (DynamicsT m a)    -- ^ the delayed value
{-# INLINABLE delayI #-}
delayI (Dynamics x) (Dynamics d) (Dynamics i) = M.memo0Dynamics $ Dynamics r 
  where
    r p = do 
      let t  = pointTime p
          sc = pointSpecs p
          n  = pointIteration p
      a <- d p
      let t' = t - a
          n' = fromIntegral $ floor $ (t' - spcStartTime sc) / spcDT sc
          y | n' < 0    = i $ p { pointTime = spcStartTime sc,
                                  pointIteration = 0, 
                                  pointPhase = 0 }
            | n' < n    = x $ p { pointTime = t',
                                  pointIteration = n',
                                  pointPhase = -1 }
            | n' > n    = error $
                          "Cannot return the future data: delay. " ++
                          "The lag time cannot be negative."
            | otherwise = error $
                          "Cannot return the current data: delay. " ++
                          "The lag time is too small."
      y

--
-- Financial Functions
--

-- | Return the Net Present Value (NPV) of the stream computed using the specified
-- discount rate, the initial value and some factor (usually 1).
--
-- It is defined in the following way:
--
-- @
-- npv stream rate init factor =
--   mdo let dt' = liftParameter dt
--       df <- integ (- df * rate) 1
--       accum <- integ (stream * df) init
--       return $ (accum + dt' * stream * df) * factor
-- @
npv :: (MonadSim m, MonadFix m)
       => DynamicsT m Double                  -- ^ the stream
       -> DynamicsT m  Double                 -- ^ the discount rate
       -> DynamicsT m Double                  -- ^ the initial value
       -> DynamicsT m Double                  -- ^ factor
       -> SimulationT m (DynamicsT m Double)  -- ^ the Net Present Value (NPV)
{-# INLINABLE npv #-}
npv stream rate init factor =
  mdo let dt' = liftParameter dt
      df <- integ (- df * rate) 1
      accum <- integ (stream * df) init
      return $ (accum + dt' * stream * df) * factor

-- | Return the Net Present Value End of period (NPVE) of the stream computed
-- using the specified discount rate, the initial value and some factor.
--
-- It is defined in the following way:
--
-- @
-- npve stream rate init factor =
--   mdo let dt' = liftParameter dt
--       df <- integ (- df * rate \/ (1 + rate * dt')) (1 \/ (1 + rate * dt'))
--       accum <- integ (stream * df) init
--       return $ (accum + dt' * stream * df) * factor
-- @
npve :: (MonadSim m, MonadFix m)
        => DynamicsT m Double                  -- ^ the stream
        -> DynamicsT m Double                  -- ^ the discount rate
        -> DynamicsT m Double                  -- ^ the initial value
        -> DynamicsT m Double                  -- ^ factor
        -> SimulationT m (DynamicsT m Double)  -- ^ the Net Present Value End (NPVE)
{-# INLINABLE npve #-}
npve stream rate init factor =
  mdo let dt' = liftParameter dt
      df <- integ (- df * rate / (1 + rate * dt')) (1 / (1 + rate * dt'))
      accum <- integ (stream * df) init
      return $ (accum + dt' * stream * df) * factor

-- | Computation that returns 0 until the step time and then returns the specified height.
step :: MonadSim m
        => DynamicsT m Double
        -- ^ the height
        -> DynamicsT m Double
        -- ^ the step time
        -> DynamicsT m Double
{-# INLINABLE step #-}
step h st =
  discreteDynamics $
  Dynamics $ \p ->
  do let sc = pointSpecs p
         t  = pointTime p
     st' <- invokeDynamics p st
     let t' = t + spcDT sc / 2
     if st' < t'
       then invokeDynamics p h
       else return 0

-- | Computation that returns 1, starting at the time start, and lasting for the interval
-- width; 0 is returned at all other times.
pulse :: MonadSim m
         => DynamicsT m Double
         -- ^ the time start
         -> DynamicsT m Double
         -- ^ the interval width
         -> DynamicsT m Double
{-# INLINABLE pulse #-}
pulse st w =
  discreteDynamics $
  Dynamics $ \p ->
  do let sc = pointSpecs p
         t  = pointTime p
     st' <- invokeDynamics p st
     let t' = t + spcDT sc / 2
     if st' < t'
       then do w' <- invokeDynamics p w
               return $ if t' < st' + w' then 1 else 0
       else return 0

-- | Computation that returns 1, starting at the time start, and lasting for the interval
-- width and then repeats this pattern with the specified period; 0 is returned at all
-- other times.
pulseP :: MonadSim m
          => DynamicsT m Double
          -- ^ the time start
          -> DynamicsT m Double
          -- ^ the interval width
          -> DynamicsT m Double
          -- ^ the time period
          -> DynamicsT m Double
{-# INLINABLE pulseP #-}
pulseP st w period =
  discreteDynamics $
  Dynamics $ \p ->
  do let sc = pointSpecs p
         t  = pointTime p
     p'  <- invokeDynamics p period
     st' <- invokeDynamics p st
     let y' = if (p' > 0) && (t > st')
              then fromIntegral (floor $ (t - st') / p') * p'
              else 0
     let st' = st' + y'
     let t' = t + spcDT sc / 2
     if st' < t'
       then do w' <- invokeDynamics p w
               return $ if t' < st' + w' then 1 else 0
       else return 0

-- | Computation that returns 0 until the specified time start and then
-- slopes upward until the end time and then holds constant.
ramp :: MonadSim m
        => DynamicsT m Double
        -- ^ the slope parameter
        -> DynamicsT m Double
        -- ^ the time start
        -> DynamicsT m Double
        -- ^ the end time
        -> DynamicsT m Double
{-# INLINABLE ramp #-}
ramp slope st e =
  discreteDynamics $
  Dynamics $ \p ->
  do let sc = pointSpecs p
         t  = pointTime p
     st' <- invokeDynamics p st
     if st' < t
       then do slope' <- invokeDynamics p slope
               e' <- invokeDynamics p e
               if t < e'
                 then return $ slope' * (t - st')
                 else return $ slope' * (e' - st')
       else return 0
        
