
{-# LANGUAGE BangPatterns, RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.SystemDynamics
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines integrals and other functions of System Dynamics.
--

module Simulation.Aivika.SystemDynamics
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
        integEither,
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
        diffsumEither,
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
import Data.Array.IO.Safe
import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Dynamics.Extra
import Simulation.Aivika.Unboxed
import Simulation.Aivika.Table

import qualified Simulation.Aivika.Dynamics.Memo as M
import qualified Simulation.Aivika.Dynamics.Memo.Unboxed as MU

--
-- Equality and Ordering
--

-- | Compare for equality.
(.==.) :: (Eq a) => Dynamics a -> Dynamics a -> Dynamics Bool
(.==.) = liftM2 (==)

-- | Compare for inequality.
(./=.) :: (Eq a) => Dynamics a -> Dynamics a -> Dynamics Bool
(./=.) = liftM2 (/=)

-- | Compare for ordering.
(.<.) :: (Ord a) => Dynamics a -> Dynamics a -> Dynamics Bool
(.<.) = liftM2 (<)

-- | Compare for ordering.
(.>=.) :: (Ord a) => Dynamics a -> Dynamics a -> Dynamics Bool
(.>=.) = liftM2 (>=)

-- | Compare for ordering.
(.>.) :: (Ord a) => Dynamics a -> Dynamics a -> Dynamics Bool
(.>.) = liftM2 (>)

-- | Compare for ordering.
(.<=.) :: (Ord a) => Dynamics a -> Dynamics a -> Dynamics Bool
(.<=.) = liftM2 (<=)

-- | Return the maximum.
maxDynamics :: (Ord a) => Dynamics a -> Dynamics a -> Dynamics a
maxDynamics = liftM2 max

-- | Return the minimum.
minDynamics :: (Ord a) => Dynamics a -> Dynamics a -> Dynamics a
minDynamics = liftM2 min

-- | Implement the if-then-else operator.
ifDynamics :: Dynamics Bool -> Dynamics a -> Dynamics a -> Dynamics a
ifDynamics cond x y =
  do a <- cond
     if a then x else y

--
-- Ordinary Differential Equations
--

integEuler :: Dynamics Double
             -> Dynamics Double 
             -> Dynamics Double 
             -> Point -> IO Double
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

integRK2 :: Dynamics Double
           -> Dynamics Double
           -> Dynamics Double
           -> Point -> IO Double
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

integRK4 :: Dynamics Double
           -> Dynamics Double
           -> Dynamics Double
           -> Point -> IO Double
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
integ :: Dynamics Double                  -- ^ the derivative
         -> Dynamics Double               -- ^ the initial value
         -> Simulation (Dynamics Double)  -- ^ the integral
integ diff i =
  mdo y <- MU.memoDynamics z
      z <- Simulation $ \r ->
        case spcMethod (runSpecs r) of
          Euler -> return $ Dynamics $ integEuler diff i y
          RungeKutta2 -> return $ Dynamics $ integRK2 diff i y
          RungeKutta4 -> return $ Dynamics $ integRK4 diff i y
      return y

integEulerEither :: Dynamics (Either Double Double)
                    -> Dynamics Double 
                    -> Dynamics Double 
                    -> Point -> IO Double
integEulerEither (Dynamics f) (Dynamics i) (Dynamics y) p = 
  case pointIteration p of
    0 -> 
      i p
    n -> do 
      let sc = pointSpecs p
          ty = basicTime sc (n - 1) 0
          py = p { pointTime = ty, pointIteration = n - 1, pointPhase = 0 }
      b <- f py
      case b of
        Left v ->
          return v
        Right b -> do
          a <- y py
          let !v = a + spcDT (pointSpecs p) * b
          return v

-- | Like 'integ' but allows either setting a new 'Left' integral value,
-- or integrating using the 'Right' derivative directly within computation.
--
-- This function always uses Euler's method.
integEither :: Dynamics (Either Double Double)
               -- ^ either set a new 'Left' integral value, or use a 'Right' derivative
               -> Dynamics Double
               -- ^ the initial value
               -> Simulation (Dynamics Double)
integEither diff i =
  mdo y <- MU.memoDynamics z
      z <- Simulation $ \r ->
        return $ Dynamics $ integEulerEither diff i y
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
smoothI :: Dynamics Double                  -- ^ the value to smooth over time
           -> Dynamics Double               -- ^ time
           -> Dynamics Double               -- ^ the initial value
           -> Simulation (Dynamics Double)  -- ^ the first order exponential smooth
smoothI x t i =
  mdo y <- integ ((x - y) / t) i
      return y

-- | Return the first order exponential smooth.
--
-- This is a simplified version of the 'smoothI' function
-- without specifing the initial value.
smooth :: Dynamics Double                  -- ^ the value to smooth over time
          -> Dynamics Double               -- ^ time
          -> Simulation (Dynamics Double)  -- ^ the first order exponential smooth
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
smooth3I :: Dynamics Double                  -- ^ the value to smooth over time
            -> Dynamics Double               -- ^ time
            -> Dynamics Double               -- ^ the initial value
            -> Simulation (Dynamics Double)  -- ^ the third order exponential smooth
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
smooth3 :: Dynamics Double                  -- ^ the value to smooth over time
           -> Dynamics Double               -- ^ time
           -> Simulation (Dynamics Double)  -- ^ the third order exponential smooth
smooth3 x t = smooth3I x t x

-- | Return the n'th order exponential smooth.
--
-- The result is not discrete in that sense that it may change within the integration time
-- interval depending on the integration method used. Probably, you should apply
-- the 'discreteDynamics' function to the result if you want to achieve an effect when
-- the value is not changed within the time interval, which is used sometimes.
smoothNI :: Dynamics Double                  -- ^ the value to smooth over time
            -> Dynamics Double               -- ^ time
            -> Int                           -- ^ the order
            -> Dynamics Double               -- ^ the initial value
            -> Simulation (Dynamics Double)  -- ^ the n'th order exponential smooth
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
smoothN :: Dynamics Double                  -- ^ the value to smooth over time
           -> Dynamics Double               -- ^ time
           -> Int                           -- ^ the order
           -> Simulation (Dynamics Double)  -- ^ the n'th order exponential smooth
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
delay1I :: Dynamics Double                  -- ^ the value to conserve
           -> Dynamics Double               -- ^ time
           -> Dynamics Double               -- ^ the initial value
           -> Simulation (Dynamics Double)  -- ^ the first order exponential delay
delay1I x t i =
  mdo y <- integ (x - y / t) (i * t)
      return $ y / t

-- | Return the first order exponential delay.
--
-- This is a simplified version of the 'delay1I' function
-- without specifying the initial value.
delay1 :: Dynamics Double                  -- ^ the value to conserve
          -> Dynamics Double               -- ^ time
          -> Simulation (Dynamics Double)  -- ^ the first order exponential delay
delay1 x t = delay1I x t x

-- | Return the third order exponential delay.
delay3I :: Dynamics Double                  -- ^ the value to conserve
           -> Dynamics Double               -- ^ time
           -> Dynamics Double               -- ^ the initial value
           -> Simulation (Dynamics Double)  -- ^ the third order exponential delay
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
delay3 :: Dynamics Double                  -- ^ the value to conserve
          -> Dynamics Double               -- ^ time
          -> Simulation (Dynamics Double)  -- ^ the third order exponential delay
delay3 x t = delay3I x t x

-- | Return the n'th order exponential delay.
delayNI :: Dynamics Double                  -- ^ the value to conserve
           -> Dynamics Double               -- ^ time
           -> Int                           -- ^ the order
           -> Dynamics Double               -- ^ the initial value
           -> Simulation (Dynamics Double)  -- ^ the n'th order exponential delay
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
delayN :: Dynamics Double                  -- ^ the value to conserve
          -> Dynamics Double               -- ^ time
          -> Int                           -- ^ the order
          -> Simulation (Dynamics Double)  -- ^ the n'th order exponential delay
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
forecast :: Dynamics Double                  -- ^ the value to forecast
            -> Dynamics Double               -- ^ the average time
            -> Dynamics Double               -- ^ the time horizon
            -> Simulation (Dynamics Double)  -- ^ the forecast
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
trend :: Dynamics Double                  -- ^ the value for which the trend is calculated
         -> Dynamics Double               -- ^ the average time
         -> Dynamics Double               -- ^ the initial value
         -> Simulation (Dynamics Double)  -- ^ the fractional change rate
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
diffsum :: (Num a, Unboxed a)
           => Dynamics a               -- ^ the difference
           -> Dynamics a               -- ^ the initial value
           -> Simulation (Dynamics a)  -- ^ the sum
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

-- | Like 'diffsum' but allows either setting a new 'Left' sum value, or adding the 'Right' difference.
diffsumEither :: (Num a, Unboxed a)
                 => Dynamics (Either a a)
                 -- ^ either set the 'Left' value for the sum, or add the 'Right' difference to the sum
                 -> Dynamics a
                 -- ^ the initial value
                 -> Simulation (Dynamics a)
                 -- ^ the sum
diffsumEither (Dynamics diff) (Dynamics i) =
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
            b <- diff py
            case b of
              Left v ->
                return v
              Right b -> do
                a <- m py
                let !v = a + b
                return v
      return y

--
-- Table Functions
--

-- | Lookup @x@ in a table of pairs @(x, y)@ using linear interpolation.
lookupDynamics :: Dynamics Double -> Array Int (Double, Double) -> Dynamics Double
lookupDynamics (Dynamics m) tbl =
  Dynamics $ \p ->
  do a <- m p
     return $ tableLookup a tbl

-- | Lookup @x@ in a table of pairs @(x, y)@ using stepwise function.
lookupStepwiseDynamics :: Dynamics Double -> Array Int (Double, Double) -> Dynamics Double
lookupStepwiseDynamics (Dynamics m) tbl =
  Dynamics $ \p ->
  do a <- m p
     return $ tableLookupStepwise a tbl

--
-- Discrete Functions
--

-- | Return the delayed value using the specified lag time.
delay :: Dynamics a          -- ^ the value to delay
         -> Dynamics Double  -- ^ the lag time
         -> Dynamics a       -- ^ the delayed value
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
delayI :: Dynamics a          -- ^ the value to delay
          -> Dynamics Double  -- ^ the lag time
          -> Dynamics a       -- ^ the initial value
          -> Simulation (Dynamics a)    -- ^ the delayed value
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
npv :: Dynamics Double                  -- ^ the stream
       -> Dynamics Double               -- ^ the discount rate
       -> Dynamics Double               -- ^ the initial value
       -> Dynamics Double               -- ^ factor
       -> Simulation (Dynamics Double)  -- ^ the Net Present Value (NPV)
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
npve :: Dynamics Double                  -- ^ the stream
        -> Dynamics Double               -- ^ the discount rate
        -> Dynamics Double               -- ^ the initial value
        -> Dynamics Double               -- ^ factor
        -> Simulation (Dynamics Double)  -- ^ the Net Present Value End (NPVE)
npve stream rate init factor =
  mdo let dt' = liftParameter dt
      df <- integ (- df * rate / (1 + rate * dt')) (1 / (1 + rate * dt'))
      accum <- integ (stream * df) init
      return $ (accum + dt' * stream * df) * factor

-- | Computation that returns 0 until the step time and then returns the specified height.
step :: Dynamics Double
        -- ^ the height
        -> Dynamics Double
        -- ^ the step time
        -> Dynamics Double
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
pulse :: Dynamics Double
         -- ^ the time start
         -> Dynamics Double
         -- ^ the interval width
         -> Dynamics Double
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
pulseP :: Dynamics Double
          -- ^ the time start
          -> Dynamics Double
          -- ^ the interval width
          -> Dynamics Double
          -- ^ the time period
          -> Dynamics Double
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
ramp :: Dynamics Double
        -- ^ the slope parameter
        -> Dynamics Double
        -- ^ the time start
        -> Dynamics Double
        -- ^ the end time
        -> Dynamics Double
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
        
