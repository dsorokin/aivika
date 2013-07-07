
{-# LANGUAGE FlexibleContexts, BangPatterns, RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Dynamics.SystemDynamics
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines integrals and other functions of System Dynamics.
--

module Simulation.Aivika.Dynamics.SystemDynamics
       (-- * Maximum and Minimum
        maxDynamics,
        minDynamics,
        -- * Integrals
        Integ,
        newInteg,
        integInit,
        integValue,
        integDiff,
        -- * Integral Functions
        integ,
        smoothI,
        smooth,
        smooth3I,
        smooth3,
        smoothNI,
        smoothN,
        -- * Difference Equations
        Sum,
        newSum,
        sumInit,
        sumValue,
        sumDiff,
        -- * Table Functions
        lookupD,
        lookupStepwiseD) where

import Data.Array
import Data.Array.IO.Safe
import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Base

--
-- Maximum and Minimum
--

-- | Return the maximum.
maxDynamics :: (Ord a) => Dynamics a -> Dynamics a -> Dynamics a
maxDynamics = liftM2 max

-- | Return the minimum.
minDynamics :: (Ord a) => Dynamics a -> Dynamics a -> Dynamics a
minDynamics = liftM2 min

--
-- Integrals
--

-- | The 'Integ' type represents an integral.
data Integ = Integ { integInit     :: Dynamics Double,   -- ^ The initial value.
                     integExternal :: IORef (Dynamics Double),
                     integInternal :: IORef (Dynamics Double) }

-- | Create a new integral with the specified initial value.
newInteg :: Dynamics Double -> Simulation Integ
newInteg i = 
  do r1 <- liftIO $ newIORef $ initDynamics i 
     r2 <- liftIO $ newIORef $ initDynamics i 
     let integ = Integ { integInit     = i, 
                         integExternal = r1,
                         integInternal = r2 }
         z = Dynamics $ \p -> 
           do (Dynamics m) <- readIORef (integInternal integ)
              m p
     y <- umemo z
     liftIO $ writeIORef (integExternal integ) y
     return integ

-- | Return the integral's value.
integValue :: Integ -> Dynamics Double
integValue integ = 
  Dynamics $ \p ->
  do (Dynamics m) <- readIORef (integExternal integ)
     m p

-- | Set the derivative for the integral.
integDiff :: Integ -> Dynamics Double -> Simulation ()
integDiff integ diff =
  do let z = Dynamics $ \p ->
           do y <- readIORef (integExternal integ)
              let i = integInit integ
              case spcMethod (pointSpecs p) of
                Euler -> integEuler diff i y p
                RungeKutta2 -> integRK2 diff i y p
                RungeKutta4 -> integRK4 diff i y p
     liftIO $ writeIORef (integInternal integ) z

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
-- If you want to create a loopback then you should use either the 'Integ' type
-- or the recursive do-notation. Using the latter gives a more short, simple, fast
-- and intuitive code as allows defining the differential equations unordered as
-- in mathematics:
--
-- @
-- model :: Simulation [Double]
-- model = 
--   do rec a <- integ (- ka * a) 100
--          b <- integ (ka * a - kb * b) 0
--          c <- integ (kb * b) 0
--          let ka = 1
--              kb = 1
--      runDynamicsInStopTime $ sequence [a, b, c]
-- @
integ :: Dynamics Double -> Dynamics Double -> Simulation (Dynamics Double)
integ diff i =
  do rec y <- umemo z
         z <- Simulation $ \r ->
           case spcMethod (runSpecs r) of
             Euler -> return $ Dynamics $ integEuler diff i y
             RungeKutta2 -> return $ Dynamics $ integRK2 diff i y
             RungeKutta4 -> return $ Dynamics $ integRK4 diff i y
     return y

-- | Return the first order exponential smooth of the first argument
-- over the second one starting at the last argument.
--
-- To create a loopback, you should use the recursive do-notation
-- with help of which the function itself is defined:
--
-- @
-- smoothI x t i =
--   do rec y <- integ ((x - y) / t) i
--      return y
-- @     
smoothI :: Dynamics Double -> Dynamics Double -> Dynamics Double
           -> Simulation (Dynamics Double)
smoothI x t i =
  do rec y <- integ ((x - y) / t) i
     return y

-- | Return the first order exponential smooth of the first argument
-- over the second one. This is a simplified version of the 'smoothI'
-- function.
smooth :: Dynamics Double -> Dynamics Double -> Simulation (Dynamics Double)
smooth x t = smoothI x t x

-- | Return the third order exponential smooth of the first argument
-- over the second one starting at the last argument.
--
-- To create a loopback, you should use the recursive do-notation
-- with help of which the function itself is defined:
--
-- @
-- smooth3I x t i =
--   do rec y <- integ ((s2 - y) / t') i
--          s2 <- integ ((s1 - s2) / t') i
--          s1 <- integ ((x - s1) / t') i
--          let t' = t / 3.0
--      return y
-- @     
smooth3I :: Dynamics Double -> Dynamics Double -> Dynamics Double
            -> Simulation (Dynamics Double)
smooth3I x t i =
  do rec y <- integ ((s2 - y) / t') i
         s2 <- integ ((s1 - s2) / t') i
         s1 <- integ ((x - s1) / t') i
         let t' = t / 3.0
     return y

-- | Return the third order exponential smooth of the first argument
-- over the second one. This is a simplified version of the 'smooth3I'
-- function.
smooth3 :: Dynamics Double -> Dynamics Double -> Simulation (Dynamics Double)
smooth3 x t = smooth3I x t x

-- | Return the n'th order exponential smooth of the first argument
-- over the second one starting at the last argument.
smoothNI :: Dynamics Double -> Dynamics Double -> Int -> Dynamics Double 
            -> Simulation (Dynamics Double)
smoothNI x t n i =
  do rec s <- forM [1 .. n] $ \k ->
           if k == 1
           then integ ((x - s !! 1) / t') i
           else integ ((s !! (k - 1) - s !! k) / t') i
         let t' = t / fromIntegral n
     return $ s !! n

-- | Return the n'th order exponential smooth of the first argument
-- over the second one. This is a simplified version of the 'smoothNI'
-- function.
smoothN :: Dynamics Double -> Dynamics Double -> Int
           -> Simulation (Dynamics Double)
smoothN x t n = smoothNI x t n x

-- delay1I :: Dynamics Double -> Dynamics Double -> Dynamics Double 
--           -> Dynamics Double
-- delay1I x t i = y where
--   y = integ (x - y) (i * t) / t

-- delay1 :: Dynamics Double -> Dynamics Double -> Dynamics Double
-- delay1 x t = delay1I x t x

-- delay3I :: Dynamics Double -> Dynamics Double -> Dynamics Double 
--           -> Dynamics Double
-- delay3I x t i = y where
--   y  = integ (s1 - y) (i * t') / t'
--   s1 = integ (s0 - s1) (i * t') / t'
--   s0 = integ (x - s0) (i * t') / t'
--   t' = t / 3.0

-- delay3 :: Dynamics Double -> Dynamics Double -> Dynamics Double
-- delay3 x t = delay3I x t x

-- delayNI :: Dynamics Double -> Dynamics Double -> Int -> Dynamics Double 
--           -> Dynamics Double
-- delayNI x t n i = s ! n where
--   s   = array (1, n) [(k, f k) | k <- [1 .. n]]
--   f 0 = integ (x - s ! 0) (i * t') / t'
--   f k = integ (s ! (k - 1) - s ! k) (i * t') / t'
--   t'  = t / fromIntegral n

-- delayN :: Dynamics Double -> Dynamics Double -> Int -> Dynamics Double
-- delayN x t n = delayNI x t n x

-- forecast :: Dynamics Double -> Dynamics Double -> Dynamics Double 
--            -> Dynamics Double
-- forecast x at hz =
--   x * (1.0 + (x / smooth x at - 1.0) / at * hz)

-- trend :: Dynamics Double -> Dynamics Double -> Dynamics Double 
--         -> Dynamics Double
-- trend x at i =
--   (x / smoothI x at (x / (1.0 + i * at)) - 1.0) / at

--
-- Difference Equations
--

-- | The 'Sum' type represents a sum defined by some difference equation.
data Sum a = Sum { sumInit     :: Dynamics a,   -- ^ The initial value.
                   sumExternal :: IORef (Dynamics a),
                   sumInternal :: IORef (Dynamics a) }

-- | Create a new sum with the specified initial value.
newSum :: (MArray IOUArray a IO, Num a) => Dynamics a -> Simulation (Sum a)
newSum i =   
  do r1 <- liftIO $ newIORef $ initDynamics i 
     r2 <- liftIO $ newIORef $ initDynamics i 
     let sum = Sum { sumInit     = i, 
                     sumExternal = r1,
                     sumInternal = r2 }
         z = Dynamics $ \p -> 
           do (Dynamics m) <- readIORef (sumInternal sum)
              m p
     y <- umemo0 z
     liftIO $ writeIORef (sumExternal sum) y
     return sum

-- | Return the total sum defined by the difference equation.
sumValue :: Sum a -> Dynamics a
sumValue sum = 
  Dynamics $ \p ->
  do (Dynamics m) <- readIORef (sumExternal sum)
     m p

-- | Set the difference equation for the sum.
sumDiff :: (MArray IOUArray a IO, Num a) => Sum a -> Dynamics a -> Simulation ()
sumDiff sum (Dynamics diff) =
  do let z = Dynamics $ \p ->
           case pointIteration p of
             0 -> do
               let Dynamics i = sumInit sum
               i p
             n -> do 
               Dynamics y <- readIORef (sumExternal sum)
               let sc = pointSpecs p
                   ty = basicTime sc (n - 1) 0
                   py = p { pointTime = ty, 
                            pointIteration = n - 1, 
                            pointPhase = 0 }
               a <- y py
               b <- diff py
               let !v = a + b
               return v
     liftIO $ writeIORef (sumInternal sum) z

--
-- Table Functions
--

-- | Lookup @x@ in a table of pairs @(x, y)@ using linear interpolation.
lookupD :: Dynamics Double -> Array Int (Double, Double) -> Dynamics Double
lookupD (Dynamics m) tbl =
  Dynamics (\p -> do a <- m p; return $ find first last a) where
    (first, last) = bounds tbl
    find left right x =
      if left > right then
        error "Incorrect index: table"
      else
        let index = (left + 1 + right) `div` 2
            x1    = fst $ tbl ! index
        in if x1 <= x then 
             let y | index < right = find index right x
                   | right == last  = snd $ tbl ! right
                   | otherwise     = 
                     let x2 = fst $ tbl ! (index + 1)
                         y1 = snd $ tbl ! index
                         y2 = snd $ tbl ! (index + 1)
                     in y1 + (y2 - y1) * (x - x1) / (x2 - x1) 
             in y
           else
             let y | left < index = find left (index - 1) x
                   | left == first = snd $ tbl ! left
                   | otherwise    = error "Incorrect index: table"
             in y

-- | Lookup @x@ in a table of pairs @(x, y)@ using stepwise function.
lookupStepwiseD :: Dynamics Double -> Array Int (Double, Double)
                  -> Dynamics Double
lookupStepwiseD (Dynamics m) tbl =
  Dynamics (\p -> do a <- m p; return $ find first last a) where
    (first, last) = bounds tbl
    find left right x =
      if left > right then
        error "Incorrect index: table"
      else
        let index = (left + 1 + right) `div` 2
            x1    = fst $ tbl ! index
        in if x1 <= x then 
             let y | index < right = find index right x
                   | right == last  = snd $ tbl ! right
                   | otherwise     = snd $ tbl ! right
             in y
           else
             let y | left < index = find left (index - 1) x
                   | left == first = snd $ tbl ! left
                   | otherwise    = error "Incorrect index: table"
             in y

-- --
-- -- Discrete Functions
-- --
    
-- delayTrans :: Dynamics a -> Dynamics Double -> Dynamics a 
--               -> (Dynamics a -> Dynamics a) -> Dynamics a
-- delayTrans (Dynamics x) (Dynamics d) (Dynamics i) tr = tr $ Dynamics r 
--   where
--     r p = do 
--       let t  = parTime p
--           sc = parSpecs p
--           n  = parIteration p
--       a <- d p
--       let t' = (t - a) - spcStartTime sc
--           n' = fromIntegral $ floor $ t' / spcDT sc
--           y | n' < 0    = i $ p { pointTime = spcStartTime sc,
--                                   pointIteration = 0, 
--                                   pointPhase = 0 }
--             | n' < n    = x $ p { pointTime = t',
--                                   pointIteration = n',
--                                   pointPhase = -1 }
--             | n' > n    = error "Cannot return the future data: delay"
--             | otherwise = error "Cannot return the current data: delay"
--       y    

-- delay :: (Memo a) => Dynamics a -> Dynamics Double -> Dynamics a
-- delay x d = delayTrans x d x $ memo0 discrete

-- delay' :: (UMemo a) => Dynamics a -> Dynamics Double -> Dynamics a
-- delay' x d = delayTrans x d x $ memo0' discrete

-- delayI :: (Memo a) => Dynamics a -> Dynamics Double -> Dynamics a -> Dynamics a
-- delayI x d i = delayTrans x d i $ memo0 discrete         

-- delayI' :: (UMemo a) => Dynamics a -> Dynamics Double -> Dynamics a -> Dynamics a
-- delayI' x d i = delayTrans x d i $ memo0' discrete         
