
-- |
-- Module     : Simulation.Aivika.Dynamics.Internal.Fold
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module defines the fold functions that allows traversing the values of
-- any dynamic process in the integration time points.
--
module Simulation.Aivika.Dynamics.Internal.Fold
       (foldD1,
        foldD,
        divideD) where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Interpolate
import Simulation.Aivika.Dynamics.Internal.Memo

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
-- the current iteration. It can be useful for statistic functions in
-- combination with the fold.
divideD :: Dynamics Double -> Dynamics Double
divideD (Dynamics m) = 
  discrete $ Dynamics $ \p ->
  do a <- m p
     return $ a / fromInteger (toInteger (pointIteration p + 1))
