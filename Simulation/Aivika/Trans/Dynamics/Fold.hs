
-- |
-- Module     : Simulation.Aivika.Trans.Dynamics.Fold
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the fold functions that allows traversing the values of
-- any 'Dynamics' computation in the integration time points.
--
module Simulation.Aivika.Trans.Dynamics.Fold
       (foldDynamics1,
        foldDynamics) where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Dynamics.Memo

--
-- Fold
--

-- | Like the standard 'foldl1' function but applied to values in 
-- the integration time points. The accumulator values are transformed
-- according to the first argument, which should be either function 
-- 'memo0Dynamics' or its unboxed version.
foldDynamics1 :: (Dynamics a -> Simulation (Dynamics a))
                 -> (a -> a -> a) 
                 -> Dynamics a 
                 -> Simulation (Dynamics a)
foldDynamics1 tr f (Dynamics m) =
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
-- 'memo0Dynamics' or its unboxed version.
foldDynamics :: (Dynamics a -> Simulation (Dynamics a))
                -> (a -> b -> a) 
                -> a
                -> Dynamics b 
                -> Simulation (Dynamics a)
foldDynamics tr f acc (Dynamics m) =
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
