
-- |
-- Module     : Simulation.Aivika.Dynamics.Parameter
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines the parameters of simulation experiments.
--

module Simulation.Aivika.Dynamics.Parameter
       (newParameter,
        newTableParameter,
        newIndexedParameter,
        newRandomParameter,
        newNormalParameter) where

import Data.Array
import Data.IORef
import qualified Data.Map as M
import Control.Concurrent.MVar
import System.Random

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Random

-- | Create a thread-safe parameter that returns always the same value within the simulation run, 
-- where the value is recalculated for each new run.
newParameter :: IO a -> IO (Simulation a)
newParameter a = newIndexedParameter $ \_ -> a

-- | Create a thread-safe parameter that returns always the same value within the simulation run,
-- where the value is taken consequently from the specified table based on the number of the 
-- current run starting from zero. After all values from the table are used, it takes the first 
-- value of the table, then the second one and so on.
newTableParameter :: Array Int a -> IO (Simulation a)
newTableParameter t = newIndexedParameter (\i -> return $ t ! (((i - i1) `mod` n) + i1))
  where (i1, i2) = bounds t
        n = i2 - i1 + 1

-- | Create a thread-safe parameter that returns always the same value within the simulation run, 
-- where the value depends on the number of this run starting from zero.
newIndexedParameter :: (Int -> IO a) -> IO (Simulation a)
newIndexedParameter f = 
  do lock <- newMVar ()
     dict <- newIORef M.empty
     return $ Simulation $ \r ->
       do let i = runIndex r
          m <- readIORef dict
          if M.member i m
            then do let Just v = M.lookup i m
                    return v
            else withMVar lock $ 
                 \() -> do { m <- readIORef dict;
                             if M.member i m
                             then do let Just v = M.lookup i m
                                     return v
                             else do v <- f i
                                     writeIORef dict $ M.insert i v m
                                     return v }

-- | Create a new random parameter distributed uniformly.
-- The value doesn't change within the simulation run but
-- then the value is recalculated for each new run.
newRandomParameter :: Simulation Double     -- ^ minimum
                      -> Simulation Double  -- ^ maximum
                      -> IO (Simulation Double)
newRandomParameter min max =
  do x <- newParameter $ getStdRandom random
     return $ min + x * (max - min)

-- | Create a new random parameter distributed normally.
-- The value doesn't change within the simulation run but
-- then the value is recalculated for each new run.
newNormalParameter :: Simulation Double     -- ^ mean
                      -> Simulation Double  -- ^ variance
                      -> IO (Simulation Double)
newNormalParameter mu nu =
  do x <- normalGen >>= newParameter
     return $ mu + x * nu
