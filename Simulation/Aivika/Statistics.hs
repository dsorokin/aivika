
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Statistics
-- Copyright  : Copyright (c) 2009-2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- Represents statistics and results.
--
module Simulation.Aivika.Statistics
       (Statistics, 
        newStatistics,
        addStatistics,
        statisticsData,
        analyzeData,
        AnalysisResults(..),
        showResults) where 

import Data.Foldable
import Data.Array
import Data.Array.IO
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import Simulation.Aivika.UVector

-- | Represents statistics. 
-- 
-- All functions with the statistics in this module are thread-safe. Therefore 
-- you can use them in experiments when parallel simulations execute simultaneously.
data Statistics a = Statistics { statData :: UVector a, 
                                 statLock :: MVar () }

-- | Create new statistics.
newStatistics :: (MArray IOUArray a IO) => IO (Statistics a)
newStatistics = 
  do v <- newVector
     l <- newMVar ()
     return Statistics { statData = v, 
                         statLock = l }

-- | Add data to the statistics. It is thread-safe.
addStatistics :: (MArray IOUArray a IO) => Statistics a -> a -> IO ()
addStatistics s x = 
  withMVar (statLock s) $ \() ->
  appendVector (statData s) x

-- | Return the statistics data. It is thread-safe.
statisticsData :: (MArray IOUArray a IO) => Statistics a -> IO (Array Int a)
statisticsData s =
  withMVar (statLock s) $ \() -> freezeVector (statData s)
       
-- | Represents the results of the statistic analysis.
data AnalysisResults a = 
  AnalysisResults { resultsData     :: Array Int a,
                    -- ^ Statistic data.
                    resultsMean     :: Double,
                    -- ^ The average value.
                    resultsVariance :: Double,
                    -- ^ The variance.
                    resultsMin      :: a,
                    -- ^ The minimum value.
                    resultsMax      :: a 
                    -- ^ The maximum value.
                  } deriving (Eq, Ord, Show)

-- | Analyze data.
analyzeData :: Real a => Array Int a -> AnalysisResults a
analyzeData xs =
  let (i1, i2) = bounds xs
      meanx = foldl' (\y i -> y * (1 - k i) + f i * k i) 0 [i1 .. i2]
      sqrx  = foldl' (\y i -> y * (1 - k i) + g i * k i) 0 [i1 .. i2]
      minx  = foldl' (\y i -> if i == 0 then x i else min y (x i)) 0 [i1 .. i2]
      maxx  = foldl' (\y i -> if i == 0 then x i else max y (x i)) 0 [i1 .. i2]
      x i = xs ! i
      f i = fromRational (toRational (x i))
      g i = let y = f i in y * y
      k i = 1 / fromInteger (toInteger (i - i1 + 1))
  in AnalysisResults { resultsData = xs,
                       resultsMean = meanx,
                       resultsVariance = sqrx - meanx * meanx,
                       resultsMin = minx,
                       resultsMax = maxx }
       
-- | Show the results of analysis with the specified indent.       
showResults :: (Show a) => AnalysisResults a -> Int -> ShowS
showResults rs indent =
  let (i1, i2) = bounds (resultsData rs)
      tab = replicate indent ' '
  in if i1 <= i2
     then
       showString tab .
       showString "mean      = " . shows (resultsMean rs) . 
       showString "\n" . 
       showString tab .
       showString "deviation = " . shows (sqrt (resultsVariance rs)) . 
       showString "\n" .
       showString tab .
       showString "minimum   = " . shows (resultsMin rs) . 
       showString "\n" .
       showString tab .
       showString "maximum   = " . shows (resultsMax rs)
     else
       showString tab .
       showString "mean      = ---" .
       showString "\n" . 
       showString tab .
       showString "deviation = ---" .
       showString "\n" . 
       showString tab .
       showString "minimum   = ---" .
       showString "\n" . 
       showString tab .
       showString "maximum   = ---"