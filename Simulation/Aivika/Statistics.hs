
-- |
-- Module     : Simulation.Aivika.Statistics
-- Copyright  : Copyright (c) 2009-2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- Represents statistics.
--

module Simulation.Aivika.Statistics
       (SamplingStats(..),
        SamplingData(..),
        samplingStatsVariance,
        samplingStatsDeviation,
        showSamplingStats) where 

-- | Describes when the statistics consists of only samples 
-- not bound to the simulation time.
data SamplingStats a =       
  SamplingStats { samplingStatsCount :: !Int,
                  -- ^ The total number of samples.
                  samplingStatsMin   :: !a,
                  -- ^ The minimum value among the samples.
                  samplingStatsMax   :: !a,
                  -- ^ The maximum value among the samples.
                  samplingStatsMean  :: !Double,
                  -- ^ The average value.
                  samplingStatsMean2 :: !Double 
                  -- ^ The average square value.
                }
  deriving (Eq, Ord, Show)
           
-- | Specifies data type from which values we can gather the statistics.           
class SamplingData a where           
  
  -- | An empty statistics that has no samples.           
  emptySamplingStats :: SamplingStats a
           
  -- | Add a new sample to the statistics.
  addSamplingStats :: a -> SamplingStats a -> SamplingStats a

instance SamplingData Double where

  emptySamplingStats =
    SamplingStats { samplingStatsCount = 0,
                    samplingStatsMin   = 1 / 0,
                    samplingStatsMax   = (-1) / 0,
                    samplingStatsMean  = 0 / 0,
                    samplingStatsMean2 = 0 / 0 }
    
  addSamplingStats = addSamplingStatsDouble
  
instance SamplingData Int where

  emptySamplingStats =
    SamplingStats { samplingStatsCount = 0,
                    samplingStatsMin   = maxBound,
                    samplingStatsMax   = minBound,
                    samplingStatsMean  = 0 / 0,
                    samplingStatsMean2 = 0 / 0 }
    
  addSamplingStats = addSamplingStatsInt
  
addSamplingStatsDouble :: Double -> SamplingStats Double -> SamplingStats Double
addSamplingStatsDouble a stats 
  | isNaN x    = stats
  | count == 1 = SamplingStats { samplingStatsCount = 1,
                                 samplingStatsMin   = a,
                                 samplingStatsMax   = a,
                                 samplingStatsMean  = x,
                                 samplingStatsMean2 = x * x }
  | otherwise  = SamplingStats { samplingStatsCount = count,
                                 samplingStatsMin   = minX,
                                 samplingStatsMax   = maxX,
                                 samplingStatsMean  = meanX,
                                 samplingStatsMean2 = meanX2 }
    where count  = 1 + samplingStatsCount stats
          minX   = a `seq` (min a $ samplingStatsMin stats)
          maxX   = a `seq` (max a $ samplingStatsMax stats)
          meanX  = k1 * x + k2 * samplingStatsMean stats
          meanX2 = k1 * x * x + k2 * samplingStatsMean2 stats
          n      = fromInteger $ toInteger count
          x      = a
          k1     = 1.0 / n
          k2     = (n - 1.0) / n

addSamplingStatsInt :: Int -> SamplingStats Int -> SamplingStats Int
addSamplingStatsInt a stats 
  | isNaN x    = stats
  | count == 1 = SamplingStats { samplingStatsCount = 1,
                                 samplingStatsMin   = a,
                                 samplingStatsMax   = a,
                                 samplingStatsMean  = x,
                                 samplingStatsMean2 = x * x }
  | otherwise  = SamplingStats { samplingStatsCount = count,
                                 samplingStatsMin   = minX,
                                 samplingStatsMax   = maxX,
                                 samplingStatsMean  = meanX,
                                 samplingStatsMean2 = meanX2 }
    where count  = 1 + samplingStatsCount stats
          minX   = a `seq` (min a $ samplingStatsMin stats)
          maxX   = a `seq` (max a $ samplingStatsMax stats)
          meanX  = k1 * x + k2 * samplingStatsMean stats
          meanX2 = k1 * x * x + k2 * samplingStatsMean2 stats
          n      = fromInteger $ toInteger count
          x      = fromInteger $ toInteger a
          k1     = 1.0 / n
          k2     = (n - 1.0) / n

-- | Return the variance.
samplingStatsVariance :: SamplingStats a -> Double
samplingStatsVariance stats
  | count == 1 = (meanX2 - meanX * meanX)
  | otherwise  = (meanX2 - meanX * meanX) * (n / (n - 1))
    where count  = samplingStatsCount stats
          meanX  = samplingStatsMean stats
          meanX2 = samplingStatsMean2 stats
          n      = fromInteger $ toInteger count
          
-- | Return the deviation.          
samplingStatsDeviation :: SamplingStats a -> Double
samplingStatsDeviation = sqrt . samplingStatsVariance

-- | Show the summary of the statistics with the specified indent.       
showSamplingStats :: (Show a) => SamplingStats a -> Int -> ShowS
showSamplingStats stats indent =
  let tab = replicate indent ' '
  in showString tab .
     showString "count     = " . shows (samplingStatsCount stats) . 
     showString "\n" . 
     showString tab .
     showString "mean      = " . shows (samplingStatsMean stats) . 
     showString "\n" . 
     showString tab .
     showString "deviation = " . shows (samplingStatsDeviation stats) . 
     showString "\n" .
     showString tab .
     showString "minimum   = " . shows (samplingStatsMin stats) . 
     showString "\n" .
     showString tab .
     showString "maximum   = " . shows (samplingStatsMax stats)
       