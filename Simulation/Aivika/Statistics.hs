
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
        showSamplingStats,
        TimingStats(..),
        TimingData(..),
        timingStatsDeviation,
        showTimingStats) where 

-- | Defines data types that can be converted to 'Double'.
class Ord a => ConvertableToDouble a where
  
  -- | Convert the value to 'Double'.
  convertToDouble :: a -> Double
  
instance ConvertableToDouble Double where
  convertToDouble = id
  
instance ConvertableToDouble Int where
  convertToDouble = fromIntegral

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
    
  addSamplingStats = addSamplingStatsGeneric
  
instance SamplingData Int where

  emptySamplingStats =
    SamplingStats { samplingStatsCount = 0,
                    samplingStatsMin   = maxBound,
                    samplingStatsMax   = minBound,
                    samplingStatsMean  = 0 / 0,
                    samplingStatsMean2 = 0 / 0 }
    
  addSamplingStats = addSamplingStatsGeneric
  
addSamplingStatsGeneric :: ConvertableToDouble a => a -> SamplingStats a -> SamplingStats a
addSamplingStatsGeneric a stats 
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
          minX   = a `seq` min a (samplingStatsMin stats)
          maxX   = a `seq` max a (samplingStatsMax stats)
          meanX  = k1 * x + k2 * samplingStatsMean stats
          meanX2 = k1 * x * x + k2 * samplingStatsMean2 stats
          n      = fromIntegral count
          x      = convertToDouble a
          k1     = 1.0 / n
          k2     = (n - 1.0) / n

-- | Return the variance.
samplingStatsVariance :: SamplingStats a -> Double
samplingStatsVariance stats
  | count == 1 = meanX2 - meanX * meanX
  | otherwise  = (meanX2 - meanX * meanX) * (n / (n - 1))
    where count  = samplingStatsCount stats
          meanX  = samplingStatsMean stats
          meanX2 = samplingStatsMean2 stats
          n      = fromIntegral count
          
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
     
-- | This is the timing statistics where data are bound to the time.
data TimingStats a =
  TimingStats { timingStatsCount     :: !Int,
                -- ^ Return the number of samples.
                timingStatsMin       :: !a,
                -- ^ Return the minimum value.
                timingStatsMax       :: !a,
                -- ^ Return the maximum value.
                timingStatsMinTime   :: !Double,
                -- ^ Return the time at which the minimum is attained.
                timingStatsMaxTime   :: !Double,
                -- ^ Return the time at which the maximum is attained.
                timingStatsStartTime :: !Double,
                -- ^ Return the start time of sampling.
                timingStatsLastTime  :: !Double,
                -- ^ Return the last time of sampling.
                timingStatsSum       :: !Double,
                -- ^ Return the sum of values.
                timingStatsSum2      :: !Double 
                -- ^ Return the sum of square values.
                } deriving (Eq, Ord, Show)
                           
-- | Defines the data type from which values we can gather the timing statistics.
class TimingData a where                           
  
  -- | An empty statistics that has no samples.
  emptyTimingStats :: TimingStats a
  
  -- | Add a sample with the specified time to the statistics.
  addTimingStats :: Double -> a -> TimingStats a -> TimingStats a
  
  -- | Return the average value.
  timingStatsMean :: TimingStats a -> Double
  
  -- | Return the variance.
  timingStatsVariance :: TimingStats a -> Double
  
instance TimingData Double where
  
  emptyTimingStats = 
    TimingStats { timingStatsCount     = 0,
                  timingStatsMin       = 1 / 0,
                  timingStatsMax       = (-1) / 0,
                  timingStatsMinTime   = 1 / 0,
                  timingStatsMaxTime   = (-1) / 0,
                  timingStatsStartTime = 1 / 0,
                  timingStatsLastTime  = (-1) / 0,
                  timingStatsSum       = 0,
                  timingStatsSum2      = 0 }
    
  addTimingStats      = addTimingStatsGeneric
  timingStatsMean     = timingStatsMeanGeneric
  timingStatsVariance = timingStatsVarianceGeneric

instance TimingData Int where
  
  emptyTimingStats = 
    TimingStats { timingStatsCount     = 0,
                  timingStatsMin       = maxBound,
                  timingStatsMax       = minBound,
                  timingStatsMinTime   = 1 / 0,
                  timingStatsMaxTime   = (-1) / 0,
                  timingStatsStartTime = 1 / 0,
                  timingStatsLastTime  = (-1) / 0,
                  timingStatsSum       = 0,
                  timingStatsSum2      = 0 }
    
  addTimingStats      = addTimingStatsGeneric
  timingStatsMean     = timingStatsMeanGeneric
  timingStatsVariance = timingStatsVarianceGeneric

addTimingStatsGeneric :: ConvertableToDouble a => Double -> a -> TimingStats a -> TimingStats a
addTimingStatsGeneric t a stats
  | t < t'     = error "The current time cannot be less than the previous one: addTimingStats"
  | isNaN x    = stats
  | count == 1 = TimingStats { timingStatsCount     = 1,
                               timingStatsMin       = a,
                               timingStatsMax       = a,
                               timingStatsMinTime   = t,
                               timingStatsMaxTime   = t,
                               timingStatsStartTime = t,
                               timingStatsLastTime  = t,
                               timingStatsSum       = 0,
                               timingStatsSum2      = 0 }
  | otherwise  = TimingStats { timingStatsCount     = count,
                               timingStatsMin       = minX,
                               timingStatsMax       = maxX,
                               timingStatsMinTime   = minT,
                               timingStatsMaxTime   = maxT,
                               timingStatsStartTime = t0,
                               timingStatsLastTime  = t,
                               timingStatsSum       = sumX,
                               timingStatsSum2      = sumX2 }
    where count = 1 + timingStatsCount stats
          minX' = timingStatsMin stats
          maxX' = timingStatsMax stats
          minX  = a `seq` min a minX'
          maxX  = a `seq` max a maxX'
          minT | a < minX' = t
               | otherwise = timingStatsMinTime stats
          maxT | a > maxX' = t
               | otherwise = timingStatsMaxTime stats
          t0 = timingStatsStartTime stats
          t' = timingStatsLastTime stats
          x  = convertToDouble a
          sumX'  = timingStatsSum stats
          sumX   = sumX' + (t - t') * x
          sumX2' = timingStatsSum2 stats
          sumX2  = sumX2' + (t - t') * x * x
      
timingStatsMeanGeneric :: ConvertableToDouble a => TimingStats a -> Double
timingStatsMeanGeneric stats 
  | t1 > t0   = sumX / (t1 - t0)
  | otherwise = minX
    where t0   = timingStatsStartTime stats
          t1   = timingStatsLastTime stats
          sumX = timingStatsSum stats
          minX = convertToDouble $ timingStatsMin stats
  
timingStatsMean2Generic :: ConvertableToDouble a => TimingStats a -> Double
timingStatsMean2Generic stats
  | t1 > t0   = sumX2 / (t1 - t0)
  | otherwise = minX * minX
    where t0    = timingStatsStartTime stats
          t1    = timingStatsLastTime stats
          sumX2 = timingStatsSum2 stats
          minX  = convertToDouble $ timingStatsMin stats

timingStatsVarianceGeneric :: ConvertableToDouble a => TimingStats a -> Double
timingStatsVarianceGeneric stats = ex2 - ex * ex
  where ex  = timingStatsMeanGeneric stats
        ex2 = timingStatsMean2Generic stats
                
-- | Return the deviation.              
timingStatsDeviation :: TimingData a => TimingStats a -> Double
timingStatsDeviation = sqrt . timingStatsVariance

-- | Show the summary of the statistics with the specified indent.       
showTimingStats :: (Show a, TimingData a) => TimingStats a -> Int -> ShowS
showTimingStats stats indent =
  let tab = replicate indent ' '
  in showString tab .
     showString "count     = " . shows (timingStatsCount stats) . 
     showString "\n" . 
     showString tab .
     showString "mean      = " . shows (timingStatsMean stats) . 
     showString "\n" . 
     showString tab .
     showString "deviation = " . shows (timingStatsDeviation stats) . 
     showString "\n" .
     showString tab .
     showString "minimum   = " . shows (timingStatsMin stats) . 
     showString " at t = " . shows (timingStatsMinTime stats) .
     showString "\n" .
     showString tab .
     showString "maximum   = " . shows (timingStatsMax stats) .
     showString " at t = " . shows (timingStatsMaxTime stats) .
     showString "\n" .
     showString tab .
     showString "t in [" . shows (timingStatsStartTime stats) .
     showString ", " . shows (timingStatsLastTime stats) .
     showString "]"
