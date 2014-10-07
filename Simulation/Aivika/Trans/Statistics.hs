
-- |
-- Module     : Simulation.Aivika.Trans.Statistics
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- Represents statistics.
--

module Simulation.Aivika.Trans.Statistics
       (-- * Simple Statistics
        SamplingStats(..),
        SamplingData(..),
        combineSamplingStatsEither,
        samplingStatsVariance,
        samplingStatsDeviation,
        samplingStatsSummary,
        returnSamplingStats,
        listSamplingStats,
        fromIntSamplingStats,
        -- * Timing Statistics
        TimingStats(..),
        TimingData(..),
        timingStatsDeviation,
        timingStatsSummary,
        returnTimingStats,
        fromIntTimingStats) where 

import Data.Monoid

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
  deriving (Eq, Ord)
           
-- | Specifies data type from which values we can gather the statistics.           
class SamplingData a where           
  
  -- | An empty statistics that has no samples.           
  emptySamplingStats :: SamplingStats a
           
  -- | Add a new sample to the statistics.
  addSamplingStats :: a -> SamplingStats a -> SamplingStats a

  -- | Combine two statistics.
  combineSamplingStats :: SamplingStats a -> SamplingStats a -> SamplingStats a

instance SamplingData a => Monoid (SamplingStats a) where 
  
  mempty = emptySamplingStats
  
  mappend = combineSamplingStats

instance SamplingData Double where

  emptySamplingStats =
    SamplingStats { samplingStatsCount = 0,
                    samplingStatsMin   = 1 / 0,
                    samplingStatsMax   = (-1) / 0,
                    samplingStatsMean  = 0 / 0,
                    samplingStatsMean2 = 0 / 0 }
    
  addSamplingStats = addSamplingStatsGeneric
  
  combineSamplingStats = combineSamplingStatsGeneric
  
instance SamplingData Int where

  emptySamplingStats =
    SamplingStats { samplingStatsCount = 0,
                    samplingStatsMin   = maxBound,
                    samplingStatsMax   = minBound,
                    samplingStatsMean  = 0 / 0,
                    samplingStatsMean2 = 0 / 0 }
    
  addSamplingStats = addSamplingStatsGeneric

  combineSamplingStats = combineSamplingStatsGeneric
  
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

combineSamplingStatsGeneric :: ConvertableToDouble a =>
                               SamplingStats a -> SamplingStats a -> SamplingStats a
combineSamplingStatsGeneric stats1 stats2
  | c1 == 0   = stats2
  | c2 == 0   = stats1
  | otherwise = SamplingStats { samplingStatsCount = c,
                                samplingStatsMin   = minZ,
                                samplingStatsMax   = maxZ,
                                samplingStatsMean  = meanZ,
                                samplingStatsMean2 = meanZ2 }
  where c1     = samplingStatsCount stats1
        c2     = samplingStatsCount stats2
        c      = c1 + c2
        n1     = fromIntegral c1
        n2     = fromIntegral c2
        n      = n1 + n2
        minX   = samplingStatsMin stats1
        minY   = samplingStatsMin stats2
        minZ   = min minX minY
        maxX   = samplingStatsMax stats1
        maxY   = samplingStatsMax stats2
        maxZ   = max maxX maxY
        meanX  = samplingStatsMean stats1
        meanY  = samplingStatsMean stats2
        meanZ  = k1 * meanX + k2 * meanY
        meanX2 = samplingStatsMean2 stats1
        meanY2 = samplingStatsMean2 stats2
        meanZ2 = k1 * meanX2 + k2 * meanY2
        k1     = n1 / n
        k2     = n2 / n

-- | If allows combining statistics more efficiently if we know that the first argument can be a scalar.
combineSamplingStatsEither :: SamplingData a => Either a (SamplingStats a) -> SamplingStats a -> SamplingStats a
combineSamplingStatsEither (Left a) stats2 = addSamplingStats a stats2
combineSamplingStatsEither (Right stats1) stats2 = combineSamplingStats stats1 stats2

-- | Return the variance.
samplingStatsVariance :: SamplingStats a -> Double
samplingStatsVariance stats
  | count == 1 = 0
  | otherwise  = (meanX2 - meanX * meanX) * (n / (n - 1))
    where count  = samplingStatsCount stats
          meanX  = samplingStatsMean stats
          meanX2 = samplingStatsMean2 stats
          n      = fromIntegral count
          
-- | Return the deviation.          
samplingStatsDeviation :: SamplingStats a -> Double
samplingStatsDeviation = sqrt . samplingStatsVariance

-- | Return the statistics by a single sample.
returnSamplingStats :: SamplingData a => a -> SamplingStats a
returnSamplingStats x = addSamplingStats x emptySamplingStats

-- | Create the statistics by the specified list of data.
listSamplingStats :: SamplingData a => [a] -> SamplingStats a
listSamplingStats = foldr addSamplingStats emptySamplingStats

-- | Convert the statistics from integer to double values.
fromIntSamplingStats :: SamplingStats Int -> SamplingStats Double
fromIntSamplingStats stats =
  stats { samplingStatsMin = fromIntegral $ samplingStatsMin stats,
          samplingStatsMax = fromIntegral $ samplingStatsMax stats }

-- | Show the summary of the statistics.       
showSamplingStats :: (Show a) => SamplingStats a -> ShowS
showSamplingStats stats =
  showString "{ count = " . shows (samplingStatsCount stats) . 
  showString ", mean = " . shows (samplingStatsMean stats) . 
  showString ", std = " . shows (samplingStatsDeviation stats) . 
  showString ", min = " . shows (samplingStatsMin stats) . 
  showString ", max = " . shows (samplingStatsMax stats) .
  showString " }"

instance Show a => Show (SamplingStats a) where
  showsPrec prec = showSamplingStats

-- | Show the summary of the statistics using the specified indent.       
samplingStatsSummary :: (Show a) => SamplingStats a -> Int -> ShowS
samplingStatsSummary stats indent =
  let tab = replicate indent ' '
  in showString tab .
     showString "count = " . shows (samplingStatsCount stats) .
     showString "\n" .
     showString tab .
     showString "mean = " . shows (samplingStatsMean stats) . 
     showString "\n" .
     showString tab .
     showString "std = " . shows (samplingStatsDeviation stats) . 
     showString "\n" .
     showString tab .
     showString "min = " . shows (samplingStatsMin stats) . 
     showString "\n" .
     showString tab .
     showString "max = " . shows (samplingStatsMax stats)
     
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
                } deriving (Eq, Ord)
                           
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
                  timingStatsSum       = 0 / 0,
                  timingStatsSum2      = 0 / 0 }
    
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
                  timingStatsSum       = 0 / 0,
                  timingStatsSum2      = 0 / 0 }
    
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
  | count == 0 = 0 / 0
  | t1 > t0    = sumX / (t1 - t0)
  | otherwise  = minX
    where t0    = timingStatsStartTime stats
          t1    = timingStatsLastTime stats
          sumX  = timingStatsSum stats
          minX  = convertToDouble $ timingStatsMin stats
          count = timingStatsCount stats
  
timingStatsMean2Generic :: ConvertableToDouble a => TimingStats a -> Double
timingStatsMean2Generic stats
  | count == 0 = 0 / 0
  | t1 > t0    = sumX2 / (t1 - t0)
  | otherwise  = minX * minX
    where t0    = timingStatsStartTime stats
          t1    = timingStatsLastTime stats
          sumX2 = timingStatsSum2 stats
          minX  = convertToDouble $ timingStatsMin stats
          count = timingStatsCount stats

timingStatsVarianceGeneric :: ConvertableToDouble a => TimingStats a -> Double
timingStatsVarianceGeneric stats = ex2 - ex * ex
  where ex  = timingStatsMeanGeneric stats
        ex2 = timingStatsMean2Generic stats
                
-- | Return the deviation.              
timingStatsDeviation :: TimingData a => TimingStats a -> Double
timingStatsDeviation = sqrt . timingStatsVariance

-- | Return the statistics by single timing data.
returnTimingStats :: TimingData a => Double -> a -> TimingStats a
returnTimingStats t a = addTimingStats t a emptyTimingStats

-- | Convert the statistics from integer to double values.
fromIntTimingStats :: TimingStats Int -> TimingStats Double
fromIntTimingStats stats =
  stats { timingStatsMin = fromIntegral $ timingStatsMin stats,
          timingStatsMax = fromIntegral $ timingStatsMax stats }

-- | Show the summary of the statistics.       
showTimingStats :: (Show a, TimingData a) => TimingStats a -> ShowS
showTimingStats stats =
  showString "{ count = " . shows (timingStatsCount stats) . 
  showString ", mean = " . shows (timingStatsMean stats) . 
  showString ", std = " . shows (timingStatsDeviation stats) . 
  showString ", min = " . shows (timingStatsMin stats) . 
  showString " (t = " . shows (timingStatsMinTime stats) .
  showString "), max = " . shows (timingStatsMax stats) .
  showString " (t = " . shows (timingStatsMaxTime stats) .
  showString "), t in [" . shows (timingStatsStartTime stats) .
  showString ", " . shows (timingStatsLastTime stats) .
  showString "] }"

instance (Show a, TimingData a) => Show (TimingStats a) where
  showsPrec prec = showTimingStats

-- | Show the summary of the statistics using the specified indent.       
timingStatsSummary :: (Show a, TimingData a) => TimingStats a -> Int -> ShowS
timingStatsSummary stats indent =
  let tab = replicate indent ' '
  in showString tab .
     showString "count = " . shows (timingStatsCount stats) . 
     showString "\n" .
     showString tab .
     showString "mean = " . shows (timingStatsMean stats) . 
     showString "\n" .
     showString tab .
     showString "std = " . shows (timingStatsDeviation stats) . 
     showString "\n" .
     showString tab .
     showString "min = " . shows (timingStatsMin stats) . 
     showString " (t = " . shows (timingStatsMinTime stats) .
     showString ")\n" .
     showString tab .
     showString "max = " . shows (timingStatsMax stats) .
     showString " (t = " . shows (timingStatsMaxTime stats) .
     showString ")\n" .
     showString tab .
     showString "t in [" . shows (timingStatsStartTime stats) .
     showString ", " . shows (timingStatsLastTime stats) .
     showString "]"
