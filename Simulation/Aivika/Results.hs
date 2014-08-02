
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Results
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module allows exporting the simulation results from the model.
--
module Simulation.Aivika.Results where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Array as A

import Data.Ix
import Data.Maybe
import Data.Monoid

import Simulation.Aivika.Parameter
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Signal
import Simulation.Aivika.Statistics
import Simulation.Aivika.Statistics.Accumulator
import Simulation.Aivika.Ref
import qualified Simulation.Aivika.Ref.Light as LR
import Simulation.Aivika.Var
import qualified Simulation.Aivika.Queue as Q
import qualified Simulation.Aivika.Queue.Infinite as IQ
import Simulation.Aivika.Arrival
import Simulation.Aivika.Server

-- | A locale to output the simulation results.
--
-- Examples are: @\"ru\", @\"en\" etc.
type ResultLocale = String

-- | It localises the description of simulation results.
type ResultLocalisation = ResultId -> ResultDescription

-- | A name used for indentifying the results when generating output.
type ResultName = String

-- | A description used for describing the results when generating output.
type ResultDescription = String

-- | The result entity identifier.
data ResultId = TimeId
                -- ^ A 'time' computation.
              | VectorId
                -- ^ Describes a vector.
              | VectorItemId String
                -- ^ Described a vector item with the specified subscript.
              | SamplingStatsId
                -- ^ A 'SamplingStats' value.
              | SamplingStatsCountId
                -- ^ Property 'samplingStatsCount'.
              | SamplingStatsMinId
                -- ^ Property 'samplingStatsMin'.
              | SamplingStatsMaxId
                -- ^ Property 'samplingStatsMax'.
              | SamplingStatsMeanId
                -- ^ Property 'samplingStatsMean'.
              | SamplingStatsMean2Id
                -- ^ Property 'samplingStatsMean2'.
              | SamplingStatsVarianceId
                -- ^ Property 'samplingStatsVariance'.
              | SamplingStatsDeviationId
                -- ^ Property 'samplingStatsDeviation'.
              | TimingStatsId
                -- ^ A 'TimingStats' value.
              | TimingStatsCountId
                -- ^ Property 'timingStatsCount'.
              | TimingStatsMinId
                -- ^ Property 'timingStatsMin'.
              | TimingStatsMaxId
                -- ^ Property 'timingStatsMax'.
              | TimingStatsMeanId
                -- ^ Property 'timingStatsMean'.
              | TimingStatsVarianceId
                -- ^ Property 'timingStatsVariance'.
              | TimingStatsDeviationId
                -- ^ Property 'timingStatsDeviation'.
              | TimingStatsMinTimeId
                -- ^ Property 'timingStatsMinTime'.
              | TimingStatsMaxTimeId
                -- ^ Property 'timingStatsMaxTime'.
              | TimingStatsStartTimeId
                -- ^ Property 'timingStatsStartTime'.
              | TimingStatsLastTimeId
                -- ^ Property 'timingStatsLastTime'.
              | TimingStatsSumId
                -- ^ Property 'timingStatsSum'.
              | TimingStatsSum2Id
                -- ^ Property 'timingStatsSum2'.
              | FiniteQueueId
                -- ^ A finite 'Q.Queue'.
              | InfiniteQueueId
                -- ^ An infinite 'IQ.Queue'.
              | EnqueueStrategyId
                -- ^ Property 'Q.enqueueStrategy'.
              | EnqueueStoringStrategyId
                -- ^ Property 'Q.enqueueStoringStrategy'.
              | DequeueStrategyId
                -- ^ Property 'Q.dequeueStrategy'.
              | QueueNullId
                -- ^ Property 'Q.queueNull'.
              | QueueFullId
                -- ^ Property 'Q.queueFull'.
              | QueueMaxCountId
                -- ^ Property 'Q.queueMaxCount'.
              | QueueCountId
                -- ^ Property 'Q.queueCount'.
              | QueueCountStatsId
                -- ^ Property 'Q.queueCountStats'.
              | EnqueueCountId
                -- ^ Property 'Q.enqueueCount'.
              | EnqueueLostCountId
                -- ^ Property 'Q.enqueueLostCount'.
              | EnqueueStoreCountId
                -- ^ Property 'Q.enqueueStoreCount'.
              | DequeueCountId
                -- ^ Property 'Q.dequeueCount'.
              | DequeueExtractCountId
                -- ^ Property 'Q.dequeueExtractCount'.
              | QueueLoadFactorId
                -- ^ Property 'Q.queueLoadFactor'.
              | EnqueueRateId
                -- ^ Property 'Q.enqueueRate'.
              | EnqueueStoreRateId
                -- ^ Property 'Q.enqueueStoreRate'.
              | DequeueRateId
                -- ^ Property 'Q.dequeueRate'.
              | DequeueExtractRateId
                -- ^ Property 'Q.dequeueExtractRate'.
              | QueueWaitTimeId
                -- ^ Property 'Q.queueWaitTime'.
              | QueueTotalWaitTimeId
                -- ^ Property 'Q.queueTotalWaitTime'.
              | EnqueueWaitTimeId
                -- ^ Property 'Q.enqueueWaitTime'.
              | DequeueWaitTimeId
                -- ^ Property 'Q.dequeueWaitTime'.
              | ArrivalTimerId
                -- ^ An 'ArrivalTimer'.
              | ArrivalProcessingTimeId
                -- ^ Property 'arrivalProcessingTime'.
              | ServerId
                -- ^ Represents a 'Server'.
              | ServerInitStateId
                -- ^ Property 'serverInitState'.
              | ServerStateId
                -- ^ Property 'serverState'.
              | ServerTotalInputWaitTimeId
                -- ^ Property 'serverTotalInputWaitTime'.
              | ServerTotalProcessingTimeId
                -- ^ Property 'serverTotalProcessingTime'.
              | ServerTotalOutputWaitTimeId
                -- ^ Property 'serverTotalOutputWaitTime'.
              | ServerInputWaitTimeId
                -- ^ Property 'serverInputWaitTime'.
              | ServerProcessingTimeId
                -- ^ Property 'serverProcessingTime'.
              | ServerOutputWaitTimeId
                -- ^ Property 'serverOutputWaitTime'.
              | ServerInputWaitFactorId
                -- ^ Property 'serverInputWaitFactor'.
              | ServerProcessingFactorId
                -- ^ Property 'serverProcessingFactor'.
              | ServerOutputWaitFactorId
                -- ^ Property 'serverOutputWaitFactor'.
              | UserDefinedResultId ResultDescription
                -- ^ An user defined description.
              | LocalisedResultId (M.Map ResultLocale ResultDescription)
                -- ^ A localised property or object name.

-- | Represents a provider of the simulation results. It is usually something, or
-- an array of something, or a list of such values which can be simulated to get data.
class ResultProvider p where
  
  -- | Return the source of simulation results by the specified name, description and provider. 
  resultSource :: ResultName -> ResultDescription -> p -> ResultSource
  resultSource name descr = resultSource' name (UserDefinedResultId descr)

  -- | Return the source of simulation results by the specified name, identifier and provider. 
  resultSource' :: ResultName -> ResultId -> p -> ResultSource

-- | Specifies the type of results we want to receive.
data ResultType = DoubleResultType
                  -- ^ Return double numbers in time points.
                | DoubleListResultType
                  -- ^ Return lists of double numbers in time points.
                | DoubleStatsResultType
                  -- ^ Return statistics based on double numbers.
                | DoubleTimingStatsResultType
                  -- ^ Return a timing statistics based on double numbers.
                | IntResultType
                  -- ^ Return integer numbers in time points.
                | IntListResultType
                  -- ^ Return lists of integer numbers in time points.
                | IntStatsResultType
                  -- ^ Return statistics based on integer numbers.
                | IntTimingStatsResultType
                  -- ^ Return a timing statistics based on integer numbers.
                | StringResultType
                  -- ^ Return string representations in time points.
                | DefaultResultType
                  -- ^ Return data in the default type.

-- | Contains the very simulation results.
data ResultData = DoubleResultData (Event Double)
                  -- ^ Contains the double numbers in time points.
                | DoubleListResultData (Event [Double])
                  -- ^ Contains the lists of double numbers in time points.
                | DoubleStatsResultData (Event (SamplingStats Double))
                  -- ^ Contains the statistics based on double numbers.
                | DoubleTimingStatsResultData (Event (TimingStats Double))
                  -- ^ Contains the timing statistics based on double numbers.
                | IntResultData (Event Int)
                  -- ^ Contains the integer numbers in time points.
                | IntListResultData (Event [Int])
                  -- ^ Contains the lists of integer numbers in time points.
                | IntStatsResultData (Event (SamplingStats Int))
                  -- ^ Contains the statistics based on integer numbers.
                | IntTimingStatsResultData (Event (TimingStats Int))
                  -- ^ Contains the timing statistics based on integer numbers.
                | StringResultData (Event String)
                  -- ^ Contains the string representations in time.
                | NoResultData
                  -- ^ Cannot return data.

-- | Whether an object containing the results emits a signal notifying about change of data.
type ResultSignal = Maybe (Signal ())
                  
-- | It associates the result sources with their names.
type ResultSourceMap = M.Map ResultName ResultSource

-- | Defines a source that actually returns simulation results.
data ResultSource = ResultItemSource ResultItem
                    -- ^ The item source.
                  | ResultObjectSource ResultObject
                    -- ^ The object source.
                  | ResultVectorSource ResultVector
                    -- ^ The vector source.
                  | ResultSeparatorSource ResultSeparator
                    -- ^ This is a separator text.

-- | The simulation results represented by a single item.
data ResultItem =
  ResultItem { resultItemName :: ResultName,
               -- ^ The item name.
               resultItemId :: ResultId,
               -- ^ The item identifier.
               resultItemData :: ResultData,
               -- ^ The item data.
               resultItemSignal :: ResultSignal
               -- ^ Whether the item emits a signal.
             }
  
-- | The simulation results represented by an object having properties.
data ResultObject =
  ResultObject { resultObjectName :: ResultName,
                 -- ^ The object name.
                 resultObjectId :: ResultId,
                 -- ^ The object identifier.
                 resultObjectTypeId :: ResultId,
                 -- ^ The object type identifier.
                 resultObjectProperties :: [ResultProperty],
                 -- ^ The object properties.
                 resultObjectSummary :: ResultSource
                 -- ^ A short version of the object.
               }

-- | The object property containing the simulation results.
data ResultProperty =
  ResultProperty { resultPropertyLabel :: ResultName,
                   -- ^ The property short label.
                   resultPropertyId :: ResultId,
                   -- ^ The property identifier.
                   resultPropertySource :: ResultSource
                   -- ^ The simulation results supplied by the property.
                 }

-- | The simulation results represented by a vector.
data ResultVector =
  ResultVector { resultVectorName :: ResultName,
                 -- ^ The vector name.
                 resultVectorId :: ResultId,
                 -- ^ The vector identifier.
                 resultVectorItems :: V.Vector ResultSource,
                 -- ^ The sources supplied by the vector items.
                 resultVectorSubscript :: V.Vector String
                 -- ^ The subscript used as a suffix to create item names.
               }

-- | It separates the simulation results when printing.
data ResultSeparator =
  ResultSeparator { resultSeparatorText :: String
                    -- ^ The separator text.
                  }

-- | Return the result source name.
resultSourceName :: ResultSource -> ResultName
resultSourceName (ResultItemSource x) = resultItemName x
resultSourceName (ResultObjectSource x) = resultObjectName x
resultSourceName (ResultVectorSource x) = resultVectorName x
resultSourceName (ResultSeparatorSource x) = []

-- | Return a short version of the source, i.e. its summary.
resultSourceSummary :: ResultSource -> ResultSource
resultSourceSummary (ResultItemSource (ResultItem n i (DoubleStatsResultData x) s)) =
  makeSamplingStatsSummary n i x
resultSourceSummary (ResultItemSource (ResultItem n i (DoubleTimingStatsResultData x) s)) =
  makeTimingStatsSummary n i x
resultSourceSummary (ResultItemSource (ResultItem n i (IntStatsResultData x) s)) =
  makeSamplingStatsSummary n i x
resultSourceSummary (ResultItemSource (ResultItem n i (IntTimingStatsResultData x) s)) =
  makeTimingStatsSummary n i x
resultSourceSummary z@(ResultItemSource x) = z
resultSourceSummary (ResultObjectSource x) = resultObjectSummary x
resultSourceSummary (ResultVectorSource x) =
  ResultVectorSource $
  x { resultVectorItems =
         V.map resultSourceSummary (resultVectorItems x) }
resultSourceSummary z@(ResultSeparatorSource x) = z

-- | Flatten the result items.
flattenResultItems :: ResultSource -> [ResultItem]
flattenResultItems (ResultItemSource x) = [x]
flattenResultItems (ResultObjectSource x) =
  concat $ map (flattenResultItems . resultPropertySource) $ resultObjectProperties x
flattenResultItems (ResultVectorSource x) =
  concat $ map flattenResultItems $ V.toList $ resultVectorItems x
flattenResultItems (ResultSeparatorSource x) = []

-- | Transform the result items using the specified function.
mapResultItems :: (ResultItem -> ResultSource) -> ResultSource -> ResultSource
mapResultItems f (ResultItemSource x) = f x
mapResultItems f (ResultObjectSource x) =
  ResultObjectSource $
  x { resultObjectProperties =
         flip map (resultObjectProperties x) $ \p ->
         p { resultPropertySource = 
                mapResultItems f (resultPropertySource p) } }
mapResultItems f (ResultVectorSource x) =
  ResultVectorSource $
  x { resultVectorItems =
         V.map (mapResultItems f) (resultVectorItems x) }
mapResultItems f z@(ResultSeparatorSource x) = z

-- | Retype the result item changing the data type and probably even the data structure. 
retypeResultItem :: ResultType -> ResultItem -> ResultSource
retypeResultItem DoubleResultType z = ResultItemSource $ tr z
  where
    tr z@(ResultItem n i (DoubleResultData x) s) =
      z
    tr z@(ResultItem n i (DoubleListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntResultData x) s) =
      z { resultItemData = DoubleResultData $ fmap fromIntegral x }
    tr z@(ResultItem n i (IntListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem DoubleListResultType z = ResultItemSource $ tr z
  where
    tr z@(ResultItem n i (DoubleResultData x) s) =
      z { resultItemData = DoubleListResultData $ fmap return x }
    tr z@(ResultItem n i (DoubleListResultData x) s) =
      z
    tr z@(ResultItem n i (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntResultData x) s) =
      z { resultItemData = DoubleListResultData $ fmap (return . fromIntegral) x }
    tr z@(ResultItem n i (IntListResultData x) s) =
      z { resultItemData = DoubleListResultData $ fmap (fmap fromIntegral) x }
    tr z@(ResultItem n i (IntStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem DoubleStatsResultType z = ResultItemSource $ tr z
  where
    tr z@(ResultItem n i (DoubleResultData x) s) =
      z { resultItemData = DoubleStatsResultData $ fmap returnSamplingStats x }
    tr z@(ResultItem n i (DoubleListResultData x) s) =
      z { resultItemData = DoubleStatsResultData $ fmap listSamplingStats x }
    tr z@(ResultItem n i (DoubleStatsResultData x) s) =
      z
    tr z@(ResultItem n i (DoubleTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntResultData x) s) =
      z { resultItemData = DoubleStatsResultData $ fmap (fromIntSamplingStats . returnSamplingStats) x }
    tr z@(ResultItem n i (IntListResultData x) s) =
      z { resultItemData = DoubleStatsResultData $ fmap (fromIntSamplingStats . listSamplingStats) x }
    tr z@(ResultItem n i (IntStatsResultData x) s) =
      z { resultItemData = DoubleStatsResultData $ fmap fromIntSamplingStats x }
    tr z@(ResultItem n i (IntTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem DoubleTimingStatsResultType z = ResultItemSource $ tr z
  where
    tr z@(ResultItem n i (DoubleResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleTimingStatsResultData x) s) =
      z
    tr z@(ResultItem n i (IntResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntTimingStatsResultData x) s) =
      z { resultItemData = DoubleTimingStatsResultData $ fmap fromIntTimingStats x }
    tr z@(ResultItem n i (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem IntResultType z = ResultItemSource $ tr z
  where
    tr z@(ResultItem n i (DoubleResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntResultData x) s) =
      z
    tr z@(ResultItem n i (IntListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem IntListResultType z = ResultItemSource $ tr z
  where
    tr z@(ResultItem n i (DoubleResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntResultData x) s) =
      z { resultItemData = IntListResultData $ fmap return x }
    tr z@(ResultItem n i (IntListResultData x) s) =
      z
    tr z@(ResultItem n i (IntStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem IntStatsResultType z = ResultItemSource $ tr z
  where
    tr z@(ResultItem n i (DoubleResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntResultData x) s) =
      z { resultItemData = IntStatsResultData $ fmap returnSamplingStats x }
    tr z@(ResultItem n i (IntListResultData x) s) =
      z { resultItemData = IntStatsResultData $ fmap listSamplingStats x }
    tr z@(ResultItem n i (IntStatsResultData x) s) =
      z
    tr z@(ResultItem n i (IntTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem IntTimingStatsResultType z = ResultItemSource $ tr z
  where
    tr z@(ResultItem n i (DoubleResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (DoubleTimingStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n i (IntTimingStatsResultData x) s) =
      z
    tr z@(ResultItem n i (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem StringResultType z@(ResultItem n i (DoubleStatsResultData x) s) =
  mapResultItems (retypeResultItem StringResultType) $
  mapResultItems (\x -> ResultItemSource x { resultItemSignal = s }) $
  makeSamplingStatsSource DoubleResultData n i x
retypeResultItem StringResultType z@(ResultItem n i (DoubleTimingStatsResultData x) s) =
  mapResultItems (retypeResultItem StringResultType) $
  mapResultItems (\x -> ResultItemSource x { resultItemSignal = s }) $
  makeTimingStatsSource DoubleResultData n i x
retypeResultItem StringResultType z@(ResultItem n i (IntStatsResultData x) s) =
  mapResultItems (retypeResultItem StringResultType) $
  mapResultItems (\x -> ResultItemSource x { resultItemSignal = s }) $
  makeSamplingStatsSource IntResultData n i x
retypeResultItem StringResultType z@(ResultItem n i (IntTimingStatsResultData x) s) =
  mapResultItems (retypeResultItem StringResultType) $
  mapResultItems (\x -> ResultItemSource x { resultItemSignal = s }) $
  makeTimingStatsSource IntResultData n i x
retypeResultItem StringResultType z = ResultItemSource $ tr z
  where
    tr z@(ResultItem n i (DoubleResultData x) s) =
      z { resultItemData = StringResultData $ fmap show x }
    tr z@(ResultItem n i (DoubleListResultData x) s) =
      z { resultItemData = StringResultData $ fmap show x }
    tr z@(ResultItem n i (IntResultData x) s) =
      z { resultItemData = StringResultData $ fmap show x }
    tr z@(ResultItem n i (IntListResultData x) s) =
      z { resultItemData = StringResultData $ fmap show x }
    tr z@(ResultItem n i (StringResultData x) s) =
      z
retypeResultItem DefaultResultType z = ResultItemSource z

-- | Return the sources by the results using the desired data type.
retypeResults :: ResultType
                 -- ^ the data type in which we are going to receive the sources
                 -> Results
                 -- ^ the simulation results
                 -> [ResultSource]
retypeResults t results =
  map (mapResultItems $ retypeResultItem t) (resultSourceList results)

-- | It contains the results of simulation.
data Results =
  Results { resultSourceMap :: ResultSourceMap,
            -- ^ The sources of simulation results as a map of associated names.
            resultSourceList :: [ResultSource]
            -- ^ The sources of simulation results as an ordered list.
          }

-- | It representes the predefined signals provided by every simulation model.
data ResultPredefinedSignals =
  ResultPredefinedSignals { resultSignalInIntegTimes :: Signal Double,
                            -- ^ The signal triggered in the integration time points.
                            resultSignalInStartTime :: Signal Double,
                            -- ^ The signal triggered in the start time.
                            resultSignalInStopTime :: Signal Double
                            -- ^ The signal triggered in the stop time.
                          }

-- | Create the predefined signals provided by every simulation model.
newResultPredefinedSignals :: Simulation ResultPredefinedSignals
newResultPredefinedSignals = runDynamicsInStartTime $ runEventWith EarlierEvents d where
  d = do signalInIntegTimes <- newSignalInIntegTimes
         signalInStartTime  <- newSignalInStartTime
         signalInStopTime   <- newSignalInStopTime
         return ResultPredefinedSignals { resultSignalInIntegTimes = signalInIntegTimes,
                                          resultSignalInStartTime  = signalInStartTime,
                                          resultSignalInStopTime   = signalInStopTime }

-- | Prepare the simulation results.
results :: [ResultSource] -> Results
results m =
  Results { resultSourceMap  = M.fromList $ map (\x -> (resultSourceName x, x)) m,
            resultSourceList = m }

-- | Return a short version of the simulation results, i.e. their summary.
resultSummary :: Results -> Results
resultSummary xs =
  results (map resultSourceSummary $ resultSourceList xs)

-- | Return a mixed signal for the specified items received from 
-- the provided simulation results.
--
-- This signal is triggered when the item signals are triggered.
-- The mixed signal is also triggered in the integration time points
-- if there is at least one item without signal.
mixedResultItemSignal :: ResultPredefinedSignals -> [ResultItem] -> Signal ()
mixedResultItemSignal rs xs =
  let xs0 = map resultItemSignal xs
      xs1 = filter isJust xs0
      xs2 = filter isNothing xs0
      signal1 = mconcat $ map fromJust xs1
      signal2 = if null xs2 
                then signal3 <> signal4
                else signal5
      signal3 = void $ resultSignalInStartTime rs
      signal4 = void $ resultSignalInStopTime rs
      signal5 = void $ resultSignalInIntegTimes rs
  in signal1 <> signal2

-- | Lookup the mandatory result sources by the specified names.
lookupResultSources :: ResultSourceMap -> [ResultName] -> [ResultSource]
lookupResultSources xs names =
  flip map names $ \name ->
  case M.lookup name xs of
    Nothing -> 
      error $ 
      "No found result source with name " ++
      name ++ ": lookupResultSources"
    Just x -> x

-- | Represents a computation that can return the simulation data.
class ResultComputation m where

  -- | Extract data from the computation.
  resultComputationData :: m a -> Event a

  -- | Return the signal for the computation as possible.
  resultComputationSignal :: m a -> ResultSignal

instance ResultComputation Parameter where

  resultComputationData = liftParameter
  resultComputationSignal = const Nothing

instance ResultComputation Simulation where

  resultComputationData = liftSimulation
  resultComputationSignal = const Nothing

instance ResultComputation Dynamics where

  resultComputationData = liftDynamics
  resultComputationSignal = const Nothing

instance ResultComputation Event where

  resultComputationData = id
  resultComputationSignal = const Nothing

instance ResultComputation Ref where

  resultComputationData = readRef
  resultComputationSignal = Just . refChanged_

instance ResultComputation LR.Ref where

  resultComputationData = LR.readRef
  resultComputationSignal = const Nothing

instance ResultComputation Var where

  resultComputationData = readVar
  resultComputationSignal = Just . varChanged_

instance ResultComputation Signalable where

  resultComputationData = readSignalable
  resultComputationSignal = Just . signalableChanged_

-- | Make a result item source. 
makeResultItemSource :: ResultComputation m
                        => (Event a -> ResultData)
                        -- ^ transformation
                        -> ResultName
                        -- ^ the result name
                        -> ResultId
                        -- ^ the result identifier
                        -> m a
                        -- ^ the result computation
                        -> ResultSource
makeResultItemSource f name i m =
  ResultItemSource $
  ResultItem { resultItemName   = name,
               resultItemId     = i,
               resultItemData   = f $ resultComputationData m,
               resultItemSignal = resultComputationSignal m }

-- | Return the source by the specified statistics.
makeSamplingStatsSource :: (Show a, ResultComputation m)
                           => (Event a -> ResultData)
                           -- ^ transformation
                           -> ResultName
                           -- ^ the result name
                           -> ResultId
                           -- ^ the result indentifier
                           -> m (SamplingStats a)
                           -- ^ the statistics
                           -> ResultSource
makeSamplingStatsSource f name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = SamplingStatsId,
    resultObjectSummary =
      makeSamplingStatsSummary name i m,
    resultObjectProperties = [
      makeProperty "count" SamplingStatsCountId (IntResultData . fmap samplingStatsCount),
      makeProperty "mean" SamplingStatsMeanId (DoubleResultData . fmap samplingStatsMean),
      makeProperty "mean2" SamplingStatsMean2Id (DoubleResultData . fmap samplingStatsMean2),
      makeProperty "std" SamplingStatsDeviationId (DoubleResultData . fmap samplingStatsDeviation),
      makeProperty "var" SamplingStatsVarianceId (DoubleResultData . fmap samplingStatsVariance),
      makeProperty "min" SamplingStatsMinId (f . fmap samplingStatsMin),
      makeProperty "max" SamplingStatsMaxId (f . fmap samplingStatsMax) ] }
  where
    makeProperty name' i f =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i f }
    makeSource name' i f =
      ResultItemSource $
      ResultItem { resultItemName   = name ++ "." ++ name',
                   resultItemId     = i,
                   resultItemData   = f $ resultComputationData m,
                   resultItemSignal = resultComputationSignal m }
  
-- | Return the source by the specified timing statistics.
makeTimingStatsSource :: (Show a, TimingData a, ResultComputation m)
                         => (Event a -> ResultData)
                         -- ^ transformation
                         -> ResultName
                         -- ^ the result name
                         -> ResultId
                         -- ^ the result identifier
                         -> m (TimingStats a)
                         -- ^ the statistics
                         -> ResultSource
makeTimingStatsSource f name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = TimingStatsId,
    resultObjectSummary =
      makeTimingStatsSummary name i m,
    resultObjectProperties = [
      makeProperty "count" TimingStatsCountId (IntResultData . fmap timingStatsCount),
      makeProperty "mean" TimingStatsMeanId (DoubleResultData . fmap timingStatsMean),
      makeProperty "std" TimingStatsDeviationId (DoubleResultData . fmap timingStatsDeviation),
      makeProperty "var" TimingStatsVarianceId (DoubleResultData . fmap timingStatsVariance),
      makeProperty "min" TimingStatsMinId (f . fmap timingStatsMin),
      makeProperty "max" TimingStatsMaxId (f . fmap timingStatsMax),
      makeProperty "minTime" TimingStatsMinTimeId (DoubleResultData . fmap timingStatsMinTime),
      makeProperty "maxTime" TimingStatsMaxTimeId (DoubleResultData . fmap timingStatsMaxTime),
      makeProperty "startTime" TimingStatsStartTimeId (DoubleResultData . fmap timingStatsStartTime),
      makeProperty "lastTime" TimingStatsLastTimeId (DoubleResultData . fmap timingStatsLastTime),
      makeProperty "sum" TimingStatsSumId (DoubleResultData . fmap timingStatsSum),
      makeProperty "sum2" TimingStatsSum2Id (DoubleResultData . fmap timingStatsSum2) ] }
  where
    makeProperty name' i f =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i f }
    makeSource name' i f =
      ResultItemSource $
      ResultItem { resultItemName   = name ++ "." ++ name',
                   resultItemId     = i,
                   resultItemData   = f $ resultComputationData m,
                   resultItemSignal = resultComputationSignal m }
  
-- | Return the source by the specified (finite) queue.
makeQueueSource :: (Show si, Show sm, Show so)
                   => ResultName
                   -- ^ the result name
                   -> ResultId
                   -- ^ the result identifier
                   -> Q.Queue si qi sm qm so qo a
                   -- ^ the queue
                   -> ResultSource
makeQueueSource name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = FiniteQueueId,
    resultObjectSummary =
      makeQueueSummary name i m,
    resultObjectProperties = [
      makeProperty "enqueueStrategy" EnqueueStrategyId getEnqueueStrategy enqueueStrategySignal,
      makeProperty "enqueueStoringStrategy" EnqueueStoringStrategyId getEnqueueStoringStrategy enqueueStoringStrategySignal,
      makeProperty "dequeueStrategy" DequeueStrategyId getDequeueStrategy dequeueStrategySignal,
      makeProperty "queueNull" QueueNullId whetherIsEmpty whetherIsEmptySignal,
      makeProperty "queueFull" QueueFullId whetherIsFull whetherIsFullSignal,
      makeProperty "queueMaxCount" QueueMaxCountId getMaxCount maxCountSignal,
      makeProperty "queueCount" QueueCountId getCount countSignal,
      makeProperty "queueCountStats" QueueCountStatsId getCountStats countStatsSignal,
      makeProperty "enqueueCount" EnqueueCountId getEnqueueCount enqueueCountSignal,
      makeProperty "enqueueLostCount" EnqueueLostCountId getEnqueueLostCount enqueueLostCountSignal,
      makeProperty "enqueueStoreCount" EnqueueStoreCountId getEnqueueStoreCount enqueueStoreCountSignal,
      makeProperty "dequeueCount" DequeueCountId getDequeueCount dequeueCountSignal,
      makeProperty "dequeueExtractCount" DequeueExtractCountId getDequeueExtractCount dequeueExtractCountSignal,
      makeProperty "queueLoadFactor" QueueLoadFactorId getLoadFactor loadFactorSignal,
      makeProperty "enqueueRate" EnqueueRateId getEnqueueRate enqueueRateSignal,
      makeProperty "enqueueStoreRate" EnqueueStoreRateId getEnqueueStoreRate enqueueStoreRateSignal,
      makeProperty "dequeueRate" DequeueRateId getDequeueRate dequeueRateSignal,
      makeProperty "dequeueExtractRate" DequeueExtractRateId getDequeueExtractRate dequeueExtractRateSignal,
      makeProperty "queueWaitTime" QueueWaitTimeId getWaitTime waitTimeSignal,
      makeProperty "queueTotalWaitTime" QueueTotalWaitTimeId getTotalWaitTime totalWaitTimeSignal,
      makeProperty "enqueueWaitTime" EnqueueWaitTimeId getEnqueueWaitTime enqueueWaitTimeSignal,
      makeProperty "dequeueWaitTime" DequeueWaitTimeId getDequeueWaitTime dequeueWaitTimeSignal ] }
  where makeProperty name' i f g =
          ResultProperty { resultPropertyLabel = name',
                           resultPropertyId = i,
                           resultPropertySource = makeSource name' i f g }
        makeSource name' i f g =
          ResultItemSource $
          ResultItem { resultItemName   = name ++ "." ++ name',
                       resultItemId     = i,
                       resultItemData   = f m,
                       resultItemSignal = g m }
        -- properties
        getEnqueueStrategy = StringResultData . return . show . Q.enqueueStrategy
        getEnqueueStoringStrategy = StringResultData . return . show . Q.enqueueStoringStrategy
        getDequeueStrategy = StringResultData . return . show . Q.dequeueStrategy
        whetherIsEmpty = StringResultData . fmap show . Q.queueNull
        whetherIsFull = StringResultData . fmap show . Q.queueFull
        getMaxCount = IntResultData . return . Q.queueMaxCount
        getCount = IntResultData . Q.queueCount
        getCountStats = IntTimingStatsResultData . Q.queueCountStats
        getEnqueueCount = IntResultData . Q.enqueueCount
        getEnqueueLostCount = IntResultData . Q.enqueueLostCount
        getEnqueueStoreCount = IntResultData . Q.enqueueStoreCount
        getDequeueCount = IntResultData . Q.dequeueCount
        getDequeueExtractCount = IntResultData . Q.dequeueExtractCount
        getLoadFactor = DoubleResultData . Q.queueLoadFactor
        getEnqueueRate = DoubleResultData . Q.enqueueRate
        getEnqueueStoreRate = DoubleResultData . Q.enqueueStoreRate
        getDequeueRate = DoubleResultData . Q.dequeueRate
        getDequeueExtractRate = DoubleResultData . Q.dequeueExtractRate
        getWaitTime = DoubleStatsResultData . Q.queueWaitTime
        getTotalWaitTime = DoubleStatsResultData . Q.queueTotalWaitTime
        getEnqueueWaitTime = DoubleStatsResultData . Q.enqueueWaitTime
        getDequeueWaitTime = DoubleStatsResultData . Q.dequeueWaitTime
        -- signals
        enqueueStrategySignal = const Nothing
        enqueueStoringStrategySignal = const Nothing
        dequeueStrategySignal = const Nothing
        whetherIsEmptySignal = Just . Q.queueNullChanged_
        whetherIsFullSignal = Just . Q.queueFullChanged_
        maxCountSignal = const Nothing
        countSignal = Just . Q.queueCountChanged_
        countStatsSignal = Just . Q.queueCountChanged_
        enqueueCountSignal = Just . Q.enqueueCountChanged_
        enqueueLostCountSignal = Just . Q.enqueueLostCountChanged_
        enqueueStoreCountSignal = Just . Q.enqueueStoreCountChanged_
        dequeueCountSignal = Just . Q.dequeueCountChanged_
        dequeueExtractCountSignal = Just . Q.dequeueExtractCountChanged_
        loadFactorSignal = Just . Q.queueLoadFactorChanged_
        enqueueRateSignal = const Nothing
        enqueueStoreRateSignal = const Nothing
        dequeueRateSignal = const Nothing
        dequeueExtractRateSignal = const Nothing
        waitTimeSignal = Just . Q.queueWaitTimeChanged_
        totalWaitTimeSignal = Just . Q.queueTotalWaitTimeChanged_
        enqueueWaitTimeSignal = Just . Q.enqueueWaitTimeChanged_
        dequeueWaitTimeSignal = Just . Q.dequeueWaitTimeChanged_

-- | Return the source by the specified (infinite) queue.
makeInfiniteQueueSource :: (Show sm, Show so)
                           => ResultName
                           -- ^ the result name
                           -> ResultId
                           -- ^ the result identifier
                           -> IQ.Queue sm qm so qo a
                           -- ^ the queue
                           -> ResultSource
makeInfiniteQueueSource name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = InfiniteQueueId,
    resultObjectSummary =
      makeInfiniteQueueSummary name i m,
    resultObjectProperties = [
      makeProperty "enqueueStoringStrategy" EnqueueStoringStrategyId getEnqueueStoringStrategy enqueueStoringStrategySignal,
      makeProperty "dequeueStrategy" DequeueStrategyId getDequeueStrategy dequeueStrategySignal,
      makeProperty "queueNull" QueueNullId whetherIsEmpty whetherIsEmptySignal,
      makeProperty "queueCount" QueueCountId getCount countSignal,
      makeProperty "queueCountStats" QueueCountStatsId getCountStats countStatsSignal,
      makeProperty "enqueueStoreCount" EnqueueStoreCountId getEnqueueStoreCount enqueueStoreCountSignal,
      makeProperty "dequeueCount" DequeueCountId getDequeueCount dequeueCountSignal,
      makeProperty "dequeueExtractCount" DequeueExtractCountId getDequeueExtractCount dequeueExtractCountSignal,
      makeProperty "enqueueStoreRate" EnqueueStoreRateId getEnqueueStoreRate enqueueStoreRateSignal,
      makeProperty "dequeueRate" DequeueRateId getDequeueRate dequeueRateSignal,
      makeProperty "dequeueExtractRate" DequeueExtractRateId getDequeueExtractRate dequeueExtractRateSignal,
      makeProperty "queueWaitTime" QueueWaitTimeId getWaitTime waitTimeSignal,
      makeProperty "dequeueWaitTime" DequeueWaitTimeId getDequeueWaitTime dequeueWaitTimeSignal ] }
  where makeProperty name' i f g =
          ResultProperty { resultPropertyLabel = name',
                           resultPropertyId = i,
                           resultPropertySource = makeSource name' i f g }
        makeSource name' i f g =
          ResultItemSource $
          ResultItem { resultItemName   = name ++ "." ++ name',
                       resultItemId     = i,
                       resultItemData   = f m,
                       resultItemSignal = g m }
        -- properties
        getEnqueueStoringStrategy = StringResultData . return . show . IQ.enqueueStoringStrategy
        getDequeueStrategy = StringResultData . return . show . IQ.dequeueStrategy
        whetherIsEmpty = StringResultData . fmap show . IQ.queueNull
        getCount = IntResultData . IQ.queueCount
        getCountStats = IntTimingStatsResultData . IQ.queueCountStats
        getEnqueueStoreCount = IntResultData . IQ.enqueueStoreCount
        getDequeueCount = IntResultData . IQ.dequeueCount
        getDequeueExtractCount = IntResultData . IQ.dequeueExtractCount
        getEnqueueStoreRate = DoubleResultData . IQ.enqueueStoreRate
        getDequeueRate = DoubleResultData . IQ.dequeueRate
        getDequeueExtractRate = DoubleResultData . IQ.dequeueExtractRate
        getWaitTime = DoubleStatsResultData . IQ.queueWaitTime
        getDequeueWaitTime = DoubleStatsResultData . IQ.dequeueWaitTime
        -- signals
        enqueueStoringStrategySignal = const Nothing
        dequeueStrategySignal = const Nothing
        whetherIsEmptySignal = Just . IQ.queueNullChanged_
        countSignal = Just . IQ.queueCountChanged_
        countStatsSignal = Just . IQ.queueCountChanged_
        enqueueStoreCountSignal = Just . IQ.enqueueStoreCountChanged_
        dequeueCountSignal = Just . IQ.dequeueCountChanged_
        dequeueExtractCountSignal = Just . IQ.dequeueExtractCountChanged_
        enqueueStoreRateSignal = const Nothing
        dequeueRateSignal = const Nothing
        dequeueExtractRateSignal = const Nothing
        waitTimeSignal = Just . IQ.queueWaitTimeChanged_
        dequeueWaitTimeSignal = Just . IQ.dequeueWaitTimeChanged_
  
-- | Return the source by the specified arrival timer.
makeArrivalTimerSource :: ResultName
                          -- ^ the result name
                          -> ResultId
                          -- ^ the result identifier
                          -> ArrivalTimer
                          -- ^ the arrival timer
                          -> ResultSource
makeArrivalTimerSource name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = ArrivalTimerId,
    resultObjectSummary =
      makeArrivalTimerSummary name i m,
    resultObjectProperties = [
      makeProperty "processingTime" ArrivalProcessingTimeId getProcessingTime processingTimeChanged ] }
  where makeProperty name' i f g =
          ResultProperty { resultPropertyLabel = name',
                           resultPropertyId = i,
                           resultPropertySource = makeSource name' i f g }
        makeSource name' i f g =
          ResultItemSource $
          ResultItem { resultItemName   = name ++ "." ++ name',
                       resultItemId     = i,
                       resultItemData   = f m,
                       resultItemSignal = g m }
        -- properties
        getProcessingTime = DoubleStatsResultData . arrivalProcessingTime
        -- signals
        processingTimeChanged = Just . arrivalProcessingTimeChanged_

-- | Return the source by the specified server.
makeServerSource :: Show s
                    => ResultName
                    -- ^ the result name
                    -> ResultId
                    -- ^ the result identifier
                    -> Server s a b
                    -- ^ the server
                    -> ResultSource
makeServerSource name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = ServerId,
    resultObjectSummary =
      makeServerSummary name i m,
    resultObjectProperties = [
      makeProperty "initState" ServerInitStateId getInitState initStateChanged,
      makeProperty "state" ServerStateId getState stateChanged,
      makeProperty "totalInputWaitTime" ServerTotalInputWaitTimeId getTotalInputWaitTime totalInputWaitTimeChanged,
      makeProperty "totalProcessingTime" ServerTotalProcessingTimeId getTotalProcessingTime totalProcessingTimeChanged,
      makeProperty "totalOutputWaitTime" ServerTotalOutputWaitTimeId getTotalOutputWaitTime totalOutputWaitTimeChanged,
      makeProperty "inputWaitTime" ServerInputWaitTimeId getInputWaitTime inputWaitTimeChanged,
      makeProperty "processingTime" ServerProcessingTimeId getProcessingTime processingTimeChanged,
      makeProperty "outputWaitTime" ServerOutputWaitTimeId getOutputWaitTime outputWaitTimeChanged,
      makeProperty "inputWaitFactor" ServerInputWaitFactorId getInputWaitFactor inputWaitFactorChanged,
      makeProperty "processingFactor" ServerProcessingFactorId getProcessingFactor processingFactorChanged,
      makeProperty "outputWaitFactor" ServerOutputWaitFactorId getOutputWaitFactor outputWaitFactorChanged ] }
  where makeProperty name' i f g =
          ResultProperty { resultPropertyLabel = name',
                           resultPropertyId = i,
                           resultPropertySource = makeSource name' i f g }
        makeSource name' i f g =
          ResultItemSource $
          ResultItem { resultItemName   = name ++ "." ++ name',
                       resultItemId     = i,
                       resultItemData   = f m,
                       resultItemSignal = g m }
        -- properties
        getInitState = StringResultData . return . show . serverInitState 
        getState = StringResultData . fmap show . serverState
        getTotalInputWaitTime = DoubleResultData . serverTotalInputWaitTime
        getTotalProcessingTime = DoubleResultData . serverTotalProcessingTime
        getTotalOutputWaitTime = DoubleResultData . serverTotalOutputWaitTime
        getInputWaitTime = DoubleStatsResultData . serverInputWaitTime
        getProcessingTime = DoubleStatsResultData . serverProcessingTime
        getOutputWaitTime = DoubleStatsResultData . serverOutputWaitTime
        getInputWaitFactor = DoubleResultData . serverInputWaitFactor
        getProcessingFactor = DoubleResultData . serverProcessingFactor
        getOutputWaitFactor = DoubleResultData . serverOutputWaitFactor
        -- signals
        initStateChanged = const Nothing
        stateChanged = Just . serverStateChanged_
        totalInputWaitTimeChanged = Just . serverTotalInputWaitTimeChanged_
        totalProcessingTimeChanged = Just . serverTotalProcessingTimeChanged_
        totalOutputWaitTimeChanged = Just . serverTotalOutputWaitTimeChanged_
        inputWaitTimeChanged = Just . serverInputWaitTimeChanged_
        processingTimeChanged = Just . serverProcessingTimeChanged_
        outputWaitTimeChanged = Just . serverOutputWaitTimeChanged_
        inputWaitFactorChanged = Just . serverInputWaitFactorChanged_
        processingFactorChanged = Just . serverProcessingFactorChanged_
        outputWaitFactorChanged = Just . serverOutputWaitFactorChanged_

-- | Return an arbitrary text as a separator source.
makeTextSource :: String -> ResultSource
makeTextSource text =
  ResultSeparatorSource $
  ResultSeparator { resultSeparatorText = text }

-- | Return the source of the modeling time.
timeSource :: ResultSource
timeSource = resultSource' "t" TimeId time

-- | Return the summary by the specified statistics.
makeSamplingStatsSummary :: (Show a, ResultComputation m)
                            => ResultName
                            -- ^ the result name
                            -> ResultId
                            -- ^ the result indentifier
                            -> m (SamplingStats a)
                            -- ^ the statistics
                           -> ResultSource
makeSamplingStatsSummary =
  makeResultItemSource (StringResultData . fmap show)

-- | Return the summary by the specified timing statistics.
makeTimingStatsSummary :: (Show a, TimingData a, ResultComputation m)
                          => ResultName
                          -- ^ the result name
                          -> ResultId
                          -- ^ the result indentifier
                          -> m (TimingStats a)
                          -- ^ the statistics
                          -> ResultSource
makeTimingStatsSummary =
  makeResultItemSource (StringResultData . fmap show)
  
-- | Return the summary by the specified (finite) queue.
makeQueueSummary :: (Show si, Show sm, Show so)
                    => ResultName
                    -- ^ the result name
                    -> ResultId
                    -- ^ the result identifier
                    -> Q.Queue si qi sm qm so qo a
                    -- ^ the queue
                    -> ResultSource
makeQueueSummary name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = FiniteQueueId,
    resultObjectSummary =
      makeQueueSummary name i m,
    resultObjectProperties = [
      makeProperty "queueMaxCount" QueueMaxCountId getMaxCount maxCountSignal,
      makeProperty "queueCountStats" QueueCountStatsId getCountStats countStatsSignal,
      makeProperty "enqueueCount" EnqueueCountId getEnqueueCount enqueueCountSignal,
      makeProperty "enqueueLostCount" EnqueueLostCountId getEnqueueLostCount enqueueLostCountSignal,
      makeProperty "enqueueStoreCount" EnqueueStoreCountId getEnqueueStoreCount enqueueStoreCountSignal,
      makeProperty "dequeueCount" DequeueCountId getDequeueCount dequeueCountSignal,
      makeProperty "dequeueExtractCount" DequeueExtractCountId getDequeueExtractCount dequeueExtractCountSignal,
      makeProperty "queueLoadFactor" QueueLoadFactorId getLoadFactor loadFactorSignal,
      makeProperty "queueWaitTime" QueueWaitTimeId getWaitTime waitTimeSignal ] }
  where makeProperty name' i f g =
          ResultProperty { resultPropertyLabel = name',
                           resultPropertyId = i,
                           resultPropertySource = makeSource name' i f g }
        makeSource name' i f g =
          ResultItemSource $
          ResultItem { resultItemName   = name ++ "." ++ name',
                       resultItemId     = i,
                       resultItemData   = f m,
                       resultItemSignal = g m }
        -- properties
        getMaxCount = IntResultData . return . Q.queueMaxCount
        getCountStats = StringResultData . fmap show . Q.queueCountStats
        getEnqueueCount = IntResultData . Q.enqueueCount
        getEnqueueLostCount = IntResultData . Q.enqueueLostCount
        getEnqueueStoreCount = IntResultData . Q.enqueueStoreCount
        getDequeueCount = IntResultData . Q.dequeueCount
        getDequeueExtractCount = IntResultData . Q.dequeueExtractCount
        getLoadFactor = DoubleResultData . Q.queueLoadFactor
        getWaitTime = StringResultData . fmap show . Q.queueWaitTime
        -- signals
        maxCountSignal = const Nothing
        countStatsSignal = Just . Q.queueCountChanged_
        enqueueCountSignal = Just . Q.enqueueCountChanged_
        enqueueLostCountSignal = Just . Q.enqueueLostCountChanged_
        enqueueStoreCountSignal = Just . Q.enqueueStoreCountChanged_
        dequeueCountSignal = Just . Q.dequeueCountChanged_
        dequeueExtractCountSignal = Just . Q.dequeueExtractCountChanged_
        loadFactorSignal = Just . Q.queueLoadFactorChanged_
        waitTimeSignal = Just . Q.queueWaitTimeChanged_
  
-- | Return the summary by the specified (infinite) queue.
makeInfiniteQueueSummary :: (Show sm, Show so)
                            => ResultName
                            -- ^ the result name
                            -> ResultId
                            -- ^ the result identifier
                            -> IQ.Queue sm qm so qo a
                            -- ^ the queue
                            -> ResultSource
makeInfiniteQueueSummary name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = InfiniteQueueId,
    resultObjectSummary =
      makeInfiniteQueueSummary name i m,
    resultObjectProperties = [
      makeProperty "queueCountStats" QueueCountStatsId getCountStats countStatsSignal,
      makeProperty "enqueueStoreCount" EnqueueStoreCountId getEnqueueStoreCount enqueueStoreCountSignal,
      makeProperty "dequeueCount" DequeueCountId getDequeueCount dequeueCountSignal,
      makeProperty "dequeueExtractCount" DequeueExtractCountId getDequeueExtractCount dequeueExtractCountSignal,
      makeProperty "queueWaitTime" QueueWaitTimeId getWaitTime waitTimeSignal ] }
  where makeProperty name' i f g =
          ResultProperty { resultPropertyLabel = name',
                           resultPropertyId = i,
                           resultPropertySource = makeSource name' i f g }
        makeSource name' i f g =
          ResultItemSource $
          ResultItem { resultItemName   = name ++ "." ++ name',
                       resultItemId     = i,
                       resultItemData   = f m,
                       resultItemSignal = g m }
        -- properties
        getCountStats = StringResultData . fmap show . IQ.queueCountStats
        getEnqueueStoreCount = IntResultData . IQ.enqueueStoreCount
        getDequeueCount = IntResultData . IQ.dequeueCount
        getDequeueExtractCount = IntResultData . IQ.dequeueExtractCount
        getWaitTime = StringResultData . fmap show . IQ.queueWaitTime
        -- signals
        countStatsSignal = Just . IQ.queueCountChanged_
        enqueueStoreCountSignal = Just . IQ.enqueueStoreCountChanged_
        dequeueCountSignal = Just . IQ.dequeueCountChanged_
        dequeueExtractCountSignal = Just . IQ.dequeueExtractCountChanged_
        waitTimeSignal = Just . IQ.queueWaitTimeChanged_
  
-- | Return the summary by the specified arrival timer.
makeArrivalTimerSummary :: ResultName
                           -- ^ the result name
                           -> ResultId
                           -- ^ the result identifier
                           -> ArrivalTimer
                           -- ^ the arrival timer
                           -> ResultSource
makeArrivalTimerSummary name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = ArrivalTimerId,
    resultObjectSummary =
      makeArrivalTimerSummary name i m,
    resultObjectProperties = [
      makeProperty "processingTime" ArrivalProcessingTimeId getProcessingTime processingTimeChanged ] }
  where makeProperty name' i f g =
          ResultProperty { resultPropertyLabel = name',
                           resultPropertyId = i,
                           resultPropertySource = makeSource name' i f g }
        makeSource name' i f g =
          ResultItemSource $
          ResultItem { resultItemName   = name ++ "." ++ name',
                       resultItemId     = i,
                       resultItemData   = f m,
                       resultItemSignal = g m }
        -- properties
        getProcessingTime = StringResultData . fmap show . arrivalProcessingTime
        -- signals
        processingTimeChanged = Just . arrivalProcessingTimeChanged_

-- | Return the summary by the specified server.
makeServerSummary :: Show s
                     => ResultName
                     -- ^ the result name
                     -> ResultId
                     -- ^ the result identifier
                     -> Server s a b
                     -- ^ the server
                     -> ResultSource
makeServerSummary name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = ServerId,
    resultObjectSummary =
      makeServerSummary name i m,
    resultObjectProperties = [
      makeProperty "inputWaitTime" ServerInputWaitTimeId getInputWaitTime inputWaitTimeChanged,
      makeProperty "processingTime" ServerProcessingTimeId getProcessingTime processingTimeChanged,
      makeProperty "outputWaitTime" ServerOutputWaitTimeId getOutputWaitTime outputWaitTimeChanged,
      makeProperty "inputWaitFactor" ServerInputWaitFactorId getInputWaitFactor inputWaitFactorChanged,
      makeProperty "processingFactor" ServerProcessingFactorId getProcessingFactor processingFactorChanged,
      makeProperty "outputWaitFactor" ServerOutputWaitFactorId getOutputWaitFactor outputWaitFactorChanged ] }
  where makeProperty name' i f g =
          ResultProperty { resultPropertyLabel = name',
                           resultPropertyId = i,
                           resultPropertySource = makeSource name' i f g }
        makeSource name' i f g =
          ResultItemSource $
          ResultItem { resultItemName   = name ++ "." ++ name',
                       resultItemId     = i,
                       resultItemData   = f m,
                       resultItemSignal = g m }
        -- properties
        getInputWaitTime = StringResultData . fmap show . serverInputWaitTime
        getProcessingTime = StringResultData . fmap show . serverProcessingTime
        getOutputWaitTime = StringResultData . fmap show . serverOutputWaitTime
        getInputWaitFactor = DoubleResultData . serverInputWaitFactor
        getProcessingFactor = DoubleResultData . serverProcessingFactor
        getOutputWaitFactor = DoubleResultData . serverOutputWaitFactor
        -- signals
        inputWaitTimeChanged = Just . serverInputWaitTimeChanged_
        processingTimeChanged = Just . serverProcessingTimeChanged_
        outputWaitTimeChanged = Just . serverOutputWaitTimeChanged_
        inputWaitFactorChanged = Just . serverInputWaitFactorChanged_
        processingFactorChanged = Just . serverProcessingFactorChanged_
        outputWaitFactorChanged = Just . serverOutputWaitFactorChanged_

-- | Make an integer subscript
makeIntSubscript :: Show a => a -> String
makeIntSubscript i = "[" ++ show i ++ "]"

instance ResultComputation m => ResultProvider (m Double) where

  resultSource' = makeResultItemSource DoubleResultData

instance ResultComputation m => ResultProvider (m [Double]) where

  resultSource' = makeResultItemSource DoubleListResultData

instance ResultComputation m => ResultProvider (m (SamplingStats Double)) where

  resultSource' = makeResultItemSource DoubleStatsResultData

instance ResultComputation m => ResultProvider (m (TimingStats Double)) where

  resultSource' = makeResultItemSource DoubleTimingStatsResultData

instance ResultComputation m => ResultProvider (m Int) where

  resultSource' = makeResultItemSource IntResultData

instance ResultComputation m => ResultProvider (m [Int]) where

  resultSource' = makeResultItemSource IntListResultData

instance ResultComputation m => ResultProvider (m (SamplingStats Int)) where

  resultSource' = makeResultItemSource IntStatsResultData

instance ResultComputation m => ResultProvider (m (TimingStats Int)) where

  resultSource' = makeResultItemSource IntTimingStatsResultData

instance ResultComputation m => ResultProvider (m String) where

  resultSource' = makeResultItemSource StringResultData

instance ResultProvider p => ResultProvider [p] where

  resultSource' name i m =
    resultSource' name i $ ResultListWithSubscript m subscript where
      subscript = map snd $ zip m $ map makeIntSubscript [0..]

instance (Show i, Ix i, ResultProvider p) => ResultProvider (A.Array i p) where

  resultSource' name i m =
    resultSource' name i $ ResultListWithSubscript items subscript where
      items = A.elems m
      subscript = map show (A.indices m)

instance ResultProvider p => ResultProvider (V.Vector p) where

  resultSource' name i m =
    resultSource' name i $ ResultVectorWithSubscript m subscript where
      subscript = V.imap (\i x -> makeIntSubscript i) m

-- | Represents a list with the specified subscript.
data ResultListWithSubscript p =
  ResultListWithSubscript [p] [String]

-- | Represents an array with the specified subscript.
data ResultArrayWithSubscript i p =
  ResultArrayWithSubscript (A.Array i p) (A.Array i String)

-- | Represents a vector with the specified subscript.
data ResultVectorWithSubscript p =
  ResultVectorWithSubscript (V.Vector p) (V.Vector String)

instance ResultProvider p => ResultProvider (ResultListWithSubscript p) where

  resultSource' name i (ResultListWithSubscript xs ys) =
    ResultVectorSource $
    ResultVector { resultVectorName = name,
                   resultVectorId = i,
                   resultVectorItems = V.fromList items,
                   resultVectorSubscript = V.fromList ys }
    where
      items =
        flip map (zip ys xs) $ \(y, x) ->
        let name' = name ++ y
        in resultSource' name' (VectorItemId y) x
    
instance (Show i, Ix i, ResultProvider p) => ResultProvider (ResultArrayWithSubscript i p) where

  resultSource' name i (ResultArrayWithSubscript xs ys) =
    resultSource' name i $ ResultListWithSubscript items subscript where
      items = A.elems xs
      subscript = A.elems ys

instance ResultProvider p => ResultProvider (ResultVectorWithSubscript p) where

  resultSource' name i (ResultVectorWithSubscript xs ys) =
    ResultVectorSource $
    ResultVector { resultVectorName = name,
                   resultVectorId = i,
                   resultVectorItems = items,
                   resultVectorSubscript = ys }
    where
      items =
        V.generate (V.length xs) $ \i ->
        let x = xs V.! i
            y = ys V.! i
            name' = name ++ y
        in resultSource' name' (VectorItemId y) x

instance (Ix i, Show i, ResultComputation m) => ResultProvider (m (A.Array i Double)) where

  resultSource' = makeResultItemSource (DoubleListResultData . fmap A.elems)

instance (Ix i, Show i, ResultComputation m) => ResultProvider (m (A.Array i Int)) where

  resultSource' = makeResultItemSource (IntListResultData . fmap A.elems)

instance ResultComputation m => ResultProvider (m (V.Vector Double)) where

  resultSource' = makeResultItemSource (DoubleListResultData . fmap V.toList)

instance ResultComputation m => ResultProvider (m (V.Vector Int)) where

  resultSource' = makeResultItemSource (IntListResultData . fmap V.toList)

instance (Show si, Show sm, Show so) => ResultProvider (Q.Queue si qi sm qm so qo a) where

  resultSource' = makeQueueSource

instance (Show sm, Show so) => ResultProvider (IQ.Queue sm qm so qo a) where

  resultSource' = makeInfiniteQueueSource

instance ResultProvider ArrivalTimer where

  resultSource' = makeArrivalTimerSource

instance Show s => ResultProvider (Server s a b) where

  resultSource' = makeServerSource
