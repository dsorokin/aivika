
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Results
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
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
import Simulation.Aivika.Ref
import qualified Simulation.Aivika.Ref.Light as LR
import Simulation.Aivika.Var
import qualified Simulation.Aivika.Queue as Q
import qualified Simulation.Aivika.Queue.Infinite as IQ

-- | A locale to output the simulation results.
--
-- Examples are: @\"ru\", @\"en\" etc.
type ResultLocale = String

-- | It localises the output of simulation results.
type ResultLocalisation = ResultId -> String

-- | A lable used for indentifying the results when generating output.
type ResultLabel = String

-- | The result entity identifier.
data ResultId = SamplingStatsId
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
              | LocalisedResultId (M.Map ResultLocale String)
                -- ^ A localised property or object name.

-- | Represents a provider of the simulation results. It is usually something, or
-- an array of something, or a list of such values which can be simulated to get data.
class ResultProvider p where
  
  -- | Return the source of simulation results with the specified name. 
  resultSource :: String -> p -> ResultSource

-- | Specifies the type of results we want to receive.
data ResultType = DoubleResultType
                  -- ^ Return double numbers in time points.
                | DoubleListResultType
                  -- ^ Return lists of double numbers in time points.
                | DoubleStatsResultType
                  -- ^ Return statistics based on double numbers.
                | IntResultType
                  -- ^ Return integer numbers in time points.
                | IntListResultType
                  -- ^ Return lists of integer numbers in time points.
                | IntStatsResultType
                  -- ^ Return statistics based on integer numbers.
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
                | IntResultData (Event Int)
                  -- ^ Contains the integer numbers in time points.
                | IntListResultData (Event [Int])
                  -- ^ Contains the lists of integer numbers in time points.
                | IntStatsResultData (Event (SamplingStats Int))
                  -- ^ Contains the statistics based on integer numbers.
                | StringResultData (Event String)
                  -- ^ Contains the string representations in time.
                | NoResultData
                  -- ^ Cannot return data.

-- | Whether an object containing the results emits a signal notifying about change of data.
type ResultSignal = Maybe (Signal ())
                  
-- | Defines a source that actually returns simulation results.
data ResultSource =
  ResultSource { resultOutput :: ResultType -> ResultOutput
                 -- ^ Output the simulation results.
               }

-- | It associates the result sources with their labels.
type ResultSourceMap = M.Map ResultLabel ResultSource

-- | Defines an output of the simulation results.
data ResultOutput = ResultItemOutput ResultItem
                    -- ^ The item output.
                  | ResultObjectOutput ResultObject
                    -- ^ The object output.
                  | ResultVectorOutput ResultVector
                    -- ^ The vector output.
                  | ResultSeparatorOutput ResultSeparator
                    -- ^ This is a separator output.

-- | The simulation results represented by a single item.
data ResultItem =
  ResultItem { resultItemName :: String,
               -- ^ The item name.
               resultItemData :: ResultData,
               -- ^ The item data.
               resultItemSignal :: ResultSignal
               -- ^ Whether the item emits a signal.
             }
  
-- | The simulation results represented by an object having properties.
data ResultObject =
  ResultObject { resultObjectName :: String,
                 -- ^ The object name.
                 resultObjectId :: ResultId,
                 -- ^ The object identifier.
                 resultObjectProperties :: [ResultProperty]
                 -- ^ The object properties.
               }

-- | The simulation results represented by a property of the object.
data ResultProperty =
  ResultProperty { resultPropertyLabel :: String,
                   -- ^ The property label.
                   resultPropertyId :: ResultId,
                   -- ^ The property identifier.
                   resultPropertyOutput :: ResultOutput
                   -- ^ An output supplied by the property.
                 }

-- | The simulation results represented by a vector.
data ResultVector =
  ResultVector { resultVectorName :: String,
                 -- ^ The vector name.
                 resultVectorItems :: V.Vector ResultOutput,
                 -- ^ The vector items.
                 resultVectorSubscript :: V.Vector String
                 -- ^ The subscript used as a suffix to create item names.
               }

-- | It separates the simulation results when printing.
data ResultSeparator =
  ResultSeparator { resultSeparatorText :: String
                    -- ^ The separator text.
                  }

-- | Flatten the result items.
flattenResultItems :: ResultOutput -> [ResultItem]
flattenResultItems (ResultItemOutput x) = [x]
flattenResultItems (ResultObjectOutput x) =
  concat $ map (flattenResultItems . resultPropertyOutput) $ resultObjectProperties x
flattenResultItems (ResultVectorOutput x) =
  concat $ map flattenResultItems $ V.toList $ resultVectorItems x
flattenResultItems (ResultSeparatorOutput x) = []

-- | Transform the result items using the specified function.
mapResultItems :: (ResultItem -> ResultOutput) -> ResultOutput -> ResultOutput
mapResultItems f (ResultItemOutput x) = f x
mapResultItems f (ResultObjectOutput x) =
  ResultObjectOutput $
  x { resultObjectProperties =
         flip map (resultObjectProperties x) $ \p ->
         p { resultPropertyOutput = mapResultItems f (resultPropertyOutput p) }
    }
mapResultItems f (ResultVectorOutput x) =
  ResultVectorOutput $
  x { resultVectorItems =
         V.map (mapResultItems f) (resultVectorItems x)
    }
mapResultItems f z@(ResultSeparatorOutput x) = z

-- | Retype the result item changing the data type and probably even the data structure. 
retypeResultItem :: ResultType -> ResultItem -> ResultOutput
retypeResultItem DoubleResultType z = ResultItemOutput $ tr z
  where
    tr z@(ResultItem n (DoubleResultData x) s) =
      z
    tr z@(ResultItem n (DoubleListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (IntResultData x) s) =
      z { resultItemData = DoubleResultData $ fmap fromIntegral x }
    tr z@(ResultItem n (IntListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (IntStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem DoubleListResultType z = ResultItemOutput $ tr z
  where
    tr z@(ResultItem n (DoubleResultData x) s) =
      z { resultItemData = DoubleListResultData $ fmap return x }
    tr z@(ResultItem n (DoubleListResultData x) s) =
      z
    tr z@(ResultItem n (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (IntResultData x) s) =
      z { resultItemData = DoubleListResultData $ fmap (return . fromIntegral) x }
    tr z@(ResultItem n (IntListResultData x) s) =
      z { resultItemData = DoubleListResultData $ fmap (fmap fromIntegral) x }
    tr z@(ResultItem n (IntStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem DoubleStatsResultType z = ResultItemOutput $ tr z
  where
    tr z@(ResultItem n (DoubleResultData x) s) =
      z { resultItemData = DoubleStatsResultData $ fmap returnSamplingStats x }
    tr z@(ResultItem n (DoubleListResultData x) s) =
      z { resultItemData = DoubleStatsResultData $ fmap listSamplingStats x }
    tr z@(ResultItem n (DoubleStatsResultData x) s) =
      z
    tr z@(ResultItem n (IntResultData x) s) =
      z { resultItemData = DoubleStatsResultData $ fmap (fromIntSamplingStats . returnSamplingStats) x }
    tr z@(ResultItem n (IntListResultData x) s) =
      z { resultItemData = DoubleStatsResultData $ fmap (fromIntSamplingStats . listSamplingStats) x }
    tr z@(ResultItem n (IntStatsResultData x) s) =
      z { resultItemData = DoubleStatsResultData $ fmap fromIntSamplingStats x }
    tr z@(ResultItem n (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem IntResultType z = ResultItemOutput $ tr z
  where
    tr z@(ResultItem n (DoubleResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (DoubleListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (IntResultData x) s) =
      z
    tr z@(ResultItem n (IntListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (IntStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem IntListResultType z = ResultItemOutput $ tr z
  where
    tr z@(ResultItem n (DoubleResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (DoubleListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (IntResultData x) s) =
      z { resultItemData = IntListResultData $ fmap return x }
    tr z@(ResultItem n (IntListResultData x) s) =
      z
    tr z@(ResultItem n (IntStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem IntStatsResultType z = ResultItemOutput $ tr z
  where
    tr z@(ResultItem n (DoubleResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (DoubleListResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (DoubleStatsResultData x) s) =
      z { resultItemData = NoResultData }
    tr z@(ResultItem n (IntResultData x) s) =
      z { resultItemData = IntStatsResultData $ fmap returnSamplingStats x }
    tr z@(ResultItem n (IntListResultData x) s) =
      z { resultItemData = IntStatsResultData $ fmap listSamplingStats x }
    tr z@(ResultItem n (IntStatsResultData x) s) =
      z
    tr z@(ResultItem n (StringResultData x) s) =
      z { resultItemData = NoResultData }
retypeResultItem StringResultType z@(ResultItem n (DoubleStatsResultData x) s) =
  mapResultItems (retypeResultItem StringResultType) $
  mapResultItems (\x -> ResultItemOutput x { resultItemSignal = s }) $
  makeSamplingStatsOutput DoubleResultData n x
retypeResultItem StringResultType z@(ResultItem n (IntStatsResultData x) s) =
  mapResultItems (retypeResultItem StringResultType) $
  mapResultItems (\x -> ResultItemOutput x { resultItemSignal = s }) $
  makeSamplingStatsOutput IntResultData n x
retypeResultItem StringResultType z = ResultItemOutput $ tr z
  where
    tr z@(ResultItem n (DoubleResultData x) s) =
      z { resultItemData = StringResultData $ fmap show x }
    tr z@(ResultItem n (DoubleListResultData x) s) =
      z { resultItemData = StringResultData $ fmap show x }
    tr z@(ResultItem n (IntResultData x) s) =
      z { resultItemData = StringResultData $ fmap show x }
    tr z@(ResultItem n (IntListResultData x) s) =
      z { resultItemData = StringResultData $ fmap show x }
    tr z@(ResultItem n (StringResultData x) s) =
      z
retypeResultItem DefaultResultType z = ResultItemOutput z

-- | It contains the results of simulation.
data Results =
  Results { resultPredefinedSignals :: ResultPredefinedSignals,
            -- ^ The predefined signals provided by every simulation model.
            resultSources :: ResultSourceMap
            -- ^ The sources of simulation results.
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

-- | Prepare the simulation results starting from the initial modeling time.
resultsFromStartTime :: [(ResultLabel, ResultSource)] -> Simulation Results
resultsFromStartTime m =
  do s <- newResultPredefinedSignals
     return Results { resultPredefinedSignals = s,
                      resultSources           = M.fromList m }

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

-- | Lookup the mandatory result sources by the specified labels.
lookupResultSources :: ResultSourceMap -> [ResultLabel] -> [ResultSource]
lookupResultSources xs labels =
  flip map labels $ \label ->
  case M.lookup label xs of
    Nothing -> 
      error $ 
      "No found result source with label " ++
      label ++ ": lookupResultSources"
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

-- | Generate the result source by output retyping and restructuring the data if required.
generateResultSource :: (String -> a -> ResultOutput) -> (String -> a -> ResultSource)
generateResultSource f name m =
  ResultSource $ \t -> mapResultItems (retypeResultItem t) (f name m)

-- | Make the result source by the specified transformation function.
makeResultSource :: ResultComputation m
                    => (Event a -> ResultData)
                    -- ^ transformation
                    -> String
                    -- ^ the result name
                    -> m a
                    -- ^ the result computation
                    -> ResultSource
makeResultSource f = generateResultSource $ makeResultItemOutput f

-- | Make a result item output. 
makeResultItemOutput :: ResultComputation m
                        => (Event a -> ResultData)
                        -- ^ transformation
                        -> String
                        -- ^ the result name
                        -> m a
                        -- ^ the result computation
                        -> ResultOutput
makeResultItemOutput f name m =
  ResultItemOutput $
  ResultItem { resultItemName   = name,
               resultItemData   = f $ resultComputationData m,
               resultItemSignal = resultComputationSignal m }

-- | Output the specified statistics.
makeSamplingStatsOutput :: (Show a, ResultComputation m)
                           => (Event a -> ResultData)
                           -- ^ transformation
                           -> String
                           -- ^ the result name
                           -> m (SamplingStats a)
                           -- ^ the statistics
                           -> ResultOutput
makeSamplingStatsOutput f name m =
  ResultObjectOutput $
  ResultObject {
    resultObjectName = name,
    resultObjectId = SamplingStatsId,
    resultObjectProperties = [
      ResultProperty {
         resultPropertyLabel = "count",
         resultPropertyId = SamplingStatsCountId,
         resultPropertyOutput =
           makeOutput (name ++ ".count") (IntResultData . fmap samplingStatsCount) },
      ResultProperty {
        resultPropertyLabel = "mean",
        resultPropertyId = SamplingStatsMeanId,
        resultPropertyOutput =
          makeOutput (name ++ ".mean") (DoubleResultData . fmap samplingStatsMean) },
      ResultProperty {
        resultPropertyLabel = "mean2",
        resultPropertyId = SamplingStatsMean2Id,
        resultPropertyOutput =
          makeOutput (name ++ ".mean2") (DoubleResultData . fmap samplingStatsMean2) },
      ResultProperty {
        resultPropertyLabel = "std",
        resultPropertyId = SamplingStatsDeviationId,
        resultPropertyOutput =
          makeOutput (name ++ ".std") (DoubleResultData . fmap samplingStatsDeviation) },
      ResultProperty {
        resultPropertyLabel = "var",
        resultPropertyId = SamplingStatsVarianceId,
        resultPropertyOutput =
          makeOutput (name ++ ".var") (DoubleResultData . fmap samplingStatsVariance) },
      ResultProperty {
        resultPropertyLabel = "min",
        resultPropertyId = SamplingStatsMinId,
        resultPropertyOutput =
          makeOutput (name ++ ".min") (f . fmap samplingStatsMin) },
      ResultProperty {
        resultPropertyLabel = "max",
        resultPropertyId = SamplingStatsMaxId,
        resultPropertyOutput =
          makeOutput (name ++ ".max") (f . fmap samplingStatsMax) } ] }
  where makeOutput name' f =
          ResultItemOutput $
          ResultItem { resultItemName   = name',
                       resultItemData   = f $ resultComputationData m,
                       resultItemSignal = resultComputationSignal m }

-- | Output the specified (finite) queue.
makeQueueOutput :: (Show si, Show sm, Show so)
                   => String
                   -- ^ the result name
                   -> Q.Queue si qi sm qm so qo a
                   -- ^ the queue
                   -> ResultOutput
makeQueueOutput name queue =
  ResultObjectOutput $
  ResultObject {
    resultObjectName = name,
    resultObjectId = FiniteQueueId,
    resultObjectProperties = [
      ResultProperty {
         resultPropertyLabel = "enqueueStrategy",
         resultPropertyId = EnqueueStrategyId,
         resultPropertyOutput =
           makeOutput (name ++ ".enqueueStrategy") getEnqueueStrategy enqueueStrategySignal  },
      ResultProperty {
         resultPropertyLabel = "enqueueStoringStrategy",
         resultPropertyId = EnqueueStoringStrategyId,
         resultPropertyOutput =
           makeOutput (name ++ ".enqueueStoringStrategy") getEnqueueStoringStrategy enqueueStoringStrategySignal},
      ResultProperty {
         resultPropertyLabel = "dequeueStrategy",
         resultPropertyId = DequeueStrategyId,
         resultPropertyOutput =
           makeOutput (name ++ ".dequeueStrategy") getDequeueStrategy dequeueStrategySignal },
      ResultProperty {
         resultPropertyLabel = "queueNull",
         resultPropertyId = QueueNullId,
         resultPropertyOutput =
           makeOutput (name ++ ".queueNull") whetherIsEmpty whetherIsEmptySignal },
      ResultProperty {
         resultPropertyLabel = "queueFull",
         resultPropertyId = QueueFullId,
         resultPropertyOutput =
           makeOutput (name ++ ".queueFull") whetherIsFull whetherIsFullSignal },
      ResultProperty {
         resultPropertyLabel = "queueMaxCount",
         resultPropertyId = QueueMaxCountId,
         resultPropertyOutput =
           makeOutput (name ++ ".queueMaxCount") getMaxCount maxCountSignal },
      ResultProperty {
         resultPropertyLabel = "queueCount",
         resultPropertyId = QueueCountId,
         resultPropertyOutput =
           makeOutput (name ++ ".queueCount") getCount countSignal },
      ResultProperty {
         resultPropertyLabel = "enqueueCount",
         resultPropertyId = EnqueueCountId,
         resultPropertyOutput =
           makeOutput (name ++ ".enqueueCount") getEnqueueCount enqueueCountSignal },
      ResultProperty {
         resultPropertyLabel = "enqueueLostCount",
         resultPropertyId = EnqueueLostCountId,
         resultPropertyOutput =
           makeOutput (name ++ ".enqueueLostCount") getEnqueueLostCount enqueueLostCountSignal },
      ResultProperty {
         resultPropertyLabel = "enqueueStoreCount",
         resultPropertyId = EnqueueStoreCountId,
         resultPropertyOutput =
           makeOutput (name ++ ".enqueueStoreCount") getEnqueueStoreCount enqueueStoreCountSignal },
      ResultProperty {
         resultPropertyLabel = "dequeueCount",
         resultPropertyId = DequeueCountId,
         resultPropertyOutput =
           makeOutput (name ++ ".dequeueCount") getDequeueCount dequeueCountSignal },
      ResultProperty {
         resultPropertyLabel = "dequeueExtractCount",
         resultPropertyId = DequeueExtractCountId,
         resultPropertyOutput =
           makeOutput (name ++ ".dequeueExtractCount") getDequeueExtractCount dequeueExtractCountSignal },
      ResultProperty {
         resultPropertyLabel = "queueLoadFactor",
         resultPropertyId = QueueLoadFactorId,
         resultPropertyOutput =
           makeOutput (name ++ ".queueLoadFactor") getLoadFactor loadFactorSignal },
      ResultProperty {
         resultPropertyLabel = "enqueueRate",
         resultPropertyId = EnqueueRateId,
         resultPropertyOutput =
           makeOutput (name ++ ".enqueueRate") getEnqueueRate enqueueRateSignal },
      ResultProperty {
         resultPropertyLabel = "enqueueStoreRate",
         resultPropertyId = EnqueueStoreRateId,
         resultPropertyOutput =
           makeOutput (name ++ ".enqueueStoreRate") getEnqueueStoreRate enqueueStoreRateSignal },
      ResultProperty {
         resultPropertyLabel = "dequeueRate",
         resultPropertyId = DequeueRateId,
         resultPropertyOutput =
           makeOutput (name ++ ".dequeueRate") getDequeueRate dequeueRateSignal },
      ResultProperty {
         resultPropertyLabel = "dequeueExtractRate",
         resultPropertyId = DequeueExtractRateId,
         resultPropertyOutput =
           makeOutput (name ++ ".dequeueExtractRate") getDequeueExtractRate dequeueExtractRateSignal },
      ResultProperty {
         resultPropertyLabel = "queueWaitTime",
         resultPropertyId = QueueWaitTimeId,
         resultPropertyOutput =
           makeOutput (name ++ ".queueWaitTime") getWaitTime waitTimeSignal },
      ResultProperty {
         resultPropertyLabel = "queueTotalWaitTime",
         resultPropertyId = QueueTotalWaitTimeId,
         resultPropertyOutput =
           makeOutput (name ++ ".queueTotalWaitTime") getTotalWaitTime totalWaitTimeSignal },
      ResultProperty {
         resultPropertyLabel = "enqueueWaitTime",
         resultPropertyId = EnqueueWaitTimeId,
         resultPropertyOutput =
           makeOutput (name ++ ".enqueueWaitTime") getEnqueueWaitTime enqueueWaitTimeSignal },
      ResultProperty {
         resultPropertyLabel = "dequeueWaitTime",
         resultPropertyId = DequeueWaitTimeId,
         resultPropertyOutput =
           makeOutput (name ++ ".dequeueWaitTime") getDequeueWaitTime dequeueWaitTimeSignal }
      ]
    }
  where makeOutput name' f g =
          ResultItemOutput $
          ResultItem { resultItemName   = name',
                       resultItemData   = f queue,
                       resultItemSignal = g queue }
        -- properties
        getEnqueueStrategy = StringResultData . return . show . Q.enqueueStrategy
        getEnqueueStoringStrategy = StringResultData . return . show . Q.enqueueStoringStrategy
        getDequeueStrategy = StringResultData . return . show . Q.dequeueStrategy
        whetherIsEmpty = StringResultData . fmap show . Q.queueNull
        whetherIsFull = StringResultData . fmap show . Q.queueFull
        getMaxCount = IntResultData . return . Q.queueMaxCount
        getCount = IntResultData . Q.queueCount
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

-- | Output an arbitrary text as a separator.
makeTextOutput :: String -> ResultOutput
makeTextOutput text =
  ResultSeparatorOutput $
  ResultSeparator { resultSeparatorText = text }

-- | Output the modeling time.
timeOutput :: ResultOutput
timeOutput = resultOutput (resultSource "t" time) StringResultType

-- | Make an integer subscript
makeIntSubscript :: Show a => a -> String
makeIntSubscript i = "[" ++ show i ++ "]"

instance ResultComputation m => ResultProvider (m Double) where

  resultSource = makeResultSource DoubleResultData

instance ResultComputation m => ResultProvider (m [Double]) where

  resultSource = makeResultSource DoubleListResultData

instance ResultComputation m => ResultProvider (m (SamplingStats Double)) where

  resultSource = makeResultSource DoubleStatsResultData

instance ResultComputation m => ResultProvider (m Int) where

  resultSource = makeResultSource IntResultData

instance ResultComputation m => ResultProvider (m [Int]) where

  resultSource = makeResultSource IntListResultData

instance ResultComputation m => ResultProvider (m (SamplingStats Int)) where

  resultSource = makeResultSource IntStatsResultData

instance ResultComputation m => ResultProvider (m String) where

  resultSource = makeResultSource StringResultData

instance ResultProvider p => ResultProvider [p] where

  resultSource name m =
    resultSource name $ ResultListWithSubscript m subscript where
      subscript = map snd $ zip m $ map makeIntSubscript [0..]

instance (Show i, Ix i, ResultProvider p) => ResultProvider (A.Array i p) where

  resultSource name m =
    resultSource name $ ResultListWithSubscript items subscript where
      items = A.elems m
      subscript = map show (A.indices m)

instance ResultProvider p => ResultProvider (V.Vector p) where

  resultSource name m =
    resultSource name $ ResultVectorWithSubscript m subscript where
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

  resultSource name (ResultListWithSubscript xs ys) = ResultSource f where
    f t =
      ResultVectorOutput $
      ResultVector { resultVectorName = name,
                     resultVectorItems = V.fromList (items t),
                     resultVectorSubscript = V.fromList ys }
    items t =
      flip map (zip ys xs) $ \(y, x) ->
      let name' = name ++ y
      in resultOutput (resultSource name' x) t
    
instance (Show i, Ix i, ResultProvider p) => ResultProvider (ResultArrayWithSubscript i p) where

  resultSource name (ResultArrayWithSubscript xs ys) =
    resultSource name $ ResultListWithSubscript items subscript where
      items = A.elems xs
      subscript = A.elems ys

instance ResultProvider p => ResultProvider (ResultVectorWithSubscript p) where

  resultSource name (ResultVectorWithSubscript xs ys) = ResultSource f where
    f t =
      ResultVectorOutput $
      ResultVector { resultVectorName = name,
                     resultVectorItems = items t,
                     resultVectorSubscript = ys }
    items t =
      V.generate (V.length xs) $ \i ->
      let x = xs V.! i
          y = ys V.! i
          name' = name ++ y
      in resultOutput (resultSource name' x) t

instance (Ix i, Show i, ResultComputation m) => ResultProvider (m (A.Array i Double)) where

  resultSource = makeResultSource (DoubleListResultData . fmap A.elems)

instance (Ix i, Show i, ResultComputation m) => ResultProvider (m (A.Array i Int)) where

  resultSource = makeResultSource (IntListResultData . fmap A.elems)

instance ResultComputation m => ResultProvider (m (V.Vector Double)) where

  resultSource = makeResultSource (DoubleListResultData . fmap V.toList)

instance ResultComputation m => ResultProvider (m (V.Vector Int)) where

  resultSource = makeResultSource (IntListResultData . fmap V.toList)

instance (Show si, Show sm, Show so) => ResultProvider (Q.Queue si qi sm qm so qo a) where

  resultSource = generateResultSource makeQueueOutput
