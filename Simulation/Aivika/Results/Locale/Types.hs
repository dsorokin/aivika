
-- |
-- Module     : Simulation.Aivika.Results.Locale.Types
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines basic definitions for localisation.
--
module Simulation.Aivika.Results.Locale.Types
       (-- * Basic Types
        ResultLocale,
        ResultLocalisation(..),
        ResultName,
        ResultDescription,
        UserDefinedResult(..),
        LocalisedResult(..),
        -- * Locale Codes
        russianResultLocale,
        englishResultLocale,
        -- * Localisations
        pathResultLocalisation,
        localisePathResultDescription,
        localisePathResultTitle,
        lookupResultLocalisation,
        -- * Unique Identifiers
        ResultId(..),
        -- * Utilites
        resultNameToTitle) where

import Data.Char
import Data.List
import qualified Data.Map as M

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Statistics
import Simulation.Aivika.Statistics.Accumulator
import qualified Simulation.Aivika.Queue as Q
import qualified Simulation.Aivika.Queue.Infinite as IQ
import Simulation.Aivika.Arrival
import Simulation.Aivika.Server
import Simulation.Aivika.Activity
import Simulation.Aivika.Resource
import Simulation.Aivika.Operation

-- | A locale to output the simulation results.
--
-- Examples are: @\"ru\", @\"en\" etc.
type ResultLocale = String

-- | It localises the description and title of simulation results.
data ResultLocalisation =
  ResultLocalisation { localiseResultDescription :: ResultId -> ResultDescription,
                       -- ^ Localise the description.
                       localiseResultTitle :: ResultId -> ResultDescription
                       -- ^ Localise the title.
                     }

-- | It describes the user-defined simulation result.
data UserDefinedResult =
  UserDefinedResult { userDefinedResultName :: ResultName,
                      -- ^ The user-defined result name.
                      userDefinedResultDescription :: ResultDescription,
                      -- ^ The user-defined result description.
                      userDefinedResultTitle :: ResultDescription
                      -- ^ The user-defined result title.
                    } deriving (Eq, Ord, Show)

-- | This is a localisation of the specified simulation result.
data LocalisedResult =
  LocalisedResult { localisedResultDescriptions :: M.Map ResultLocale ResultDescription,
                    -- ^ The localised descriptions.
                    localisedResultTitles :: M.Map ResultLocale ResultDescription
                    -- ^ The localised titles.
                  } deriving (Eq, Ord, Show)

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
                -- ^ Describes a vector item with the specified subscript.
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
              | SamplingCounterId
                -- ^ A 'SamplingCounter' value.
              | SamplingCounterValueId
                -- ^ Property 'samplingCounterValue'.
              | SamplingCounterStatsId
                -- ^ Property 'samplingCounterStats'.
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
              | TimingCounterId
                -- ^ A 'TimingCounter' value.
              | TimingCounterValueId
                -- ^ Property 'timingCounterValue'.
              | TimingCounterStatsId
                -- ^ Property 'timingCounterStats'.
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
              | QueueRateId
                -- ^ Property 'Q.queueRate'.
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
              | ServerTotalPreemptionTimeId
                -- ^ Property 'serverTotalPreemptionTime'.
              | ServerInputWaitTimeId
                -- ^ Property 'serverInputWaitTime'.
              | ServerProcessingTimeId
                -- ^ Property 'serverProcessingTime'.
              | ServerOutputWaitTimeId
                -- ^ Property 'serverOutputWaitTime'.
              | ServerPreemptionTimeId
                -- ^ Property 'serverPreemptionTime'.
              | ServerInputWaitFactorId
                -- ^ Property 'serverInputWaitFactor'.
              | ServerProcessingFactorId
                -- ^ Property 'serverProcessingFactor'.
              | ServerOutputWaitFactorId
                -- ^ Property 'serverOutputWaitFactor'.
              | ServerPreemptionFactorId
                -- ^ Property 'serverPreemptionFactor'.
              | ActivityId
                -- ^ Represents an 'Activity'.
              | ActivityInitStateId
                -- ^ Property 'activityInitState'.
              | ActivityStateId
                -- ^ Property 'activityState'.
              | ActivityTotalUtilisationTimeId
                -- ^ Property 'activityTotalUtilisationTime'.
              | ActivityTotalIdleTimeId
                -- ^ Property 'activityTotalIdleTime'.
              | ActivityTotalPreemptionTimeId
                -- ^ Property 'activityTotalPreemptionTime'.
              | ActivityUtilisationTimeId
                -- ^ Property 'activityUtilisationTime'.
              | ActivityIdleTimeId
                -- ^ Property 'activityIdleTime'.
              | ActivityPreemptionTimeId
                -- ^ Property 'activityPreemptionTime'.
              | ActivityUtilisationFactorId
                -- ^ Property 'activityUtilisationFactor'.
              | ActivityIdleFactorId
                -- ^ Property 'activityIdleFactor'.
              | ActivityPreemptionFactorId
                -- ^ Property 'activityPreemptionFactor'.
              | ResourceId
                -- ^ Represents a 'Resource'.
              | ResourceCountId
                -- ^ Property 'resourceCount'.
              | ResourceCountStatsId
                -- ^ Property 'resourceCountStats'.
              | ResourceUtilisationCountId
                -- ^ Property 'resourceUtilisationCount'.
              | ResourceUtilisationCountStatsId
                -- ^ Property 'resourceUtilisationCountStats'.
              | ResourceQueueCountId
                -- ^ Property 'resourceQueueCount'.
              | ResourceQueueCountStatsId
                -- ^ Property 'resourceQueueCountStats'.
              | ResourceTotalWaitTimeId
                -- ^ Property 'resourceTotalWaitTime'.
              | ResourceWaitTimeId
                -- ^ Property 'resourceWaitTime'.
              | OperationId
                -- ^ Represents an 'Operation'.
              | OperationTotalUtilisationTimeId
                -- ^ Property 'operationTotalUtilisationTime'.
              | OperationTotalPreemptionTimeId
                -- ^ Property 'operationTotalPreemptionTime'.
              | OperationUtilisationTimeId
                -- ^ Property 'operationUtilisationTime'.
              | OperationPreemptionTimeId
                -- ^ Property 'operationPreemptionTime'.
              | OperationUtilisationFactorId
                -- ^ Property 'operationUtilisationFactor'.
              | OperationPreemptionFactorId
                -- ^ Property 'operationPreemptionFactor'.
              | UserDefinedResultId UserDefinedResult
                -- ^ An user defined description.
              | LocalisedResultId LocalisedResult
                -- ^ A localised property or object name.
              deriving (Eq, Ord, Show)

-- | The Russian locale.
russianResultLocale :: ResultLocale
russianResultLocale = "ru"

-- | The English locale.
englishResultLocale :: ResultLocale
englishResultLocale = "en"

-- | Lookup a localisation by the specified locale.
lookupResultLocalisation :: ResultLocale -> M.Map ResultLocale ResultDescription -> ResultDescription
lookupResultLocalisation loc m =
  case M.lookup loc m of
    Just x  -> x
    Nothing ->
      case M.lookup russianResultLocale m of
        Just x  -> x
        Nothing ->
          case M.lookup englishResultLocale m of
            Just x  -> x
            Nothing -> ""

-- | Return the path result localisation.
pathResultLocalisation :: (ResultId -> ResultDescription) -> [ResultId] -> ResultDescription
pathResultLocalisation loc is = loop is []
  where loop [] acc                      = foldr ($) [] (reverse acc)
        loop (x : xs) []                 = loop xs [(loc x ++)]
        loop ((VectorItemId s) : xs) acc = loop xs $ (s ++) : (" " ++) : acc
        loop (x : xs) acc                = loop xs $ (loc x ++) : (" / " ++) : acc

-- | Localise the path result description.
localisePathResultDescription :: ResultLocalisation -> [ResultId] -> ResultDescription
localisePathResultDescription loc is = pathResultLocalisation (localiseResultDescription loc) is

-- | Localise the path result title.
localisePathResultTitle :: ResultLocalisation -> [ResultId] -> ResultDescription
localisePathResultTitle loc is = pathResultLocalisation (localiseResultTitle loc) is

-- | Convert the result name to title.
resultNameToTitle :: ResultName -> ResultDescription
resultNameToTitle =
  unwords .
  concat .
  map (map $ map toLower) .
  map (groupBy (\x y -> not $ isUpper y)) .
  words
