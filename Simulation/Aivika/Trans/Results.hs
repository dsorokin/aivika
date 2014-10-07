
{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, UndecidableInstances, ExistentialQuantification #-}

-- |
-- Module     : Simulation.Aivika.Trans.Results
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module allows exporting the simulation results from the model.
--
module Simulation.Aivika.Trans.Results
       (-- * Definitions Focused on Modeling
        Results,
        ResultTransform,
        ResultName,
        ResultProvider(..),
        results,
        expandResults,
        resultSummary,
        resultByName,
        resultByProperty,
        resultByIndex,
        resultBySubscript,
        ResultComputing(..),
        ResultComputation(..),
        ResultListWithSubscript(..),
        ResultArrayWithSubscript(..),
#ifndef __HASTE__
        ResultVectorWithSubscript(..),
#endif
        -- * Definitions Focused on Using the Library
        ResultExtract(..),
        extractIntResults,
        extractIntListResults,
        extractIntStatsResults,
        extractIntStatsEitherResults,
        extractIntTimingStatsResults,
        extractDoubleResults,
        extractDoubleListResults,
        extractDoubleStatsResults,
        extractDoubleStatsEitherResults,
        extractDoubleTimingStatsResults,
        extractStringResults,
        ResultPredefinedSignals(..),
        newResultPredefinedSignals,
        resultSignal,
        pureResultSignal,
        -- * Definitions Focused on Extending the Library 
        ResultSourceMap,
        ResultSource(..),
        ResultItem(..),
        ResultItemable(..),
        resultItemToIntStatsEitherValue,
        resultItemToDoubleStatsEitherValue,
        ResultObject(..),
        ResultProperty(..),
        ResultVector(..),
        memoResultVectorSignal,
        memoResultVectorSummary,
        ResultSeparator(..),
        ResultValue(..),
        voidResultValue,
        ResultContainer(..),
        resultContainerPropertySource,
        resultContainerConstProperty,
        resultContainerIntegProperty,
        resultContainerProperty,
        resultContainerMapProperty,
        resultValueToContainer,
        resultContainerToValue,
        ResultData,
        ResultSignal(..),
        maybeResultSignal,
        textResultSource,
        timeResultSource,
        resultSourceToIntValues,
        resultSourceToIntListValues,
        resultSourceToIntStatsValues,
        resultSourceToIntStatsEitherValues,
        resultSourceToIntTimingStatsValues,
        resultSourceToDoubleValues,
        resultSourceToDoubleListValues,
        resultSourceToDoubleStatsValues,
        resultSourceToDoubleStatsEitherValues,
        resultSourceToDoubleTimingStatsValues,
        resultSourceToStringValues,
        resultSourceMap,
        resultSourceList,
        resultsToIntValues,
        resultsToIntListValues,
        resultsToIntStatsValues,
        resultsToIntStatsEitherValues,
        resultsToIntTimingStatsValues,
        resultsToDoubleValues,
        resultsToDoubleListValues,
        resultsToDoubleStatsValues,
        resultsToDoubleStatsEitherValues,
        resultsToDoubleTimingStatsValues,
        resultsToStringValues,
        composeResults,
        computeResultValue) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import qualified Data.Array as A

#ifndef __HASTE__
import qualified Data.Vector as V
#endif

import Data.Ix
import Data.Maybe
import Data.Monoid

import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Statistics
import Simulation.Aivika.Trans.Statistics.Accumulator
import Simulation.Aivika.Trans.Ref
import qualified Simulation.Aivika.Trans.Ref.Light as LR
import Simulation.Aivika.Trans.Var
import Simulation.Aivika.Trans.QueueStrategy
import qualified Simulation.Aivika.Trans.Queue as Q
import qualified Simulation.Aivika.Trans.Queue.Infinite as IQ
import Simulation.Aivika.Trans.Arrival
import Simulation.Aivika.Trans.Server
import Simulation.Aivika.Trans.Results.Locale

-- | A name used for indentifying the results when generating output.
type ResultName = String

-- | Represents a provider of the simulation results. It is usually something, or
-- an array of something, or a list of such values which can be simulated to get data.
class ResultProvider p where
  
  -- | Return the source of simulation results by the specified name, description and provider. 
  resultSource :: ResultName -> ResultDescription -> p -> ResultSource
  resultSource name descr = resultSource' name (UserDefinedResultId descr)

  -- | Return the source of simulation results by the specified name, identifier and provider. 
  resultSource' :: ResultName -> ResultId -> p -> ResultSource

-- | It associates the result sources with their names.
type ResultSourceMap = M.Map ResultName ResultSource

-- | Encapsulates the result source.
data ResultSource = ResultItemSource ResultItem
                    -- ^ The source consisting of a single item.
                  | ResultObjectSource ResultObject
                    -- ^ An object-like source.
                  | ResultVectorSource ResultVector
                    -- ^ A vector-like structure.
                  | ResultSeparatorSource ResultSeparator
                    -- ^ This is a separator text.

-- | The simulation results represented by a single item.
data ResultItem = forall a. ResultItemable a => ResultItem a

-- | Represents a type class for actual representing the items.
class ResultItemable a where

  -- | The item name.
  resultItemName :: a -> ResultName
  
  -- | The item identifier.
  resultItemId :: a -> ResultId

  -- | Whether the item emits a signal.
  resultItemSignal :: a -> ResultSignal

  -- | Return an expanded version of the item, for example,
  -- when the statistics item is exanded to an object
  -- having the corresponded properties for count, average,
  -- deviation, minimum, maximum and so on.
  resultItemExpansion :: a -> ResultSource
  
  -- | Return usually a short version of the item, i.e. its summary,
  -- but values of some data types such as statistics can be
  -- implicitly expanded to an object with the corresponded
  -- properties.
  resultItemSummary :: a -> ResultSource
  
  -- | Return integer numbers in time points.
  resultItemToIntValue :: a -> ResultValue Int

  -- | Return lists of integer numbers in time points. 
  resultItemToIntListValue :: a -> ResultValue [Int]

  -- | Return statistics based on integer numbers.
  resultItemToIntStatsValue :: a -> ResultValue (SamplingStats Int)

  -- | Return timing statistics based on integer numbers.
  resultItemToIntTimingStatsValue :: a -> ResultValue (TimingStats Int)

  -- | Return double numbers in time points.
  resultItemToDoubleValue :: a -> ResultValue Double
  
  -- | Return lists of double numbers in time points. 
  resultItemToDoubleListValue :: a -> ResultValue [Double]

  -- | Return statistics based on double numbers.
  resultItemToDoubleStatsValue :: a -> ResultValue (SamplingStats Double)

  -- | Return timing statistics based on integer numbers.
  resultItemToDoubleTimingStatsValue :: a -> ResultValue (TimingStats Double)

  -- | Return string representations in time points.
  resultItemToStringValue :: a -> ResultValue String

-- | Return a version optimised for fast aggregation of the statistics based on integer numbers.
resultItemToIntStatsEitherValue :: ResultItemable a => a -> ResultValue (Either Int (SamplingStats Int))
resultItemToIntStatsEitherValue x =
  case resultValueData x1 of
    Just a1 -> fmap Left x1
    Nothing ->
      case resultValueData x2 of
        Just a2 -> fmap Right x2
        Nothing -> voidResultValue x2
  where
    x1 = resultItemToIntValue x
    x2 = resultItemToIntStatsValue x

-- | Return a version optimised for fast aggregation of the statistics based on double floating point numbers.
resultItemToDoubleStatsEitherValue :: ResultItemable a => a -> ResultValue (Either Double (SamplingStats Double))
resultItemToDoubleStatsEitherValue x =
  case resultValueData x1 of
    Just a1 -> fmap Left x1
    Nothing ->
      case resultValueData x2 of
        Just a2 -> fmap Right x2
        Nothing -> voidResultValue x2
  where
    x1 = resultItemToDoubleValue x
    x2 = resultItemToDoubleStatsValue x

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
                 resultObjectSignal :: ResultSignal,
                 -- ^ A combined signal if present.
                 resultObjectSummary :: ResultSource
                 -- ^ A short version of the object, i.e. its summary.
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
                 resultVectorItems :: A.Array Int ResultSource,
                 -- ^ The results supplied by the vector items.
                 resultVectorSubscript :: A.Array Int ResultName,
                 -- ^ The subscript used as a suffix to create item names.
                 resultVectorSignal :: ResultSignal,
                 -- ^ A combined signal if present.
                 resultVectorSummary :: ResultSource
                 -- ^ A short version of the vector, i.e. summary.
               }

-- | Calculate the result vector signal and memoize it in a new vector.
memoResultVectorSignal :: ResultVector -> ResultVector
memoResultVectorSignal x =
  x { resultVectorSignal =
         foldr (<>) mempty $ map resultSourceSignal $ A.elems $ resultVectorItems x }

-- | Calculate the result vector summary and memoize it in a new vector.
memoResultVectorSummary :: ResultVector -> ResultVector
memoResultVectorSummary x =
  x { resultVectorSummary =
         ResultVectorSource $
         x { resultVectorItems =
                A.array bnds [(i, resultSourceSummary e) | (i, e) <- ies] } }
  where
    arr  = resultVectorItems x
    bnds = A.bounds arr
    ies  = A.assocs arr

-- | It separates the simulation results when printing.
data ResultSeparator =
  ResultSeparator { resultSeparatorText :: String
                    -- ^ The separator text.
                  }

-- | A parameterised value that actually represents a generalised result item that have no parametric type.
data ResultValue e =
  ResultValue { resultValueName :: ResultName,
                -- ^ The value name.
                resultValueId :: ResultId,
                -- ^ The value identifier.
                resultValueData :: ResultData e,
                -- ^ Simulation data supplied by the value.
                resultValueSignal :: ResultSignal
                -- ^ Whether the value emits a signal when changing simulation data.
              }

instance Functor ResultValue where
  fmap f x = x { resultValueData = fmap (fmap f) (resultValueData x) }

-- | Return a new value with the discarded simulation results.
voidResultValue :: ResultValue a -> ResultValue b
voidResultValue x = x { resultValueData = Nothing }

-- | A container of the simulation results such as queue, server or array.
data ResultContainer e =
  ResultContainer { resultContainerName :: ResultName,
                    -- ^ The container name.
                    resultContainerId :: ResultId,
                    -- ^ The container identifier.
                    resultContainerData :: e,
                    -- ^ The container data.
                    resultContainerSignal :: ResultSignal
                    -- ^ Whether the container emits a signal when changing simulation data.
                  }

instance Functor ResultContainer where
  fmap f x = x { resultContainerData = f (resultContainerData x) }

-- | Create a new property source by the specified container.
resultContainerPropertySource :: ResultItemable (ResultValue b)
                                 => ResultContainer a
                                 -- ^ the container
                                 -> ResultName
                                 -- ^ the property label
                                 -> ResultId
                                 -- ^ the property identifier
                                 -> (a -> ResultData b)
                                 -- ^ get the specified data from the container
                                 -> (a -> ResultSignal)
                                 -- ^ get the data signal from the container
                                 -> ResultSource
resultContainerPropertySource cont name i f g =
  ResultItemSource $
  ResultItem $
  ResultValue {
    resultValueName   = (resultContainerName cont) ++ "." ++ name,
    resultValueId     = i,
    resultValueData   = f (resultContainerData cont),
    resultValueSignal = g (resultContainerData cont) }

-- | Create a constant property by the specified container.
resultContainerConstProperty :: ResultItemable (ResultValue b)
                                => ResultContainer a
                                -- ^ the container
                                -> ResultName
                                -- ^ the property label
                                -> ResultId
                                -- ^ the property identifier
                                -> (a -> b)
                                -- ^ get the specified data from the container
                                -> ResultProperty
resultContainerConstProperty cont name i f =
  ResultProperty {
    resultPropertyLabel = name,
    resultPropertyId = i,
    resultPropertySource =
      resultContainerPropertySource cont name i (Just . return . f) (const EmptyResultSignal) }
  
-- | Create by the specified container a property that changes in the integration time points, or it is supposed to be such one.
resultContainerIntegProperty :: ResultItemable (ResultValue b)
                                => ResultContainer a
                                -- ^ the container
                                -> ResultName
                                -- ^ the property label
                                -> ResultId
                                -- ^ the property identifier
                                -> (a -> Event b)
                                -- ^ get the specified data from the container
                             -> ResultProperty
resultContainerIntegProperty cont name i f =
  ResultProperty {
    resultPropertyLabel = name,
    resultPropertyId = i,
    resultPropertySource =
      resultContainerPropertySource cont name i (Just . f) (const UnknownResultSignal) }
  
-- | Create a property by the specified container.
resultContainerProperty :: ResultItemable (ResultValue b)
                           => ResultContainer a
                           -- ^ the container
                           -> ResultName
                           -- ^ the property label
                           -> ResultId
                           -- ^ the property identifier
                           -> (a -> Event b)
                           -- ^ get the specified data from the container
                           -> (a -> Signal ())
                           -- ^ get a signal triggered when changing data.
                           -> ResultProperty
resultContainerProperty cont name i f g =                     
  ResultProperty {
    resultPropertyLabel = name,
    resultPropertyId = i,
    resultPropertySource =
      resultContainerPropertySource cont name i (Just . f) (ResultSignal . g) }

-- | Create by the specified container a mapped property which is recomputed each time again and again.
resultContainerMapProperty :: ResultItemable (ResultValue b)
                              => ResultContainer (ResultData a)
                              -- ^ the container
                              -> ResultName
                              -- ^ the property label
                              -> ResultId
                              -- ^ the property identifier
                              -> (a -> b)
                              -- ^ recompute the specified data
                              -> ResultProperty
resultContainerMapProperty cont name i f =                     
  ResultProperty {
    resultPropertyLabel = name,
    resultPropertyId = i,
    resultPropertySource =
      resultContainerPropertySource cont name i (fmap $ fmap f) (const $ resultContainerSignal cont) }

-- | Convert the result value to a container with the specified object identifier. 
resultValueToContainer :: ResultValue a -> ResultContainer (ResultData a)
resultValueToContainer x =
  ResultContainer {
    resultContainerName   = resultValueName x,
    resultContainerId     = resultValueId x,
    resultContainerData   = resultValueData x,
    resultContainerSignal = resultValueSignal x }

-- | Convert the result container to a value.
resultContainerToValue :: ResultContainer (ResultData a) -> ResultValue a
resultContainerToValue x =
  ResultValue {
    resultValueName   = resultContainerName x,
    resultValueId     = resultContainerId x,
    resultValueData   = resultContainerData x,
    resultValueSignal = resultContainerSignal x }

-- | Represents the very simulation results.
type ResultData e = Maybe (Event e)

-- | Whether an object containing the results emits a signal notifying about change of data.
data ResultSignal = EmptyResultSignal
                    -- ^ There is no signal at all.
                  | UnknownResultSignal
                    -- ^ The signal is unknown, but the entity probably changes.
                  | ResultSignal (Signal ())
                    -- ^ When the signal is precisely specified.
                  | ResultSignalMix (Signal ())
                    -- ^ When the specified signal was combined with unknown signal.

instance Monoid ResultSignal where

  mempty = EmptyResultSignal

  mappend EmptyResultSignal z = z

  mappend UnknownResultSignal EmptyResultSignal = UnknownResultSignal
  mappend UnknownResultSignal UnknownResultSignal = UnknownResultSignal
  mappend UnknownResultSignal (ResultSignal x) = ResultSignalMix x
  mappend UnknownResultSignal z@(ResultSignalMix x) = z
  
  mappend z@(ResultSignal x) EmptyResultSignal = z
  mappend (ResultSignal x) UnknownResultSignal = ResultSignalMix x
  mappend (ResultSignal x) (ResultSignal y) = ResultSignal (x <> y)
  mappend (ResultSignal x) (ResultSignalMix y) = ResultSignalMix (x <> y)
  
  mappend z@(ResultSignalMix x) EmptyResultSignal = z
  mappend z@(ResultSignalMix x) UnknownResultSignal = z
  mappend (ResultSignalMix x) (ResultSignal y) = ResultSignalMix (x <> y)
  mappend (ResultSignalMix x) (ResultSignalMix y) = ResultSignalMix (x <> y)

-- | Construct a new result signal by the specified optional pure signal.
maybeResultSignal :: Maybe (Signal ()) -> ResultSignal
maybeResultSignal (Just x) = ResultSignal x
maybeResultSignal Nothing  = EmptyResultSignal

instance ResultItemable (ResultValue Int) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = id
  resultItemToIntListValue = fmap return
  resultItemToIntStatsValue = fmap returnSamplingStats
  resultItemToIntTimingStatsValue = voidResultValue

  resultItemToDoubleValue = fmap fromIntegral
  resultItemToDoubleListValue = fmap (return . fromIntegral)
  resultItemToDoubleStatsValue = fmap (returnSamplingStats . fromIntegral)
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show

  resultItemExpansion = ResultItemSource . ResultItem
  resultItemSummary = ResultItemSource . ResultItem

instance ResultItemable (ResultValue Double) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = voidResultValue
  
  resultItemToDoubleValue = id
  resultItemToDoubleListValue = fmap return
  resultItemToDoubleStatsValue = fmap returnSamplingStats
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show
  
  resultItemExpansion = ResultItemSource . ResultItem
  resultItemSummary = ResultItemSource . ResultItem

instance ResultItemable (ResultValue [Int]) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = id
  resultItemToIntStatsValue = fmap listSamplingStats
  resultItemToIntTimingStatsValue = voidResultValue

  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = fmap (map fromIntegral)
  resultItemToDoubleStatsValue = fmap (fromIntSamplingStats . listSamplingStats)
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show
  
  resultItemExpansion = ResultItemSource . ResultItem
  resultItemSummary = ResultItemSource . ResultItem

instance ResultItemable (ResultValue [Double]) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = voidResultValue
  
  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = id
  resultItemToDoubleStatsValue = fmap listSamplingStats
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show
  
  resultItemExpansion = ResultItemSource . ResultItem
  resultItemSummary = ResultItemSource . ResultItem

instance ResultItemable (ResultValue (SamplingStats Int)) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = id
  resultItemToIntTimingStatsValue = voidResultValue

  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = voidResultValue
  resultItemToDoubleStatsValue = fmap fromIntSamplingStats
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show
  
  resultItemExpansion = samplingStatsResultSource
  resultItemSummary = samplingStatsResultSummary

instance ResultItemable (ResultValue (SamplingStats Double)) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = voidResultValue
  
  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = voidResultValue
  resultItemToDoubleStatsValue = id
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show
  
  resultItemExpansion = samplingStatsResultSource
  resultItemSummary = samplingStatsResultSummary

instance ResultItemable (ResultValue (TimingStats Int)) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = id

  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = voidResultValue
  resultItemToDoubleStatsValue = voidResultValue
  resultItemToDoubleTimingStatsValue = fmap fromIntTimingStats

  resultItemToStringValue = fmap show
  
  resultItemExpansion = timingStatsResultSource
  resultItemSummary = timingStatsResultSummary

instance ResultItemable (ResultValue (TimingStats Double)) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = voidResultValue

  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = voidResultValue
  resultItemToDoubleStatsValue = voidResultValue
  resultItemToDoubleTimingStatsValue = id

  resultItemToStringValue = fmap show
  
  resultItemExpansion = timingStatsResultSource
  resultItemSummary = timingStatsResultSummary

instance ResultItemable (ResultValue Bool) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = voidResultValue

  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = voidResultValue
  resultItemToDoubleStatsValue = voidResultValue
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show

  resultItemExpansion = ResultItemSource . ResultItem
  resultItemSummary = ResultItemSource . ResultItem

instance ResultItemable (ResultValue String) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = voidResultValue

  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = voidResultValue
  resultItemToDoubleStatsValue = voidResultValue
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show

  resultItemExpansion = ResultItemSource . ResultItem
  resultItemSummary = ResultItemSource . ResultItem

instance ResultItemable (ResultValue ()) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = voidResultValue

  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = voidResultValue
  resultItemToDoubleStatsValue = voidResultValue
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show

  resultItemExpansion = ResultItemSource . ResultItem
  resultItemSummary = ResultItemSource . ResultItem

instance ResultItemable (ResultValue FCFS) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = voidResultValue

  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = voidResultValue
  resultItemToDoubleStatsValue = voidResultValue
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show

  resultItemExpansion = ResultItemSource . ResultItem
  resultItemSummary = ResultItemSource . ResultItem

instance ResultItemable (ResultValue LCFS) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = voidResultValue

  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = voidResultValue
  resultItemToDoubleStatsValue = voidResultValue
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show

  resultItemExpansion = ResultItemSource . ResultItem
  resultItemSummary = ResultItemSource . ResultItem

instance ResultItemable (ResultValue SIRO) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = voidResultValue

  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = voidResultValue
  resultItemToDoubleStatsValue = voidResultValue
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show

  resultItemExpansion = ResultItemSource . ResultItem
  resultItemSummary = ResultItemSource . ResultItem

instance ResultItemable (ResultValue StaticPriorities) where

  resultItemName = resultValueName
  resultItemId = resultValueId
  resultItemSignal = resultValueSignal
  
  resultItemToIntValue = voidResultValue
  resultItemToIntListValue = voidResultValue
  resultItemToIntStatsValue = voidResultValue
  resultItemToIntTimingStatsValue = voidResultValue

  resultItemToDoubleValue = voidResultValue
  resultItemToDoubleListValue = voidResultValue
  resultItemToDoubleStatsValue = voidResultValue
  resultItemToDoubleTimingStatsValue = voidResultValue

  resultItemToStringValue = fmap show

  resultItemExpansion = ResultItemSource . ResultItem
  resultItemSummary = ResultItemSource . ResultItem

-- | Flatten the result source.
flattenResultSource :: ResultSource -> [ResultItem]
flattenResultSource (ResultItemSource x) = [x]
flattenResultSource (ResultObjectSource x) =
  concat $ map (flattenResultSource . resultPropertySource) $ resultObjectProperties x
flattenResultSource (ResultVectorSource x) =
  concat $ map flattenResultSource $ A.elems $ resultVectorItems x
flattenResultSource (ResultSeparatorSource x) = []

-- | Return the result source name.
resultSourceName :: ResultSource -> ResultName
resultSourceName (ResultItemSource (ResultItem x)) = resultItemName x
resultSourceName (ResultObjectSource x) = resultObjectName x
resultSourceName (ResultVectorSource x) = resultVectorName x
resultSourceName (ResultSeparatorSource x) = []

-- | Expand the result source returning a more detailed version expanding the properties as possible.
expandResultSource :: ResultSource -> ResultSource
expandResultSource (ResultItemSource (ResultItem x)) = resultItemExpansion x
expandResultSource (ResultObjectSource x) =
  ResultObjectSource $
  x { resultObjectProperties =
         flip fmap (resultObjectProperties x) $ \p ->
         p { resultPropertySource = expandResultSource (resultPropertySource p) } }
expandResultSource (ResultVectorSource x) =
  ResultVectorSource $
  x { resultVectorItems =
         A.array bnds [(i, expandResultSource e) | (i, e) <- ies] }
    where arr  = resultVectorItems x
          bnds = A.bounds arr
          ies  = A.assocs arr
expandResultSource z@(ResultSeparatorSource x) = z

-- | Return a summarised and usually more short version of the result source expanding the main properties or excluding auxiliary properties if required.
resultSourceSummary :: ResultSource -> ResultSource
resultSourceSummary (ResultItemSource (ResultItem x)) = resultItemSummary x
resultSourceSummary (ResultObjectSource x) = resultObjectSummary x
resultSourceSummary (ResultVectorSource x) = resultVectorSummary x
resultSourceSummary z@(ResultSeparatorSource x) = z

-- | Return a signal emitted by the source.
resultSourceSignal :: ResultSource -> ResultSignal
resultSourceSignal (ResultItemSource (ResultItem x)) = resultItemSignal x
resultSourceSignal (ResultObjectSource x) = resultObjectSignal x
resultSourceSignal (ResultVectorSource x) = resultVectorSignal x
resultSourceSignal (ResultSeparatorSource x) = EmptyResultSignal

-- | Represent the result source as integer numbers.
resultSourceToIntValues :: ResultSource -> [ResultValue Int]
resultSourceToIntValues = map (\(ResultItem x) -> resultItemToIntValue x) . flattenResultSource

-- | Represent the result source as lists of integer numbers.
resultSourceToIntListValues :: ResultSource -> [ResultValue [Int]]
resultSourceToIntListValues = map (\(ResultItem x) -> resultItemToIntListValue x) . flattenResultSource

-- | Represent the result source as statistics based on integer numbers.
resultSourceToIntStatsValues :: ResultSource -> [ResultValue (SamplingStats Int)]
resultSourceToIntStatsValues = map (\(ResultItem x) -> resultItemToIntStatsValue x) . flattenResultSource

-- | Represent the result source as statistics based on integer numbers and optimised for fast aggregation.
resultSourceToIntStatsEitherValues :: ResultSource -> [ResultValue (Either Int (SamplingStats Int))]
resultSourceToIntStatsEitherValues = map (\(ResultItem x) -> resultItemToIntStatsEitherValue x) . flattenResultSource

-- | Represent the result source as timing statistics based on integer numbers.
resultSourceToIntTimingStatsValues :: ResultSource -> [ResultValue (TimingStats Int)]
resultSourceToIntTimingStatsValues = map (\(ResultItem x) -> resultItemToIntTimingStatsValue x) . flattenResultSource

-- | Represent the result source as double floating point numbers.
resultSourceToDoubleValues :: ResultSource -> [ResultValue Double]
resultSourceToDoubleValues = map (\(ResultItem x) -> resultItemToDoubleValue x) . flattenResultSource

-- | Represent the result source as lists of double floating point numbers.
resultSourceToDoubleListValues :: ResultSource -> [ResultValue [Double]]
resultSourceToDoubleListValues = map (\(ResultItem x) -> resultItemToDoubleListValue x) . flattenResultSource

-- | Represent the result source as statistics based on double floating point numbers.
resultSourceToDoubleStatsValues :: ResultSource -> [ResultValue (SamplingStats Double)]
resultSourceToDoubleStatsValues = map (\(ResultItem x) -> resultItemToDoubleStatsValue x) . flattenResultSource

-- | Represent the result source as statistics based on double floating point numbers and optimised for fast aggregation.
resultSourceToDoubleStatsEitherValues :: ResultSource -> [ResultValue (Either Double (SamplingStats Double))]
resultSourceToDoubleStatsEitherValues = map (\(ResultItem x) -> resultItemToDoubleStatsEitherValue x) . flattenResultSource

-- | Represent the result source as timing statistics based on double floating point numbers.
resultSourceToDoubleTimingStatsValues :: ResultSource -> [ResultValue (TimingStats Double)]
resultSourceToDoubleTimingStatsValues = map (\(ResultItem x) -> resultItemToDoubleTimingStatsValue x) . flattenResultSource

-- | Represent the result source as string values.
resultSourceToStringValues :: ResultSource -> [ResultValue String]
resultSourceToStringValues = map (\(ResultItem x) -> resultItemToStringValue x) . flattenResultSource

-- | It contains the results of simulation.
data Results =
  Results { resultSourceMap :: ResultSourceMap,
            -- ^ The sources of simulation results as a map of associated names.
            resultSourceList :: [ResultSource]
            -- ^ The sources of simulation results as an ordered list.
          }

-- | It transforms the results of simulation.
type ResultTransform = Results -> Results

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

instance Monoid Results where

  mempty      = results mempty
  mappend x y = results $ resultSourceList x <> resultSourceList y

-- | Prepare the simulation results.
results :: [ResultSource] -> Results
results ms =
  Results { resultSourceMap  = M.fromList $ map (\x -> (resultSourceName x, x)) ms,
            resultSourceList = ms }

-- | Represent the results as integer numbers.
resultsToIntValues :: Results -> [ResultValue Int]
resultsToIntValues = concat . map resultSourceToIntValues . resultSourceList

-- | Represent the results as lists of integer numbers.
resultsToIntListValues :: Results -> [ResultValue [Int]]
resultsToIntListValues = concat . map resultSourceToIntListValues . resultSourceList

-- | Represent the results as statistics based on integer numbers.
resultsToIntStatsValues :: Results -> [ResultValue (SamplingStats Int)]
resultsToIntStatsValues = concat . map resultSourceToIntStatsValues . resultSourceList

-- | Represent the results as statistics based on integer numbers and optimised for fast aggregation.
resultsToIntStatsEitherValues :: Results -> [ResultValue (Either Int (SamplingStats Int))]
resultsToIntStatsEitherValues = concat . map resultSourceToIntStatsEitherValues . resultSourceList

-- | Represent the results as timing statistics based on integer numbers.
resultsToIntTimingStatsValues :: Results -> [ResultValue (TimingStats Int)]
resultsToIntTimingStatsValues = concat . map resultSourceToIntTimingStatsValues . resultSourceList

-- | Represent the results as double floating point numbers.
resultsToDoubleValues :: Results -> [ResultValue Double]
resultsToDoubleValues = concat . map resultSourceToDoubleValues . resultSourceList

-- | Represent the results as lists of double floating point numbers.
resultsToDoubleListValues :: Results -> [ResultValue [Double]]
resultsToDoubleListValues = concat . map resultSourceToDoubleListValues . resultSourceList

-- | Represent the results as statistics based on double floating point numbers.
resultsToDoubleStatsValues :: Results -> [ResultValue (SamplingStats Double)]
resultsToDoubleStatsValues = concat . map resultSourceToDoubleStatsValues . resultSourceList

-- | Represent the results as statistics based on double floating point numbers and optimised for fast aggregation.
resultsToDoubleStatsEitherValues :: Results -> [ResultValue (Either Double (SamplingStats Double))]
resultsToDoubleStatsEitherValues = concat . map resultSourceToDoubleStatsEitherValues . resultSourceList

-- | Represent the results as timing statistics based on double floating point numbers.
resultsToDoubleTimingStatsValues :: Results -> [ResultValue (TimingStats Double)]
resultsToDoubleTimingStatsValues = concat . map resultSourceToDoubleTimingStatsValues . resultSourceList

-- | Represent the results as string values.
resultsToStringValues :: Results -> [ResultValue String]
resultsToStringValues = concat . map resultSourceToStringValues . resultSourceList

-- | Return a signal emitted by the specified results.
resultSignal :: Results -> ResultSignal
resultSignal = mconcat . map resultSourceSignal . resultSourceList

-- | Return an expanded version of the simulation results expanding the properties as possible, which
-- takes place for expanding statistics to show the count, average, deviation, minimum, maximum etc.
-- as separate values.
expandResults :: ResultTransform
expandResults = results . map expandResultSource . resultSourceList

-- | Return a short version of the simulation results, i.e. their summary, expanding the main properties
-- or excluding auxiliary properties if required.
resultSummary :: ResultTransform
resultSummary = results . map resultSourceSummary . resultSourceList

-- | Take a result by its name.
resultByName :: ResultName -> ResultTransform
resultByName name rs =
  case M.lookup name (resultSourceMap rs) of
    Just x -> results [x]
    Nothing ->
      error $
      "Not found result source with name " ++ name ++
      ": resultByName"

-- | Take a result from the object with the specified property label.
resultByProperty :: ResultName -> ResultTransform
resultByProperty label rs = flip composeResults rs loop
  where
    loop x =
      case x of
        ResultObjectSource s ->
          let ps =
                flip filter (resultObjectProperties s) $ \p ->
                resultPropertyLabel p == label
          in case ps of
            [] ->
              error $
              "Not found property " ++ label ++
              " for object " ++ resultObjectName s ++
              ": resultByProperty"
            ps ->
              map resultPropertySource ps
        ResultVectorSource s ->
          concat $ map loop $ A.elems $ resultVectorItems s
        x ->
          error $
          "Result source " ++ resultSourceName x ++
          " is neither object, nor vector " ++
          ": resultByProperty"

-- | Take a result from the vector by the specified integer index.
resultByIndex :: Int -> ResultTransform
resultByIndex index rs = flip composeResults rs loop
  where
    loop x =
      case x of
        ResultVectorSource s ->
          [resultVectorItems s A.! index] 
        x ->
          error $
          "Result source " ++ resultSourceName x ++
          " is not vector " ++
          ": resultByIndex"

-- | Take a result from the vector by the specified string subscript.
resultBySubscript :: ResultName -> ResultTransform
resultBySubscript subscript rs = flip composeResults rs loop
  where
    loop x =
      case x of
        ResultVectorSource s ->
          let ys = A.elems $ resultVectorItems s
              zs = A.elems $ resultVectorSubscript s
              ps =
                flip filter (zip ys zs) $ \(y, z) ->
                z == subscript
          in case ps of
            [] ->
              error $
              "Not found subscript " ++ subscript ++
              " for vector " ++ resultVectorName s ++
              ": resultBySubscript"
            ps ->
              map fst ps
        x ->
          error $
          "Result source " ++ resultSourceName x ++
          " is not vector " ++
          ": resultBySubscript"

-- | Compose the results using the specified transformation function.
composeResults :: (ResultSource -> [ResultSource]) -> ResultTransform
composeResults f =
  results . concat . map f . resultSourceList

-- | Concatenate the results using the specified list of transformation functions.
concatResults :: [ResultTransform] -> ResultTransform
concatResults trs rs =
  results $ concat $ map (\tr -> resultSourceList $ tr rs) trs

-- | Append the results using the specified transformation functions.
appendResults :: ResultTransform -> ResultTransform -> ResultTransform
appendResults x y =
  concatResults [x, y]

-- | Return a pure signal as a result of combination of the predefined signals
-- with the specified result signal usually provided by the sources.
--
-- The signal returned is triggered when the source signal is triggered.
-- The pure signal is also triggered in the integration time points
-- if the source signal is unknown or it was combined with any unknown signal.
pureResultSignal :: ResultPredefinedSignals -> ResultSignal -> Signal ()
pureResultSignal rs EmptyResultSignal =
  void (resultSignalInStartTime rs)
pureResultSignal rs UnknownResultSignal =
  void (resultSignalInIntegTimes rs)
pureResultSignal rs (ResultSignal s) =
  void (resultSignalInStartTime rs) <> void (resultSignalInStopTime rs) <> s
pureResultSignal rs (ResultSignalMix s) =
  void (resultSignalInIntegTimes rs) <> s

-- | Defines a final result extract: its name, values and other data.
data ResultExtract e =
  ResultExtract { resultExtractName   :: ResultName,
                  -- ^ The result name.
                  resultExtractId     :: ResultId,
                  -- ^ The result identifier.
                  resultExtractData   :: Event e,
                  -- ^ The result values.
                  resultExtractSignal :: ResultSignal
                  -- ^ Whether the result emits a signal.
                }

-- | Extract the results as integer values, or raise a conversion error.
extractIntResults :: Results -> [ResultExtract Int]
extractIntResults rs = flip map (resultsToIntValues rs) $ \x ->
  let n = resultValueName x
      i = resultValueId x
      a = resultValueData x
      s = resultValueSignal x
  in case a of
    Nothing ->
      error $
      "Cannot represent variable " ++ n ++
      " as a source of integer values: extractIntResults"
    Just a ->
      ResultExtract n i a s

-- | Extract the results as lists of integer values, or raise a conversion error.
extractIntListResults :: Results -> [ResultExtract [Int]]
extractIntListResults rs = flip map (resultsToIntListValues rs) $ \x ->
  let n = resultValueName x
      i = resultValueId x
      a = resultValueData x
      s = resultValueSignal x
  in case a of
    Nothing ->
      error $
      "Cannot represent variable " ++ n ++
      " as a source of lists of integer values: extractIntListResults"
    Just a ->
      ResultExtract n i a s

-- | Extract the results as statistics based on integer values,
-- or raise a conversion error.
extractIntStatsResults :: Results -> [ResultExtract (SamplingStats Int)]
extractIntStatsResults rs = flip map (resultsToIntStatsValues rs) $ \x ->
  let n = resultValueName x
      i = resultValueId x
      a = resultValueData x
      s = resultValueSignal x
  in case a of
    Nothing ->
      error $
      "Cannot represent variable " ++ n ++
      " as a source of statistics based on integer values: extractIntStatsResults"
    Just a ->
      ResultExtract n i a s

-- | Extract the results as statistics based on integer values and optimised
-- for fast aggregation, or raise a conversion error.
extractIntStatsEitherResults :: Results -> [ResultExtract (Either Int (SamplingStats Int))]
extractIntStatsEitherResults rs = flip map (resultsToIntStatsEitherValues rs) $ \x ->
  let n = resultValueName x
      i = resultValueId x
      a = resultValueData x
      s = resultValueSignal x
  in case a of
    Nothing ->
      error $
      "Cannot represent variable " ++ n ++
      " as a source of statistics based on integer values: extractIntStatsEitherResults"
    Just a ->
      ResultExtract n i a s

-- | Extract the results as timing statistics based on integer values,
-- or raise a conversion error.
extractIntTimingStatsResults :: Results -> [ResultExtract (TimingStats Int)]
extractIntTimingStatsResults rs = flip map (resultsToIntTimingStatsValues rs) $ \x ->
  let n = resultValueName x
      i = resultValueId x
      a = resultValueData x
      s = resultValueSignal x
  in case a of
    Nothing ->
      error $
      "Cannot represent variable " ++ n ++
      " as a source of timing statistics based on integer values: extractIntTimingStatsResults"
    Just a ->
      ResultExtract n i a s

-- | Extract the results as double floating point values, or raise a conversion error.
extractDoubleResults :: Results -> [ResultExtract Double]
extractDoubleResults rs = flip map (resultsToDoubleValues rs) $ \x ->
  let n = resultValueName x
      i = resultValueId x
      a = resultValueData x
      s = resultValueSignal x
  in case a of
    Nothing ->
      error $
      "Cannot represent variable " ++ n ++
      " as a source of double floating point values: extractDoubleResults"
    Just a ->
      ResultExtract n i a s

-- | Extract the results as lists of double floating point values,
-- or raise a conversion error.
extractDoubleListResults :: Results -> [ResultExtract [Double]]
extractDoubleListResults rs = flip map (resultsToDoubleListValues rs) $ \x ->
  let n = resultValueName x
      i = resultValueId x
      a = resultValueData x
      s = resultValueSignal x
  in case a of
    Nothing ->
      error $
      "Cannot represent variable " ++ n ++
      " as a source of lists of double floating point values: extractDoubleListResults"
    Just a ->
      ResultExtract n i a s

-- | Extract the results as statistics based on double floating point values,
-- or raise a conversion error.
extractDoubleStatsResults :: Results -> [ResultExtract (SamplingStats Double)]
extractDoubleStatsResults rs = flip map (resultsToDoubleStatsValues rs) $ \x ->
  let n = resultValueName x
      i = resultValueId x
      a = resultValueData x
      s = resultValueSignal x
  in case a of
    Nothing ->
      error $
      "Cannot represent variable " ++ n ++
      " as a source of statistics based on double floating point values: extractDoubleStatsResults"
    Just a ->
      ResultExtract n i a s

-- | Extract the results as statistics based on double floating point values
-- and optimised for fast aggregation, or raise a conversion error.
extractDoubleStatsEitherResults :: Results -> [ResultExtract (Either Double (SamplingStats Double))]
extractDoubleStatsEitherResults rs = flip map (resultsToDoubleStatsEitherValues rs) $ \x ->
  let n = resultValueName x
      i = resultValueId x
      a = resultValueData x
      s = resultValueSignal x
  in case a of
    Nothing ->
      error $
      "Cannot represent variable " ++ n ++
      " as a source of statistics based on double floating point values: extractDoubleStatsEitherResults"
    Just a ->
      ResultExtract n i a s

-- | Extract the results as timing statistics based on double floating point values,
-- or raise a conversion error.
extractDoubleTimingStatsResults :: Results -> [ResultExtract (TimingStats Double)]
extractDoubleTimingStatsResults rs = flip map (resultsToDoubleTimingStatsValues rs) $ \x ->
  let n = resultValueName x
      i = resultValueId x
      a = resultValueData x
      s = resultValueSignal x
  in case a of
    Nothing ->
      error $
      "Cannot represent variable " ++ n ++
      " as a source of timing statistics based on double floating point values: extractDoubleTimingStatsResults"
    Just a ->
      ResultExtract n i a s

-- | Extract the results as string values, or raise a conversion error.
extractStringResults :: Results -> [ResultExtract String]
extractStringResults rs = flip map (resultsToStringValues rs) $ \x ->
  let n = resultValueName x
      i = resultValueId x
      a = resultValueData x
      s = resultValueSignal x
  in case a of
    Nothing ->
      error $
      "Cannot represent variable " ++ n ++
      " as a source of string values: extractStringResults"
    Just a ->
      ResultExtract n i a s

-- | Represents a computation that can return the simulation data.
class ResultComputing m where

  -- | Compute data with the results of simulation.
  computeResultData :: m a -> ResultData a

  -- | Return the signal triggered when data change if such a signal exists.
  computeResultSignal :: m a -> ResultSignal

-- | Return a new result value by the specified name, identifier and computation.
computeResultValue :: ResultComputing m
                      => ResultName
                      -- ^ the result name
                      -> ResultId
                      -- ^ the result identifier
                      -> m a
                      -- ^ the result computation
                      -> ResultValue a
computeResultValue name i m =
  ResultValue {
    resultValueName   = name,
    resultValueId     = i,
    resultValueData   = computeResultData m,
    resultValueSignal = computeResultSignal m }

-- | Represents a computation that can return the simulation data.
data ResultComputation a =
  ResultComputation { resultComputationData :: ResultData a,
                      -- ^ Return data from the computation.
                      resultComputationSignal :: ResultSignal
                      -- ^ Return a signal from the computation.
                    }

instance ResultComputing ResultComputation where

  computeResultData = resultComputationData
  computeResultSignal = resultComputationSignal

instance ResultComputing Parameter where

  computeResultData = Just . liftParameter
  computeResultSignal = const UnknownResultSignal

instance ResultComputing Simulation where

  computeResultData = Just . liftSimulation
  computeResultSignal = const UnknownResultSignal

instance ResultComputing Dynamics where

  computeResultData = Just . liftDynamics
  computeResultSignal = const UnknownResultSignal

instance ResultComputing Event where

  computeResultData = Just . id
  computeResultSignal = const UnknownResultSignal

instance ResultComputing Ref where

  computeResultData = Just . readRef
  computeResultSignal = ResultSignal . refChanged_

instance ResultComputing LR.Ref where

  computeResultData = Just . LR.readRef
  computeResultSignal = const UnknownResultSignal

instance ResultComputing Var where

  computeResultData = Just . readVar
  computeResultSignal = ResultSignal . varChanged_

instance ResultComputing Signalable where

  computeResultData = Just . readSignalable
  computeResultSignal = ResultSignal . signalableChanged_
      
-- | Return a source by the specified statistics.
samplingStatsResultSource :: (ResultItemable (ResultValue a),
                              ResultItemable (ResultValue (SamplingStats a)))
                             => ResultValue (SamplingStats a)
                             -- ^ the statistics
                             -> ResultSource
samplingStatsResultSource x =
  ResultObjectSource $
  ResultObject {
    resultObjectName      = resultValueName x,
    resultObjectId        = resultValueId x,
    resultObjectTypeId    = SamplingStatsId,
    resultObjectSignal    = resultValueSignal x,
    resultObjectSummary   = samplingStatsResultSummary x,
    resultObjectProperties = [
      resultContainerMapProperty c "count" SamplingStatsCountId samplingStatsCount,
      resultContainerMapProperty c "mean" SamplingStatsMeanId samplingStatsMean,
      resultContainerMapProperty c "mean2" SamplingStatsMean2Id samplingStatsMean2,
      resultContainerMapProperty c "std" SamplingStatsDeviationId samplingStatsDeviation,
      resultContainerMapProperty c "var" SamplingStatsVarianceId samplingStatsVariance,
      resultContainerMapProperty c "min" SamplingStatsMinId samplingStatsMin,
      resultContainerMapProperty c "max" SamplingStatsMaxId samplingStatsMax ] }
  where
    c = resultValueToContainer x

-- | Return the summary by the specified statistics.
samplingStatsResultSummary :: ResultItemable (ResultValue (SamplingStats a))
                              => ResultValue (SamplingStats a)
                              -- ^ the statistics
                           -> ResultSource
samplingStatsResultSummary = ResultItemSource . ResultItem . resultItemToStringValue 
  
-- | Return a source by the specified timing statistics.
timingStatsResultSource :: (TimingData a,
                            ResultItemable (ResultValue a),
                            ResultItemable (ResultValue (TimingStats a)))
                           => ResultValue (TimingStats a)
                           -- ^ the statistics
                           -> ResultSource
timingStatsResultSource x =
  ResultObjectSource $
  ResultObject {
    resultObjectName      = resultValueName x,
    resultObjectId        = resultValueId x,
    resultObjectTypeId    = TimingStatsId,
    resultObjectSignal    = resultValueSignal x,
    resultObjectSummary   = timingStatsResultSummary x,
    resultObjectProperties = [
      resultContainerMapProperty c "count" TimingStatsCountId timingStatsCount,
      resultContainerMapProperty c "mean" TimingStatsMeanId timingStatsMean,
      resultContainerMapProperty c "std" TimingStatsDeviationId timingStatsDeviation,
      resultContainerMapProperty c "var" TimingStatsVarianceId timingStatsVariance,
      resultContainerMapProperty c "min" TimingStatsMinId timingStatsMin,
      resultContainerMapProperty c "max" TimingStatsMaxId timingStatsMax,
      resultContainerMapProperty c "minTime" TimingStatsMinTimeId timingStatsMinTime,
      resultContainerMapProperty c "maxTime" TimingStatsMaxTimeId timingStatsMaxTime,
      resultContainerMapProperty c "startTime" TimingStatsStartTimeId timingStatsStartTime,
      resultContainerMapProperty c "lastTime" TimingStatsLastTimeId timingStatsLastTime,
      resultContainerMapProperty c "sum" TimingStatsSumId timingStatsSum,
      resultContainerMapProperty c "sum2" TimingStatsSum2Id timingStatsSum2 ] }
  where
    c = resultValueToContainer x

-- | Return the summary by the specified timing statistics.
timingStatsResultSummary :: (TimingData a, ResultItemable (ResultValue (TimingStats a)))
                            => ResultValue (TimingStats a) 
                            -- ^ the statistics
                            -> ResultSource
timingStatsResultSummary = ResultItemSource . ResultItem . resultItemToStringValue
  
-- | Return a source by the specified finite queue.
queueResultSource :: (Show si, Show sm, Show so,
                      ResultItemable (ResultValue si),
                      ResultItemable (ResultValue sm),
                      ResultItemable (ResultValue so))
                     => ResultContainer (Q.Queue si qi sm qm so qo a)
                     -- ^ the queue container
                     -> ResultSource
queueResultSource c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = FiniteQueueId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = queueResultSummary c,
    resultObjectProperties = [
      resultContainerConstProperty c "enqueueStrategy" EnqueueStrategyId Q.enqueueStrategy,
      resultContainerConstProperty c "enqueueStoringStrategy" EnqueueStoringStrategyId Q.enqueueStoringStrategy,
      resultContainerConstProperty c "dequeueStrategy" DequeueStrategyId Q.dequeueStrategy,
      resultContainerProperty c "queueNull" QueueNullId Q.queueNull Q.queueNullChanged_,
      resultContainerProperty c "queueFull" QueueFullId Q.queueFull Q.queueFullChanged_,
      resultContainerConstProperty c "queueMaxCount" QueueMaxCountId Q.queueMaxCount,
      resultContainerProperty c "queueCount" QueueCountId Q.queueCount Q.queueCountChanged_,
      resultContainerProperty c "queueCountStats" QueueCountStatsId Q.queueCountStats Q.queueCountChanged_,
      resultContainerProperty c "enqueueCount" EnqueueCountId Q.enqueueCount Q.enqueueCountChanged_,
      resultContainerProperty c "enqueueLostCount" EnqueueLostCountId Q.enqueueLostCount Q.enqueueLostCountChanged_,
      resultContainerProperty c "enqueueStoreCount" EnqueueStoreCountId Q.enqueueStoreCount Q.enqueueStoreCountChanged_,
      resultContainerProperty c "dequeueCount" DequeueCountId Q.dequeueCount Q.dequeueCountChanged_,
      resultContainerProperty c "dequeueExtractCount" DequeueExtractCountId Q.dequeueExtractCount Q.dequeueExtractCountChanged_,
      resultContainerProperty c "queueLoadFactor" QueueLoadFactorId Q.queueLoadFactor Q.queueLoadFactorChanged_,
      resultContainerIntegProperty c "enqueueRate" EnqueueRateId Q.enqueueRate,
      resultContainerIntegProperty c "enqueueStoreRate" EnqueueStoreRateId Q.enqueueStoreRate,
      resultContainerIntegProperty c "dequeueRate" DequeueRateId Q.dequeueRate,
      resultContainerIntegProperty c "dequeueExtractRate" DequeueExtractRateId Q.dequeueExtractRate,
      resultContainerProperty c "queueWaitTime" QueueWaitTimeId Q.queueWaitTime Q.queueWaitTimeChanged_,
      resultContainerProperty c "queueTotalWaitTime" QueueTotalWaitTimeId Q.queueTotalWaitTime Q.queueTotalWaitTimeChanged_,
      resultContainerProperty c "enqueueWaitTime" EnqueueWaitTimeId Q.enqueueWaitTime Q.enqueueWaitTimeChanged_,
      resultContainerProperty c "dequeueWaitTime" DequeueWaitTimeId Q.dequeueWaitTime Q.dequeueWaitTimeChanged_,
      resultContainerProperty c "queueRate" QueueRateId Q.queueRate Q.queueRateChanged_ ] }

-- | Return the summary by the specified finite queue.
queueResultSummary :: (Show si, Show sm, Show so)
                      => ResultContainer (Q.Queue si qi sm qm so qo a)
                      -- ^ the queue container
                      -> ResultSource
queueResultSummary c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = FiniteQueueId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = queueResultSummary c,
    resultObjectProperties = [
      resultContainerConstProperty c "queueMaxCount" QueueMaxCountId Q.queueMaxCount,
      resultContainerProperty c "queueCountStats" QueueCountStatsId Q.queueCountStats Q.queueCountChanged_,
      resultContainerProperty c "enqueueCount" EnqueueCountId Q.enqueueCount Q.enqueueCountChanged_,
      resultContainerProperty c "enqueueLostCount" EnqueueLostCountId Q.enqueueLostCount Q.enqueueLostCountChanged_,
      resultContainerProperty c "enqueueStoreCount" EnqueueStoreCountId Q.enqueueStoreCount Q.enqueueStoreCountChanged_,
      resultContainerProperty c "dequeueCount" DequeueCountId Q.dequeueCount Q.dequeueCountChanged_,
      resultContainerProperty c "dequeueExtractCount" DequeueExtractCountId Q.dequeueExtractCount Q.dequeueExtractCountChanged_,
      resultContainerProperty c "queueLoadFactor" QueueLoadFactorId Q.queueLoadFactor Q.queueLoadFactorChanged_,
      resultContainerProperty c "queueWaitTime" QueueWaitTimeId Q.queueWaitTime Q.queueWaitTimeChanged_,
      resultContainerProperty c "queueRate" QueueRateId Q.queueRate Q.queueRateChanged_ ] }

-- | Return a source by the specified infinite queue.
infiniteQueueResultSource :: (Show sm, Show so,
                              ResultItemable (ResultValue sm),
                              ResultItemable (ResultValue so))
                             => ResultContainer (IQ.Queue sm qm so qo a)
                             -- ^ the queue container
                             -> ResultSource
infiniteQueueResultSource c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = FiniteQueueId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = infiniteQueueResultSummary c,
    resultObjectProperties = [
      resultContainerConstProperty c "enqueueStoringStrategy" EnqueueStoringStrategyId IQ.enqueueStoringStrategy,
      resultContainerConstProperty c "dequeueStrategy" DequeueStrategyId IQ.dequeueStrategy,
      resultContainerProperty c "queueNull" QueueNullId IQ.queueNull IQ.queueNullChanged_,
      resultContainerProperty c "queueCount" QueueCountId IQ.queueCount IQ.queueCountChanged_,
      resultContainerProperty c "queueCountStats" QueueCountStatsId IQ.queueCountStats IQ.queueCountChanged_,
      resultContainerProperty c "enqueueStoreCount" EnqueueStoreCountId IQ.enqueueStoreCount IQ.enqueueStoreCountChanged_,
      resultContainerProperty c "dequeueCount" DequeueCountId IQ.dequeueCount IQ.dequeueCountChanged_,
      resultContainerProperty c "dequeueExtractCount" DequeueExtractCountId IQ.dequeueExtractCount IQ.dequeueExtractCountChanged_,
      resultContainerIntegProperty c "enqueueStoreRate" EnqueueStoreRateId IQ.enqueueStoreRate,
      resultContainerIntegProperty c "dequeueRate" DequeueRateId IQ.dequeueRate,
      resultContainerIntegProperty c "dequeueExtractRate" DequeueExtractRateId IQ.dequeueExtractRate,
      resultContainerProperty c "queueWaitTime" QueueWaitTimeId IQ.queueWaitTime IQ.queueWaitTimeChanged_,
      resultContainerProperty c "dequeueWaitTime" DequeueWaitTimeId IQ.dequeueWaitTime IQ.dequeueWaitTimeChanged_,
      resultContainerProperty c "queueRate" QueueRateId IQ.queueRate IQ.queueRateChanged_ ] }

-- | Return the summary by the specified infinite queue.
infiniteQueueResultSummary :: (Show sm, Show so)
                              => ResultContainer (IQ.Queue sm qm so qo a)
                              -- ^ the queue container
                              -> ResultSource
infiniteQueueResultSummary c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = FiniteQueueId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = infiniteQueueResultSummary c,
    resultObjectProperties = [
      resultContainerProperty c "queueCountStats" QueueCountStatsId IQ.queueCountStats IQ.queueCountChanged_,
      resultContainerProperty c "enqueueStoreCount" EnqueueStoreCountId IQ.enqueueStoreCount IQ.enqueueStoreCountChanged_,
      resultContainerProperty c "dequeueCount" DequeueCountId IQ.dequeueCount IQ.dequeueCountChanged_,
      resultContainerProperty c "dequeueExtractCount" DequeueExtractCountId IQ.dequeueExtractCount IQ.dequeueExtractCountChanged_,
      resultContainerProperty c "queueWaitTime" QueueWaitTimeId IQ.queueWaitTime IQ.queueWaitTimeChanged_,
      resultContainerProperty c "queueRate" QueueRateId IQ.queueRate IQ.queueRateChanged_ ] }
  
-- | Return a source by the specified arrival timer.
arrivalTimerResultSource :: ResultContainer ArrivalTimer
                            -- ^ the arrival timer container
                            -> ResultSource
arrivalTimerResultSource c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = ArrivalTimerId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = arrivalTimerResultSummary c,
    resultObjectProperties = [
      resultContainerProperty c "processingTime" ArrivalProcessingTimeId arrivalProcessingTime arrivalProcessingTimeChanged_ ] }

-- | Return the summary by the specified arrival timer.
arrivalTimerResultSummary :: ResultContainer ArrivalTimer
                             -- ^ the arrival timer container
                             -> ResultSource
arrivalTimerResultSummary c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = ArrivalTimerId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = arrivalTimerResultSummary c,
    resultObjectProperties = [
      resultContainerProperty c "processingTime" ArrivalProcessingTimeId arrivalProcessingTime arrivalProcessingTimeChanged_ ] }

-- | Return a source by the specified server.
serverResultSource :: (Show s, ResultItemable (ResultValue s))
                      => ResultContainer (Server s a b)
                      -- ^ the server container
                      -> ResultSource
serverResultSource c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = ServerId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = serverResultSummary c,
    resultObjectProperties = [
      resultContainerConstProperty c "initState" ServerInitStateId serverInitState,
      resultContainerProperty c "state" ServerStateId serverState serverStateChanged_,
      resultContainerProperty c "totalInputWaitTime" ServerTotalInputWaitTimeId serverTotalInputWaitTime serverTotalInputWaitTimeChanged_,
      resultContainerProperty c "totalProcessingTime" ServerTotalProcessingTimeId serverTotalProcessingTime serverTotalProcessingTimeChanged_,
      resultContainerProperty c "totalOutputWaitTime" ServerTotalOutputWaitTimeId serverTotalOutputWaitTime serverTotalOutputWaitTimeChanged_,
      resultContainerProperty c "inputWaitTime" ServerInputWaitTimeId serverInputWaitTime serverInputWaitTimeChanged_,
      resultContainerProperty c "processingTime" ServerProcessingTimeId serverProcessingTime serverProcessingTimeChanged_,
      resultContainerProperty c "outputWaitTime" ServerOutputWaitTimeId serverOutputWaitTime serverOutputWaitTimeChanged_,
      resultContainerProperty c "inputWaitFactor" ServerInputWaitFactorId serverInputWaitFactor serverInputWaitFactorChanged_,
      resultContainerProperty c "processingFactor" ServerProcessingFactorId serverProcessingFactor serverProcessingFactorChanged_,
      resultContainerProperty c "outputWaitFactor" ServerOutputWaitFactorId serverOutputWaitFactor serverOutputWaitFactorChanged_ ] }

-- | Return the summary by the specified server.
serverResultSummary :: ResultContainer (Server s a b)
                       -- ^ the server container
                       -> ResultSource
serverResultSummary c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = ServerId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = serverResultSummary c,
    resultObjectProperties = [
      resultContainerProperty c "inputWaitTime" ServerInputWaitTimeId serverInputWaitTime serverInputWaitTimeChanged_,
      resultContainerProperty c "processingTime" ServerProcessingTimeId serverProcessingTime serverProcessingTimeChanged_,
      resultContainerProperty c "outputWaitTime" ServerOutputWaitTimeId serverOutputWaitTime serverOutputWaitTimeChanged_,
      resultContainerProperty c "inputWaitFactor" ServerInputWaitFactorId serverInputWaitFactor serverInputWaitFactorChanged_,
      resultContainerProperty c "processingFactor" ServerProcessingFactorId serverProcessingFactor serverProcessingFactorChanged_,
      resultContainerProperty c "outputWaitFactor" ServerOutputWaitFactorId serverOutputWaitFactor serverOutputWaitFactorChanged_ ] }

-- | Return an arbitrary text as a separator source.
textResultSource :: String -> ResultSource
textResultSource text =
  ResultSeparatorSource $
  ResultSeparator { resultSeparatorText = text }

-- | Return the source of the modeling time.
timeResultSource :: ResultSource
timeResultSource = resultSource' "t" TimeId time
                         
-- | Make an integer subscript
intSubscript :: Int -> ResultName
intSubscript i = "[" ++ show i ++ "]"

instance ResultComputing m => ResultProvider (m Double) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ computeResultValue name i m

instance ResultComputing m => ResultProvider (m [Double]) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ computeResultValue name i m

instance ResultComputing m => ResultProvider (m (SamplingStats Double)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ computeResultValue name i m

instance ResultComputing m => ResultProvider (m (TimingStats Double)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ computeResultValue name i m

instance ResultComputing m => ResultProvider (m Int) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ computeResultValue name i m

instance ResultComputing m => ResultProvider (m [Int]) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ computeResultValue name i m

instance ResultComputing m => ResultProvider (m (SamplingStats Int)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ computeResultValue name i m

instance ResultComputing m => ResultProvider (m (TimingStats Int)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ computeResultValue name i m

instance ResultComputing m => ResultProvider (m String) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ computeResultValue name i m

instance ResultProvider p => ResultProvider [p] where

  resultSource' name i m =
    resultSource' name i $ ResultListWithSubscript m subscript where
      subscript = map snd $ zip m $ map intSubscript [0..]

instance (Show i, Ix i, ResultProvider p) => ResultProvider (A.Array i p) where

  resultSource' name i m =
    resultSource' name i $ ResultListWithSubscript items subscript where
      items = A.elems m
      subscript = map (\i -> "[" ++ show i ++ "]") (A.indices m)

#ifndef __HASTE__

instance ResultProvider p => ResultProvider (V.Vector p) where
  
  resultSource' name i m =
    resultSource' name i $ ResultVectorWithSubscript m subscript where
      subscript = V.imap (\i x -> intSubscript i) m

#endif

-- | Represents a list with the specified subscript.
data ResultListWithSubscript p =
  ResultListWithSubscript [p] [String]

-- | Represents an array with the specified subscript.
data ResultArrayWithSubscript i p =
  ResultArrayWithSubscript (A.Array i p) (A.Array i String)

#ifndef __HASTE__

-- | Represents a vector with the specified subscript.
data ResultVectorWithSubscript p =
  ResultVectorWithSubscript (V.Vector p) (V.Vector String)

#endif

instance ResultProvider p => ResultProvider (ResultListWithSubscript p) where

  resultSource' name i (ResultListWithSubscript xs ys) =
    ResultVectorSource $
    memoResultVectorSignal $
    memoResultVectorSummary $
    ResultVector { resultVectorName = name,
                   resultVectorId = i,
                   resultVectorItems = axs,
                   resultVectorSubscript = ays,
                   resultVectorSignal = undefined,
                   resultVectorSummary = undefined }
    where
      bnds   = (0, length xs - 1)
      axs    = A.listArray bnds items
      ays    = A.listArray bnds ys
      items  =
        flip map (zip ys xs) $ \(y, x) ->
        let name' = name ++ y
        in resultSource' name' (VectorItemId y) x
      items' = map resultSourceSummary items
    
instance (Show i, Ix i, ResultProvider p) => ResultProvider (ResultArrayWithSubscript i p) where

  resultSource' name i (ResultArrayWithSubscript xs ys) =
    resultSource' name i $ ResultListWithSubscript items subscript where
      items = A.elems xs
      subscript = A.elems ys
      
#ifndef __HASTE__

instance ResultProvider p => ResultProvider (ResultVectorWithSubscript p) where

  resultSource' name i (ResultVectorWithSubscript xs ys) =
    ResultVectorSource $
    memoResultVectorSignal $
    memoResultVectorSummary $
    ResultVector { resultVectorName = name,
                   resultVectorId = i,
                   resultVectorItems = axs,
                   resultVectorSubscript = ays,
                   resultVectorSignal = undefined,
                   resultVectorSummary = undefined }
    where
      bnds   = (0, V.length xs - 1)
      axs    = A.listArray bnds (V.toList items)
      ays    = A.listArray bnds (V.toList ys)
      items =
        V.generate (V.length xs) $ \i ->
        let x = xs V.! i
            y = ys V.! i
            name' = name ++ y
        in resultSource' name' (VectorItemId y) x
      items' = V.map resultSourceSummary items

#endif

instance (Ix i, Show i, ResultComputing m) => ResultProvider (m (A.Array i Double)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ fmap A.elems $ computeResultValue name i m

instance (Ix i, Show i, ResultComputing m) => ResultProvider (m (A.Array i Int)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ fmap A.elems $ computeResultValue name i m

#ifndef __HASTE__

instance ResultComputing m => ResultProvider (m (V.Vector Double)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ fmap V.toList $ computeResultValue name i m

instance ResultComputing m => ResultProvider (m (V.Vector Int)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ fmap V.toList $ computeResultValue name i m

#endif

instance (Show si, Show sm, Show so,
          ResultItemable (ResultValue si),
          ResultItemable (ResultValue sm),
          ResultItemable (ResultValue so))
         => ResultProvider (Q.Queue si qi sm qm so qo a) where

  resultSource' name i m =
    queueResultSource $ ResultContainer name i m (ResultSignal $ Q.queueChanged_ m)

instance (Show sm, Show so,
          ResultItemable (ResultValue sm),
          ResultItemable (ResultValue so))
         => ResultProvider (IQ.Queue sm qm so qo a) where

  resultSource' name i m =
    infiniteQueueResultSource $ ResultContainer name i m (ResultSignal $ IQ.queueChanged_ m)

instance ResultProvider ArrivalTimer where

  resultSource' name i m =
    arrivalTimerResultSource $ ResultContainer name i m (ResultSignal $ arrivalProcessingTimeChanged_ m)

instance (Show s, ResultItemable (ResultValue s)) => ResultProvider (Server s a b) where

  resultSource' name i m =
    serverResultSource $ ResultContainer name i m (ResultSignal $ serverChanged_ m)
