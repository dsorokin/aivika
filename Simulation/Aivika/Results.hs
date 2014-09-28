
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, ExistentialQuantification #-}

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
import qualified Data.Array as A
import qualified Data.Vector as V

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
import Simulation.Aivika.QueueStrategy
import qualified Simulation.Aivika.Queue as Q
import qualified Simulation.Aivika.Queue.Infinite as IQ
import Simulation.Aivika.Arrival
import Simulation.Aivika.Server
import Simulation.Aivika.Results.Locale

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
  -- properties as it takes place with the 'resultItemExpansion'
  -- function.
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
                 resultVectorSubscript :: A.Array Int String,
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
resultValueToContainer :: ResultId -> ResultValue a -> ResultContainer (ResultData a)
resultValueToContainer i x =
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

-- | Return a signal emitted by the source with eliminated 'EmptyResultSignal' as possible.
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

-- | Represent the result source as timing statistics based on double floating point numbers.
resultSourceToDoubleTimingStatsValues :: ResultSource -> [ResultValue (TimingStats Double)]
resultSourceToDoubleTimingStatsValues = map (\(ResultItem x) -> resultItemToDoubleTimingStatsValue x) . flattenResultSource

-- | Represent the result source as 'String' values.
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

-- | Represent the results as timing statistics based on double floating point numbers.
resultsToDoubleTimingStatsValues :: Results -> [ResultValue (TimingStats Double)]
resultsToDoubleTimingStatsValues = concat . map resultSourceToDoubleTimingStatsValues . resultSourceList

-- | Represent the results as 'String' values.
resultsToStringValues :: Results -> [ResultValue String]
resultsToStringValues = concat . map resultSourceToStringValues . resultSourceList

-- | Return a signal emitted by the specified results.
resultSignal :: Results -> ResultSignal
resultSignal = mconcat . map resultSourceSignal . resultSourceList

-- | Return an expanded version of the simulation results expanding the properties as possible, which
-- takes place for expanding statistics to show the count, average, deviation, minimum, maximum and so on
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
resultByProperty label rs =
  flip composeResults rs $ \x ->
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
    x ->
      error $
      "Result source " ++ resultSourceName x ++
      " is not object" ++
      ": resultByProperty"

-- | Compose the results using the specified transformation function.
composeResults :: (ResultSource -> [ResultSource]) -> ResultTransform
composeResults f =
  results . concat . map f . resultSourceList

-- | Concatenate the results using the specified list of transformation functions.
concatResults :: [ResultTransform] -> ResultTransform
concatResults trs rs =
  results $ concat $ map (\tr -> resultSourceList $ tr rs) trs

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
      makeProperty "count" SamplingStatsCountId samplingStatsCount,
      makeProperty "mean" SamplingStatsMeanId samplingStatsMean,
      makeProperty "mean2" SamplingStatsMean2Id samplingStatsMean2,
      makeProperty "std" SamplingStatsDeviationId samplingStatsDeviation,
      makeProperty "var" SamplingStatsVarianceId samplingStatsVariance,
      makeProperty "min" SamplingStatsMinId samplingStatsMin,
      makeProperty "max" SamplingStatsMaxId samplingStatsMax ] }
  where
    makeProperty name' i f =
      ResultProperty { resultPropertyLabel  = name',
                       resultPropertyId     = i,
                       resultPropertySource = makeSource name' i f }
    makeSource name' i f =
      ResultItemSource $
      ResultItem $
      ResultValue { resultValueName   = resultValueName x ++ "." ++ name',
                    resultValueId     = i,
                    resultValueData   = fmap (fmap f) $ resultValueData x,
                    resultValueSignal = resultValueSignal x }
  
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
      makeProperty "count" TimingStatsCountId timingStatsCount,
      makeProperty "mean" TimingStatsMeanId timingStatsMean,
      makeProperty "std" TimingStatsDeviationId timingStatsDeviation,
      makeProperty "var" TimingStatsVarianceId timingStatsVariance,
      makeProperty "min" TimingStatsMinId timingStatsMin,
      makeProperty "max" TimingStatsMaxId timingStatsMax,
      makeProperty "minTime" TimingStatsMinTimeId timingStatsMinTime,
      makeProperty "maxTime" TimingStatsMaxTimeId timingStatsMaxTime,
      makeProperty "startTime" TimingStatsStartTimeId timingStatsStartTime,
      makeProperty "lastTime" TimingStatsLastTimeId timingStatsLastTime,
      makeProperty "sum" TimingStatsSumId timingStatsSum,
      makeProperty "sum2" TimingStatsSum2Id timingStatsSum2 ] }
  where
    makeProperty name' i f =
      ResultProperty { resultPropertyLabel  = name',
                       resultPropertyId     = i,
                       resultPropertySource = makeSource name' i f }
    makeSource name' i f =
      ResultItemSource $
      ResultItem $
      ResultValue { resultValueName   = resultValueName x ++ "." ++ name',
                    resultValueId     = i,
                    resultValueData   = fmap (fmap f) $ resultValueData x,
                    resultValueSignal = resultValueSignal x }
  
-- | Return a source by the specified finite queue.
queueResultSource :: (Show si, Show sm, Show so,
                      ResultItemable (ResultValue si),
                      ResultItemable (ResultValue sm),
                      ResultItemable (ResultValue so))
                     => ResultName
                     -- ^ the result name
                     -> ResultId
                     -- ^ the result identifier
                     -> Q.Queue si qi sm qm so qo a
                     -- ^ the queue
                     -> ResultSource
queueResultSource name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = FiniteQueueId,
    resultObjectSignal = ResultSignal $ Q.queueChanged_ m,
    resultObjectSummary = queueResultSummary name i m,
    resultObjectProperties = [
      makeProperty0 "enqueueStrategy" EnqueueStrategyId Q.enqueueStrategy,
      makeProperty0 "enqueueStoringStrategy" EnqueueStoringStrategyId Q.enqueueStoringStrategy,
      makeProperty0 "dequeueStrategy" DequeueStrategyId Q.dequeueStrategy,
      makeProperty "queueNull" QueueNullId Q.queueNull Q.queueNullChanged_,
      makeProperty "queueFull" QueueFullId Q.queueFull Q.queueFullChanged_,
      makeProperty0 "queueMaxCount" QueueMaxCountId Q.queueMaxCount,
      makeProperty "queueCount" QueueCountId Q.queueCount Q.queueCountChanged_,
      makeProperty "queueCountStats" QueueCountStatsId Q.queueCountStats Q.queueCountChanged_,
      makeProperty "enqueueCount" EnqueueCountId Q.enqueueCount Q.enqueueCountChanged_,
      makeProperty "enqueueLostCount" EnqueueLostCountId Q.enqueueLostCount Q.enqueueLostCountChanged_,
      makeProperty "enqueueStoreCount" EnqueueStoreCountId Q.enqueueStoreCount Q.enqueueStoreCountChanged_,
      makeProperty "dequeueCount" DequeueCountId Q.dequeueCount Q.dequeueCountChanged_,
      makeProperty "dequeueExtractCount" DequeueExtractCountId Q.dequeueExtractCount Q.dequeueExtractCountChanged_,
      makeProperty "queueLoadFactor" QueueLoadFactorId Q.queueLoadFactor Q.queueLoadFactorChanged_,
      makeProperty1 "enqueueRate" EnqueueRateId Q.enqueueRate,
      makeProperty1 "enqueueStoreRate" EnqueueStoreRateId Q.enqueueStoreRate,
      makeProperty1 "dequeueRate" DequeueRateId Q.dequeueRate,
      makeProperty1 "dequeueExtractRate" DequeueExtractRateId Q.dequeueExtractRate,
      makeProperty "queueWaitTime" QueueWaitTimeId Q.queueWaitTime Q.queueWaitTimeChanged_,
      makeProperty "queueTotalWaitTime" QueueTotalWaitTimeId Q.queueTotalWaitTime Q.queueTotalWaitTimeChanged_,
      makeProperty "enqueueWaitTime" EnqueueWaitTimeId Q.enqueueWaitTime Q.enqueueWaitTimeChanged_,
      makeProperty "dequeueWaitTime" DequeueWaitTimeId Q.dequeueWaitTime Q.dequeueWaitTimeChanged_,
      makeProperty "queueRate" QueueRateId Q.queueRate Q.queueRateChanged_ ] }
  where
    makeProperty0 name' i f =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . return . f) (const EmptyResultSignal) }
    makeProperty1 name' i f =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . f) (const UnknownResultSignal) }
    makeProperty name' i f g =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . f) (ResultSignal . g) }
    makeSource name' i f g =
      ResultItemSource $
      ResultItem $
      ResultValue { resultValueName   = name ++ "." ++ name',
                    resultValueId     = i,
                    resultValueData   = f m,
                    resultValueSignal = g m }

-- | Return a source by the specified infinite queue.
infiniteQueueResultSource :: (Show sm, Show so,
                              ResultItemable (ResultValue sm),
                              ResultItemable (ResultValue so))
                             => ResultName
                             -- ^ the result name
                             -> ResultId
                             -- ^ the result identifier
                             -> IQ.Queue sm qm so qo a
                             -- ^ the queue
                             -> ResultSource
infiniteQueueResultSource name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = FiniteQueueId,
    resultObjectSignal = ResultSignal $ IQ.queueChanged_ m,
    resultObjectSummary = infiniteQueueResultSummary name i m,
    resultObjectProperties = [
      makeProperty0 "enqueueStoringStrategy" EnqueueStoringStrategyId IQ.enqueueStoringStrategy,
      makeProperty0 "dequeueStrategy" DequeueStrategyId IQ.dequeueStrategy,
      makeProperty "queueNull" QueueNullId IQ.queueNull IQ.queueNullChanged_,
      makeProperty "queueCount" QueueCountId IQ.queueCount IQ.queueCountChanged_,
      makeProperty "queueCountStats" QueueCountStatsId IQ.queueCountStats IQ.queueCountChanged_,
      makeProperty "enqueueStoreCount" EnqueueStoreCountId IQ.enqueueStoreCount IQ.enqueueStoreCountChanged_,
      makeProperty "dequeueCount" DequeueCountId IQ.dequeueCount IQ.dequeueCountChanged_,
      makeProperty "dequeueExtractCount" DequeueExtractCountId IQ.dequeueExtractCount IQ.dequeueExtractCountChanged_,
      makeProperty1 "enqueueStoreRate" EnqueueStoreRateId IQ.enqueueStoreRate,
      makeProperty1 "dequeueRate" DequeueRateId IQ.dequeueRate,
      makeProperty1 "dequeueExtractRate" DequeueExtractRateId IQ.dequeueExtractRate,
      makeProperty "queueWaitTime" QueueWaitTimeId IQ.queueWaitTime IQ.queueWaitTimeChanged_,
      makeProperty "dequeueWaitTime" DequeueWaitTimeId IQ.dequeueWaitTime IQ.dequeueWaitTimeChanged_,
      makeProperty "queueRate" QueueRateId IQ.queueRate IQ.queueRateChanged_ ] }
  where
    makeProperty0 name' i f =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . return . f) (const EmptyResultSignal) }
    makeProperty1 name' i f =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . f) (const UnknownResultSignal) }
    makeProperty name' i f g =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . f) (ResultSignal . g) }
    makeSource name' i f g =
      ResultItemSource $
      ResultItem $
      ResultValue { resultValueName   = name ++ "." ++ name',
                    resultValueId     = i,
                    resultValueData   = f m,
                    resultValueSignal = g m }
  
-- | Return a source by the specified arrival timer.
arrivalTimerResultSource :: ResultName
                            -- ^ the result name
                            -> ResultId
                            -- ^ the result identifier
                            -> ArrivalTimer
                            -- ^ the arrival timer
                            -> ResultSource
arrivalTimerResultSource name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = ArrivalTimerId,
    resultObjectSignal = ResultSignal $ arrivalProcessingTimeChanged_ m,
    resultObjectSummary = arrivalTimerResultSummary name i m,
    resultObjectProperties = [
      makeProperty "processingTime" ArrivalProcessingTimeId arrivalProcessingTime arrivalProcessingTimeChanged_ ] }
  where
    makeProperty name' i f g =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . f) (ResultSignal . g) }
    makeSource name' i f g =
      ResultItemSource $
      ResultItem $
      ResultValue { resultValueName   = name ++ "." ++ name',
                    resultValueId     = i,
                    resultValueData   = f m,
                    resultValueSignal = g m }

-- | Return a source by the specified server.
serverResultSource :: (Show s, ResultItemable (ResultValue s))
                      => ResultName
                      -- ^ the result name
                      -> ResultId
                      -- ^ the result identifier
                      -> Server s a b
                      -- ^ the server
                      -> ResultSource
serverResultSource name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = ServerId,
    resultObjectSignal = ResultSignal $ serverChanged_ m,
    resultObjectSummary = serverResultSummary name i m,
    resultObjectProperties = [
      makeProperty0 "initState" ServerInitStateId serverInitState,
      makeProperty "state" ServerStateId serverState serverStateChanged_,
      makeProperty "totalInputWaitTime" ServerTotalInputWaitTimeId serverTotalInputWaitTime serverTotalInputWaitTimeChanged_,
      makeProperty "totalProcessingTime" ServerTotalProcessingTimeId serverTotalProcessingTime serverTotalProcessingTimeChanged_,
      makeProperty "totalOutputWaitTime" ServerTotalOutputWaitTimeId serverTotalOutputWaitTime serverTotalOutputWaitTimeChanged_,
      makeProperty "inputWaitTime" ServerInputWaitTimeId serverInputWaitTime serverInputWaitTimeChanged_,
      makeProperty "processingTime" ServerProcessingTimeId serverProcessingTime serverProcessingTimeChanged_,
      makeProperty "outputWaitTime" ServerOutputWaitTimeId serverOutputWaitTime serverOutputWaitTimeChanged_,
      makeProperty "inputWaitFactor" ServerInputWaitFactorId serverInputWaitFactor serverInputWaitFactorChanged_,
      makeProperty "processingFactor" ServerProcessingFactorId serverProcessingFactor serverProcessingFactorChanged_,
      makeProperty "outputWaitFactor" ServerOutputWaitFactorId serverOutputWaitFactor serverOutputWaitFactorChanged_ ] }
  where
    makeProperty0 name' i f =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . return . f) (const EmptyResultSignal) }
    makeProperty name' i f g =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . f) (ResultSignal . g) }
    makeSource name' i f g =
      ResultItemSource $
      ResultItem $
      ResultValue { resultValueName   = name ++ "." ++ name',
                    resultValueId     = i,
                    resultValueData   = f m,
                    resultValueSignal = g m }

-- | Return an arbitrary text as a separator source.
textResultSource :: String -> ResultSource
textResultSource text =
  ResultSeparatorSource $
  ResultSeparator { resultSeparatorText = text }

-- | Return the source of the modeling time.
timeResultSource :: ResultSource
timeResultSource = resultSource' "t" TimeId time

-- | Return the summary by the specified statistics.
samplingStatsResultSummary :: ResultItemable (ResultValue (SamplingStats a))
                              => ResultValue (SamplingStats a)
                              -- ^ the statistics
                           -> ResultSource
samplingStatsResultSummary = ResultItemSource . ResultItem . resultItemToStringValue 

-- | Return the summary by the specified timing statistics.
timingStatsResultSummary :: (TimingData a, ResultItemable (ResultValue (TimingStats a)))
                            => ResultValue (TimingStats a) 
                            -- ^ the statistics
                            -> ResultSource
timingStatsResultSummary = ResultItemSource . ResultItem . resultItemToStringValue
                         
-- | Return the summary by the specified finite queue.
queueResultSummary :: (Show si, Show sm, Show so)
                      => ResultName
                      -- ^ the result name
                      -> ResultId
                      -- ^ the result identifier
                      -> Q.Queue si qi sm qm so qo a
                      -- ^ the queue
                      -> ResultSource
queueResultSummary name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = FiniteQueueId,
    resultObjectSignal = ResultSignal $ Q.queueChanged_ m,
    resultObjectSummary = queueResultSummary name i m,
    resultObjectProperties = [
      makeProperty0 "queueMaxCount" QueueMaxCountId Q.queueMaxCount,
      makeProperty "queueCountStats" QueueCountStatsId Q.queueCountStats Q.queueCountChanged_,
      makeProperty "enqueueCount" EnqueueCountId Q.enqueueCount Q.enqueueCountChanged_,
      makeProperty "enqueueLostCount" EnqueueLostCountId Q.enqueueLostCount Q.enqueueLostCountChanged_,
      makeProperty "enqueueStoreCount" EnqueueStoreCountId Q.enqueueStoreCount Q.enqueueStoreCountChanged_,
      makeProperty "dequeueCount" DequeueCountId Q.dequeueCount Q.dequeueCountChanged_,
      makeProperty "dequeueExtractCount" DequeueExtractCountId Q.dequeueExtractCount Q.dequeueExtractCountChanged_,
      makeProperty "queueLoadFactor" QueueLoadFactorId Q.queueLoadFactor Q.queueLoadFactorChanged_,
      makeProperty "queueWaitTime" QueueWaitTimeId Q.queueWaitTime Q.queueWaitTimeChanged_,
      makeProperty "queueRate" QueueRateId Q.queueRate Q.queueRateChanged_ ] }
  where
    makeProperty0 name' i f =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . return . f) (const EmptyResultSignal) }
    makeProperty name' i f g =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . f) (ResultSignal . g) }
    makeSource name' i f g =
      ResultItemSource $
      ResultItem $
      ResultValue { resultValueName   = name ++ "." ++ name',
                    resultValueId     = i,
                    resultValueData   = f m,
                    resultValueSignal = g m }
  
-- | Return the summary by the specified infinite queue.
infiniteQueueResultSummary :: (Show sm, Show so)
                              => ResultName
                              -- ^ the result name
                              -> ResultId
                              -- ^ the result identifier
                              -> IQ.Queue sm qm so qo a
                              -- ^ the queue
                              -> ResultSource
infiniteQueueResultSummary name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = FiniteQueueId,
    resultObjectSignal = ResultSignal $ IQ.queueChanged_ m,
    resultObjectSummary = infiniteQueueResultSummary name i m,
    resultObjectProperties = [
      makeProperty "queueCountStats" QueueCountStatsId IQ.queueCountStats IQ.queueCountChanged_,
      makeProperty "enqueueStoreCount" EnqueueStoreCountId IQ.enqueueStoreCount IQ.enqueueStoreCountChanged_,
      makeProperty "dequeueCount" DequeueCountId IQ.dequeueCount IQ.dequeueCountChanged_,
      makeProperty "dequeueExtractCount" DequeueExtractCountId IQ.dequeueExtractCount IQ.dequeueExtractCountChanged_,
      makeProperty "queueWaitTime" QueueWaitTimeId IQ.queueWaitTime IQ.queueWaitTimeChanged_,
      makeProperty "queueRate" QueueRateId IQ.queueRate IQ.queueRateChanged_ ] }
  where
    makeProperty name' i f g =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . f) (ResultSignal . g) }
    makeSource name' i f g =
      ResultItemSource $
      ResultItem $
      ResultValue { resultValueName   = name ++ "." ++ name',
                    resultValueId     = i,
                    resultValueData   = f m,
                    resultValueSignal = g m }
  
-- | Return the summary by the specified arrival timer.
arrivalTimerResultSummary :: ResultName
                             -- ^ the result name
                             -> ResultId
                             -- ^ the result identifier
                             -> ArrivalTimer
                             -- ^ the arrival timer
                             -> ResultSource
arrivalTimerResultSummary name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = ArrivalTimerId,
    resultObjectSignal = ResultSignal $ arrivalProcessingTimeChanged_ m,
    resultObjectSummary = arrivalTimerResultSummary name i m,
    resultObjectProperties = [
      makeProperty "processingTime" ArrivalProcessingTimeId arrivalProcessingTime arrivalProcessingTimeChanged_ ] }
  where
    makeProperty name' i f g =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . f) (ResultSignal . g) }
    makeSource name' i f g =
      ResultItemSource $
      ResultItem $
      ResultValue { resultValueName   = name ++ "." ++ name',
                    resultValueId     = i,
                    resultValueData   = f m,
                    resultValueSignal = g m }

-- | Return the summary by the specified server.
serverResultSummary :: ResultName
                       -- ^ the result name
                       -> ResultId
                       -- ^ the result identifier
                       -> Server s a b
                       -- ^ the server
                       -> ResultSource
serverResultSummary name i m =
  ResultObjectSource $
  ResultObject {
    resultObjectName = name,
    resultObjectId = i,
    resultObjectTypeId = ServerId,
    resultObjectSignal = ResultSignal $ serverChanged_ m,
    resultObjectSummary = serverResultSummary name i m,
    resultObjectProperties = [
      makeProperty "inputWaitTime" ServerInputWaitTimeId serverInputWaitTime serverInputWaitTimeChanged_,
      makeProperty "processingTime" ServerProcessingTimeId serverProcessingTime serverProcessingTimeChanged_,
      makeProperty "outputWaitTime" ServerOutputWaitTimeId serverOutputWaitTime serverOutputWaitTimeChanged_,
      makeProperty "inputWaitFactor" ServerInputWaitFactorId serverInputWaitFactor serverInputWaitFactorChanged_,
      makeProperty "processingFactor" ServerProcessingFactorId serverProcessingFactor serverProcessingFactorChanged_,
      makeProperty "outputWaitFactor" ServerOutputWaitFactorId serverOutputWaitFactor serverOutputWaitFactorChanged_ ] }
  where
    makeProperty name' i f g =
      ResultProperty { resultPropertyLabel = name',
                       resultPropertyId = i,
                       resultPropertySource = makeSource name' i (Just . f) (ResultSignal . g) }
    makeSource name' i f g =
      ResultItemSource $
      ResultItem $
      ResultValue { resultValueName   = name ++ "." ++ name',
                    resultValueId     = i,
                    resultValueData   = f m,
                    resultValueSignal = g m }

-- | Make an integer subscript
intSubscript :: Int -> String
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
      subscript = map show (A.indices m)

instance ResultProvider p => ResultProvider (V.Vector p) where

  resultSource' name i m =
    resultSource' name i $ ResultVectorWithSubscript m subscript where
      subscript = V.imap (\i x -> intSubscript i) m

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

instance (Ix i, Show i, ResultComputing m) => ResultProvider (m (A.Array i Double)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ fmap A.elems $ computeResultValue name i m

instance (Ix i, Show i, ResultComputing m) => ResultProvider (m (A.Array i Int)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ fmap A.elems $ computeResultValue name i m

instance ResultComputing m => ResultProvider (m (V.Vector Double)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ fmap V.toList $ computeResultValue name i m

instance ResultComputing m => ResultProvider (m (V.Vector Int)) where

  resultSource' name i m =
    ResultItemSource $ ResultItem $ fmap V.toList $ computeResultValue name i m

instance (Show si, Show sm, Show so,
          ResultItemable (ResultValue si),
          ResultItemable (ResultValue sm),
          ResultItemable (ResultValue so))
         => ResultProvider (Q.Queue si qi sm qm so qo a) where

  resultSource' = queueResultSource

instance (Show sm, Show so,
          ResultItemable (ResultValue sm),
          ResultItemable (ResultValue so))
         => ResultProvider (IQ.Queue sm qm so qo a) where

  resultSource' = infiniteQueueResultSource

instance ResultProvider ArrivalTimer where

  resultSource' = arrivalTimerResultSource

instance (Show s, ResultItemable (ResultValue s)) => ResultProvider (Server s a b) where

  resultSource' = serverResultSource
