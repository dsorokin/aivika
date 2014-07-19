
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

import System.IO

import Simulation.Aivika.Parameter
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Signal
import Simulation.Aivika.Statistics
import Simulation.Aivika.Ref
import qualified Simulation.Aivika.Ref.Light as LR
import Simulation.Aivika.Var

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

-- | Make a result item output. 
makeResultItemOutput :: ResultComputation m
                        => String
                        -- ^ the result name
                        -> m a
                        -- ^ the result computation
                        -> (Event a -> ResultData)
                        -- ^ transformation
                        -> ResultOutput
makeResultItemOutput name m f =
  ResultItemOutput $
  ResultItem { resultItemName   = name,
               resultItemData   = f $ resultComputationData m,
               resultItemSignal = resultComputationSignal m }

-- | Output the specified statistics.
makeSamplingStatsOutput :: (Show a, ResultComputation m)
                           => String
                           -- ^ the result name
                           -> m (SamplingStats a)
                           -- ^ the statistics
                           -> (Event a -> ResultData)
                           -- ^ transformation
                           -> ResultOutput
makeSamplingStatsOutput name m f =
  ResultObjectOutput $
  ResultObject {
    resultObjectName = name,
    resultObjectId = SamplingStatsId,
    resultObjectProperties = [
      ResultProperty {
         resultPropertyLabel = "count",
         resultPropertyId = SamplingStatsCountId,
         resultPropertyOutput =
           makeResultItemOutput (name ++ ".count") m (IntResultData . fmap samplingStatsCount) },
      ResultProperty {
        resultPropertyLabel = "mean",
        resultPropertyId = SamplingStatsMeanId,
        resultPropertyOutput =
          makeResultItemOutput (name ++ ".mean") m (DoubleResultData . fmap samplingStatsMean) },
      ResultProperty {
        resultPropertyLabel = "mean2",
        resultPropertyId = SamplingStatsMean2Id,
        resultPropertyOutput =
          makeResultItemOutput (name ++ ".mean2") m (DoubleResultData . fmap samplingStatsMean2) },
      ResultProperty {
        resultPropertyLabel = "std",
        resultPropertyId = SamplingStatsDeviationId,
        resultPropertyOutput =
          makeResultItemOutput (name ++ ".std") m (DoubleResultData . fmap samplingStatsDeviation) },
      ResultProperty {
        resultPropertyLabel = "var",
        resultPropertyId = SamplingStatsVarianceId,
        resultPropertyOutput =
          makeResultItemOutput (name ++ ".var") m (DoubleResultData . fmap samplingStatsVariance) },
      ResultProperty {
        resultPropertyLabel = "min",
        resultPropertyId = SamplingStatsMinId,
        resultPropertyOutput =
          makeResultItemOutput (name ++ ".min") m (f . fmap samplingStatsMin) },
      ResultProperty {
        resultPropertyLabel = "max",
        resultPropertyId = SamplingStatsMaxId,
        resultPropertyOutput =
          makeResultItemOutput (name ++ ".max") m (f . fmap samplingStatsMax) } ] }

-- | Output the modeling time.
timeOutput :: ResultOutput
timeOutput = resultOutput (resultSource "t" time) StringResultType

-- | Output an arbitrary text as a separator.
makeTextOutput :: String -> ResultOutput
makeTextOutput text =
  ResultSeparatorOutput $
  ResultSeparator { resultSeparatorText = text }

-- | Make an integer subscript
makeIntSubscript :: Show a => a -> String
makeIntSubscript i = "[" ++ show i ++ "]"

instance ResultComputation m => ResultProvider (m Double) where

  resultSource name m = ResultSource f where
    f DoubleResultType =
      makeResultItemOutput name m DoubleResultData
    f DoubleListResultType =
      makeResultItemOutput name m (DoubleListResultData . fmap return)
    f DoubleStatsResultType =
      makeResultItemOutput name m (DoubleStatsResultData . fmap returnSamplingStats)
    f IntResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntListResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntStatsResultType =
      makeResultItemOutput name m (const NoResultData)
    f StringResultType =
      makeResultItemOutput name m (StringResultData . fmap show)
    f DefaultResultType =
      makeResultItemOutput name m DoubleResultData

instance ResultComputation m => ResultProvider (m [Double]) where

  resultSource name m = ResultSource f where
    f DoubleResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleListResultType =
      makeResultItemOutput name m DoubleListResultData
    f DoubleStatsResultType =
      makeResultItemOutput name m (DoubleStatsResultData . fmap listSamplingStats)
    f IntResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntListResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntStatsResultType =
      makeResultItemOutput name m (const NoResultData)
    f StringResultType =
      makeResultItemOutput name m (StringResultData . fmap show)
    f DefaultResultType =
      makeResultItemOutput name m DoubleListResultData

instance ResultComputation m => ResultProvider (m (SamplingStats Double)) where

  resultSource name m = ResultSource f where
    f DoubleResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleListResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleStatsResultType =
      makeResultItemOutput name m DoubleStatsResultData
    f IntResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntListResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntStatsResultType =
      makeResultItemOutput name m (const NoResultData)
    f StringResultType =
      makeSamplingStatsOutput name m DoubleResultData
    f DefaultResultType =
      makeResultItemOutput name m DoubleStatsResultData

instance ResultComputation m => ResultProvider (m Int) where

  resultSource name m = ResultSource f where
    f DoubleResultType =
      makeResultItemOutput name m (DoubleResultData . fmap fromIntegral)
    f DoubleListResultType =
      makeResultItemOutput name m (DoubleListResultData . fmap (return . fromIntegral))
    f DoubleStatsResultType =
      makeResultItemOutput name m (DoubleStatsResultData . fmap (fromIntSamplingStats . returnSamplingStats))
    f IntResultType =
      makeResultItemOutput name m IntResultData
    f IntListResultType =
      makeResultItemOutput name m (IntListResultData . fmap return)
    f IntStatsResultType =
      makeResultItemOutput name m (IntStatsResultData . fmap returnSamplingStats)
    f StringResultType =
      makeResultItemOutput name m (StringResultData . fmap show)
    f DefaultResultType =
      makeResultItemOutput name m IntResultData

instance ResultComputation m => ResultProvider (m [Int]) where

  resultSource name m = ResultSource f where
    f DoubleResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleListResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleStatsResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntListResultType =
      makeResultItemOutput name m IntListResultData
    f IntStatsResultType =
      makeResultItemOutput name m (IntStatsResultData . fmap listSamplingStats)
    f StringResultType =
      makeResultItemOutput name m (StringResultData . fmap show)
    f DefaultResultType =
      makeResultItemOutput name m IntListResultData

instance ResultComputation m => ResultProvider (m (SamplingStats Int)) where

  resultSource name m = ResultSource f where
    f DoubleResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleListResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleStatsResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntListResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntStatsResultType =
      makeResultItemOutput name m IntStatsResultData
    f StringResultType =
      makeSamplingStatsOutput name m IntResultData
    f DefaultResultType =
      makeResultItemOutput name m IntStatsResultData

instance ResultComputation m => ResultProvider (m String) where

  resultSource name m = ResultSource f where
    f DoubleResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleListResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleStatsResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntListResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntStatsResultType =
      makeResultItemOutput name m (const NoResultData)
    f StringResultType =
      makeResultItemOutput name m StringResultData
    f DefaultResultType =
      makeResultItemOutput name m StringResultData

instance ResultProvider p => ResultProvider [p] where

  resultSource name m = ResultSource f where
    f t =
      ResultVectorOutput $
      ResultVector { resultVectorName = name,
                     resultVectorItems = V.fromList (items t),
                     resultVectorSubscript = V.fromList (subscript t) }
    subscript t =
      flip map (zip [0..] m) $ \(i, x) ->
      makeIntSubscript i
    items t =
      flip map (zip [0..] m) $ \(i, x) ->
      let name' = name ++ makeIntSubscript i
      in resultOutput (resultSource name' x) t

instance (Show i, Ix i, ResultProvider p) => ResultProvider (A.Array i p) where

  resultSource name m = ResultSource f where
    f t =
      ResultVectorOutput $
      ResultVector { resultVectorName = name,
                     resultVectorItems = V.fromList (items t),
                     resultVectorSubscript = V.fromList (subscript t) }
    xs = A.assocs m
    subscript t =
      flip map xs $ \(i, x) ->
      makeIntSubscript i
    items t =
      flip map xs $ \(i, x) ->
      let name' = name ++ makeIntSubscript i
      in resultOutput (resultSource name' x) t

instance ResultProvider p => ResultProvider (V.Vector p) where

  resultSource name m = resultSource name (V.toList m)

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

  resultSource name (ResultArrayWithSubscript xs ys) = ResultSource f where
    f t =
      ResultVectorOutput $
      ResultVector { resultVectorName = name,
                     resultVectorItems = V.fromList (items t),
                     resultVectorSubscript = V.fromList (subscript t) }
    xs' = A.elems xs
    ys' = A.elems ys
    subscript t = ys'
    items t =
      flip map (zip ys' xs') $ \(y, x) ->
      let name' = name ++ y
      in resultOutput (resultSource name' x) t

instance ResultProvider p => ResultProvider (ResultVectorWithSubscript p) where

  resultSource name (ResultVectorWithSubscript xs ys) =
    resultSource name $ ResultListWithSubscript (V.toList xs) (V.toList ys)

instance (Ix i, Show i, ResultComputation m) => ResultProvider (m (A.Array i Double)) where

  resultSource name m = ResultSource f where
    f DoubleResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleListResultType =
      makeResultItemOutput name m (DoubleListResultData . fmap A.elems)
    f DoubleStatsResultType =
      makeResultItemOutput name m (DoubleStatsResultData . fmap (listSamplingStats . A.elems))
    f IntResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntListResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntStatsResultType =
      makeResultItemOutput name m (const NoResultData)
    f StringResultType =
      makeResultItemOutput name m (StringResultData . fmap show)
    f DefaultResultType =
      makeResultItemOutput name m (DoubleListResultData . fmap A.elems)

instance (Ix i, Show i, ResultComputation m) => ResultProvider (m (A.Array i Int)) where

  resultSource name m = ResultSource f where
    f DoubleResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleListResultType =
      makeResultItemOutput name m (DoubleListResultData . fmap (map fromIntegral . A.elems))
    f DoubleStatsResultType =
      makeResultItemOutput name m (DoubleStatsResultData . fmap (fromIntSamplingStats . listSamplingStats . A.elems))
    f IntResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntListResultType =
      makeResultItemOutput name m (IntListResultData . fmap A.elems)
    f IntStatsResultType =
      makeResultItemOutput name m (IntStatsResultData . fmap (listSamplingStats . A.elems))
    f StringResultType =
      makeResultItemOutput name m (StringResultData . fmap show)
    f DefaultResultType =
      makeResultItemOutput name m (IntListResultData . fmap A.elems)

instance ResultComputation m => ResultProvider (m (V.Vector Double)) where

  resultSource name m = ResultSource f where
    f DoubleResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleListResultType =
      makeResultItemOutput name m (DoubleListResultData . fmap V.toList)
    f DoubleStatsResultType =
      makeResultItemOutput name m (DoubleStatsResultData . fmap (listSamplingStats . V.toList))
    f IntResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntListResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntStatsResultType =
      makeResultItemOutput name m (const NoResultData)
    f StringResultType =
      makeResultItemOutput name m (StringResultData . fmap show)
    f DefaultResultType =
      makeResultItemOutput name m (DoubleListResultData . fmap V.toList)

instance ResultComputation m => ResultProvider (m (V.Vector Int)) where

  resultSource name m = ResultSource f where
    f DoubleResultType =
      makeResultItemOutput name m (const NoResultData)
    f DoubleListResultType =
      makeResultItemOutput name m (DoubleListResultData . fmap (map fromIntegral . V.toList))
    f DoubleStatsResultType =
      makeResultItemOutput name m (DoubleStatsResultData . fmap (fromIntSamplingStats . listSamplingStats . V.toList))
    f IntResultType =
      makeResultItemOutput name m (const NoResultData)
    f IntListResultType =
      makeResultItemOutput name m (IntListResultData . fmap V.toList)
    f IntStatsResultType =
      makeResultItemOutput name m (IntStatsResultData . fmap (listSamplingStats . V.toList))
    f StringResultType =
      makeResultItemOutput name m (StringResultData . fmap show)
    f DefaultResultType =
      makeResultItemOutput name m (IntListResultData . fmap V.toList)

-- | This is a function that shows the result output within
-- the 'Event' computation synchronized with the event queue.
type ResultOutputShowS = ResultOutput -> Event ShowS

-- | This is a function that prints the result output within
-- the 'Event' computation synchronized with the event queue.
type ResultOutputPrint = ResultOutput -> Event ()

-- | Print a localised text representation of the specified output with the given indent.
hPrintResultOutputIndented :: Handle
                              -- ^ a handle
                              -> Int
                              -- ^ an indent
                              -> ResultLocalisation
                              -- ^ a localisation
                              -> ResultOutputPrint
hPrintResultOutputIndented h indent loc output@(ResultItemOutput x) =
  hPrintResultOutputIndentedLabelled h indent (resultItemName x) loc output
hPrintResultOutputIndented h indent loc output@(ResultVectorOutput x) =
  hPrintResultOutputIndentedLabelled h indent (resultVectorName x) loc output
hPrintResultOutputIndented h indent loc output@(ResultObjectOutput x) =
  hPrintResultOutputIndentedLabelled h indent (resultObjectName x) loc output
hPrintResultOutputIndented h indent loc output@(ResultSeparatorOutput x) =
  hPrintResultOutputIndentedLabelled h indent (resultSeparatorText x) loc output

-- | Print an indented and labelled text representation of the specified output.
hPrintResultOutputIndentedLabelled :: Handle
                                      -- ^ a handle
                                      -> Int
                                      -- ^ an indent
                                      -> String
                                      -- ^ a label
                                      -> ResultLocalisation
                                      -- ^ a localisation
                                      -> ResultOutputPrint
hPrintResultOutputIndentedLabelled h indent label loc (ResultItemOutput x) =
  case resultItemData x of
    StringResultData m ->
      do a <- m
         let tab = replicate indent ' '
         liftIO $
           do hPutStr h tab
              hPutStr h label
              hPutStr h " = "
              hPutStrLn h a
    _ ->
      error $
      "Expected to see a string value for variable " ++
      (resultItemName x) ++ ": hPrintResultOutputIndentedLabelled"
hPrintResultOutputIndentedLabelled h indent label loc (ResultVectorOutput x) =
  do let tab = replicate indent ' '
     liftIO $
       do hPutStr h tab
          hPutStr h label
          hPutStrLn h ":"
          hPutStrLn h ""
     let items = V.toList (resultVectorItems x)
         subscript = V.toList (resultVectorSubscript x)
     forM_ (zip items subscript) $ \(i, s) ->
       hPrintResultOutputIndentedLabelled h (indent + 2) (label ++ s) loc i
     liftIO $
       hPutStrLn h ""
hPrintResultOutputIndentedLabelled h indent label loc (ResultObjectOutput x) =
  do let tab = replicate indent ' '
     liftIO $
       do hPutStr h tab
          hPutStr h "-- "
          hPutStr h (loc $ resultObjectId x)
          hPutStrLn h ""
          hPutStr h tab
          hPutStr h label
          hPutStrLn h ":"
          hPutStrLn h ""
     forM_ (resultObjectProperties x) $ \p ->
       do let indent' = 2 + indent
              tab'    = "  " ++ tab
              label'  = resultPropertyLabel p
              output' = resultPropertyOutput p
          liftIO $
            do hPutStr h tab'
               hPutStr h "-- "
               hPutStr h (loc $ resultPropertyId p)
               hPutStrLn h ""
          hPrintResultOutputIndentedLabelled h indent' label' loc output'
          liftIO $
            hPutStrLn h ""
hPrintResultOutputIndentedLabelled h indent label loc (ResultSeparatorOutput x) =
  do let tab = replicate indent ' '
     liftIO $
       do hPutStr h tab
          hPutStr h label
          hPutStrLn h ""

-- | Print a localised text representation of the specified output with the given indent.
printResultOutputIndented :: Int
                             -- ^ an indent
                             -> ResultLocalisation
                             -- ^ a localisation
                             -> ResultOutputPrint
printResultOutputIndented = hPrintResultOutputIndented stdout

-- | Print a localised text representation of the specified output.
hPrintResultOutput :: Handle
                      -- ^ a handle
                      -> ResultLocalisation
                      -- ^ a localisation
                      -> ResultOutputPrint
hPrintResultOutput h = hPrintResultOutputIndented h 0

-- | Print a localised text representation of the specified output.
printResultOutput :: ResultLocalisation
                     -- ^ a localisation
                     -> ResultOutputPrint
printResultOutput = hPrintResultOutput stdout

-- | Print a text representation of the specified output in Russian.
hPrintResultOutputInRussian :: Handle -> ResultOutputPrint
hPrintResultOutputInRussian h = hPrintResultOutput h russianResultLocalisation

-- | Print a text representation of the specified output in English.
hPrintResultOutputInEnglish :: Handle -> ResultOutputPrint
hPrintResultOutputInEnglish h = hPrintResultOutput h englishResultLocalisation

-- | Print a text representation of the specified output in Russian.
printResultOutputInRussian :: ResultOutputPrint
printResultOutputInRussian = hPrintResultOutputInRussian stdout

-- | Print a text representation of the specified output in English.
printResultOutputInEnglish :: ResultOutputPrint
printResultOutputInEnglish = hPrintResultOutputInEnglish stdout

-- | Show a localised text representation of the specified output with the given indent.
showResultOutputIndented :: Int
                            -- ^ an indent
                            -> ResultLocalisation
                            -- ^ a localisation
                            -> ResultOutputShowS
showResultOutputIndented indent loc output@(ResultItemOutput x) =
  showResultOutputIndentedLabelled indent (resultItemName x) loc output
showResultOutputIndented indent loc output@(ResultVectorOutput x) =
  showResultOutputIndentedLabelled indent (resultVectorName x) loc output
showResultOutputIndented indent loc output@(ResultObjectOutput x) =
  showResultOutputIndentedLabelled indent (resultObjectName x) loc output

-- | Show an indented and labelled text representation of the specified output.
showResultOutputIndentedLabelled :: Int
                                   -- ^ an indent
                                   -> String
                                   -- ^ a label
                                   -> ResultLocalisation
                                   -- ^ a localisation
                                   -> ResultOutputShowS
showResultOutputIndentedLabelled indent label loc (ResultItemOutput x) =
  case resultItemData x of
    StringResultData m ->
      do a <- m
         let tab = replicate indent ' '
         return $
           showString tab .
           showString label .
           showString " = " .
           showString a .
           showString "\n"
    _ ->
      error $
      "Expected to see a string value for variable " ++
      (resultItemName x) ++ ": showResultOutputIndentedLabelled"
showResultOutputIndentedLabelled indent label loc (ResultVectorOutput x) =
  do let tab = replicate indent ' '
         items = V.toList (resultVectorItems x)
         subscript = V.toList (resultVectorSubscript x)
     contents <-
       forM (zip items subscript) $ \(i, s) ->
       showResultOutputIndentedLabelled (indent + 2) (label ++ s) loc i
     let showContents = foldr (.) id contents
     return $
       showString tab .
       showString label .
       showString ":\n\n" .
       showContents .
       showString "\n"
showResultOutputIndentedLabelled indent label loc (ResultObjectOutput x) =
  do let tab = replicate indent ' '
     contents <-
       forM (resultObjectProperties x) $ \p ->
       do let indent' = 2 + indent
              tab'    = "  " ++ tab
              label'  = resultPropertyLabel p
              output' = resultPropertyOutput p
          showProperties <-
            showResultOutputIndentedLabelled indent' label' loc output'
          return $
            showString tab' .
            showString "-- " .
            showString (loc $ resultPropertyId p) .
            showString "\n" .
            showProperties
     let showContents = foldr (.) id contents
     return $
       showString tab .
       showString "-- " .
       showString (loc $ resultObjectId x) .
       showString "\n" .
       showString tab .
       showString label .
       showString ":\n\n" .
       showContents .
       showString "\n"
showResultOutputIndentedLabelled indent label loc (ResultSeparatorOutput x) =
  do let tab = replicate indent ' '
     return $
       showString tab .
       showString label .
       showString "\n"

-- | Show a localised text representation of the specified output.
showResultOutput :: ResultLocalisation
                    -- ^ a localisation
                    -> ResultOutputShowS
showResultOutput = showResultOutputIndented 0

-- | Show a text representation of the specified output in Russian.
showResultOutputInRussian :: ResultOutputShowS
showResultOutputInRussian = showResultOutput russianResultLocalisation

-- | Show a text representation of the specified output in English.
showResultOutputInEnglish :: ResultOutputShowS
showResultOutputInEnglish = showResultOutput englishResultLocalisation

-- | Output the results using the desired data type.
outputResults :: Results
                 -- ^ the simulation results
                 -> ResultType
                 -- ^ the data type in which we are going to receive an output
                 -> [ResultOutput]
outputResults results t = ys where
  xs = M.elems (resultSources results)
  ys = map (flip resultOutput StringResultType) xs

-- | Print the results with the information about the modeling time.
printResultsWithTime :: ResultOutputPrint -> Results -> Event ()
printResultsWithTime print results =
  do let y1 = makeTextOutput "----------"
         y2 = timeOutput
         y3 = makeTextOutput ""
         ys = outputResults results StringResultType
     print y1
     print y2
     print y3
     mapM_ print ys
     print y3

-- | Print the simulation results in start time.
printResultsInStartTime :: ResultOutputPrint -> Results -> Simulation ()
printResultsInStartTime print results =
  runEventInStartTime $ printResultsWithTime print results

-- | Print the simulation results in stop time.
printResultsInStopTime :: ResultOutputPrint -> Results -> Simulation ()
printResultsInStopTime print results =
  runEventInStopTime $ printResultsWithTime print results

-- | Print the simulation results in integration time points.
printResultsInIntegTimes :: ResultOutputPrint -> Results -> Simulation ()
printResultsInIntegTimes print results =
  do let loop (m : ms) = m >> loop ms
         loop [] = return ()
     ms <- runDynamicsInIntegTimes $ runEvent $
           printResultsWithTime print results
     liftIO $ loop ms

-- | Print in Russian the simulation results in start time.
printInitResultsInRussian :: Results -> Simulation ()
printInitResultsInRussian = printResultsInStartTime  printResultOutputInRussian

-- | Print in English the simulation results in start time.
printInitResultsInEnglish :: Results -> Simulation ()
printInitResultsInEnglish = printResultsInStartTime  printResultOutputInEnglish

-- | Print in Russian the simulation results in stop time.
printFinalResultsInRussian :: Results -> Simulation ()
printFinalResultsInRussian = printResultsInStopTime  printResultOutputInRussian

-- | Print in English the simulation results in stop time.
printFinalResultsInEnglish :: Results -> Simulation ()
printFinalResultsInEnglish = printResultsInStopTime  printResultOutputInEnglish

-- | Print in Russian the simulation results in integration time points.
printIntegResultsInRussian :: Results -> Simulation ()
printIntegResultsInRussian = printResultsInIntegTimes  printResultOutputInRussian

-- | Print in English the simulation results in integration time points.
printIntegResultsInEnglish :: Results -> Simulation ()
printIntegResultsInEnglish = printResultsInIntegTimes  printResultOutputInEnglish

-- | The Russian locale.
russianResultLocale :: ResultLocale
russianResultLocale = "ru"

-- | The English locale.
englishResultLocale :: ResultLocale
englishResultLocale = "en"

-- | The Russian localisation of the simulation results.
russianResultLocalisation :: ResultLocalisation
russianResultLocalisation = lookupResultLocalisation russianResultLocale

-- | The English localisation of the simulation results.
englishResultLocalisation :: ResultLocalisation
englishResultLocalisation = lookupResultLocalisation englishResultLocale

-- | Lookup the localisation by the specified locale.
lookupResultLocalisation :: ResultLocale -> ResultLocalisation
lookupResultLocalisation = undefined
