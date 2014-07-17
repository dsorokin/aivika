
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

-- | Flatten the result items.
flattenResultItems :: ResultOutput -> [ResultItem]
flattenResultItems (ResultItemOutput x) = [x]
flattenResultItems (ResultObjectOutput x) =
  concat $ map (flattenResultItems . resultPropertyOutput) $ resultObjectProperties x
flattenResultItems (ResultVectorOutput x) =
  concat $ map flattenResultItems $ V.toList $ resultVectorItems x

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
                           -> ResultOutput
makeSamplingStatsOutput name m =
  let f g = StringResultData . fmap (show . g)
  in ResultObjectOutput $
     ResultObject {
       resultObjectName = name,
       resultObjectId = SamplingStatsId,
       resultObjectProperties = [
         ResultProperty {
            resultPropertyLabel = "count",
            resultPropertyId = SamplingStatsCountId,
            resultPropertyOutput =
              makeResultItemOutput (name ++ ".count") m (f samplingStatsCount) },
         ResultProperty {
           resultPropertyLabel = "mean",
           resultPropertyId = SamplingStatsMeanId,
           resultPropertyOutput =
             makeResultItemOutput (name ++ ".mean") m (f samplingStatsMean) },
         ResultProperty {
           resultPropertyLabel = "mean2",
           resultPropertyId = SamplingStatsMean2Id,
           resultPropertyOutput =
             makeResultItemOutput (name ++ ".mean2") m (f samplingStatsMean2) },
         ResultProperty {
           resultPropertyLabel = "std",
           resultPropertyId = SamplingStatsDeviationId,
           resultPropertyOutput =
             makeResultItemOutput (name ++ ".std") m (f samplingStatsDeviation) },
         ResultProperty {
           resultPropertyLabel = "var",
           resultPropertyId = SamplingStatsVarianceId,
           resultPropertyOutput =
             makeResultItemOutput (name ++ ".var") m (f samplingStatsVariance) },
         ResultProperty {
           resultPropertyLabel = "min",
           resultPropertyId = SamplingStatsMinId,
           resultPropertyOutput =
             makeResultItemOutput (name ++ ".min") m (f samplingStatsMin) },
         ResultProperty {
           resultPropertyLabel = "max",
           resultPropertyId = SamplingStatsMaxId,
           resultPropertyOutput =
             makeResultItemOutput (name ++ ".max") m (f samplingStatsMax) } ] }

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
      makeSamplingStatsOutput name m
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
      makeSamplingStatsOutput name m
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

-- | Print a localised text representation of the specified output with the given indent.
hPrintResultOutputIndented :: Handle
                              -- ^ a handle
                              -> ResultLocalisation
                              -- ^ a localisation
                              -> ResultOutput
                              -- ^ the output to represent
                              -> Int
                              -- ^ an ident
                              -> Event ()
hPrintResultOutputIndented h loc output@(ResultItemOutput x) =
  hPrintResultOutputLabeledIndented h (resultItemName x) loc output
hPrintResultOutputIndented h loc output@(ResultVectorOutput x) =
  hPrintResultOutputLabeledIndented h (resultVectorName x) loc output
hPrintResultOutputIndented h loc output@(ResultObjectOutput x) =
  hPrintResultOutputLabeledIndented h (resultObjectName x) loc output

-- | Print a labeled and indented text representation of the specified output.
hPrintResultOutputLabeledIndented :: Handle
                                     -- ^ a handle
                                     -> String
                                     -- ^ a label
                                     -> ResultLocalisation
                                     -- ^ a localisation
                                     -> ResultOutput
                                     -- ^ the output to represent
                                     -> Int
                                     -- ^ an ident
                                     -> Event ()
hPrintResultOutputLabeledIndented h label loc (ResultItemOutput x) indent =
  case resultItemData x of
    StringResultData m ->
      do a <- m
         let tab = replicate indent ' '
         liftIO $
           do hPutStr h tab
              hPutStr h label
              hPutStr h " = "
              hPutStrLn h (show a)
    _ ->
      error $
      "Expected to see a string value for variable " ++
      (resultItemName x) ++ ": hPrintLabeledLocalisedResultOutput"
hPrintResultOutputLabeledIndented h label loc (ResultVectorOutput x) indent =
  do let tab = replicate indent ' '
     liftIO $
       do hPutStr h tab
          hPutStr h label
          hPutStrLn h ":"
          hPutStrLn h ""
     let items = V.toList (resultVectorItems x)
         subscript = V.toList (resultVectorSubscript x)
     forM_ (zip items subscript) $ \(i, s) ->
       hPrintResultOutputLabeledIndented h (label ++ s) loc i (indent + 2)
hPrintResultOutputLabeledIndented h label loc (ResultObjectOutput x) indent =
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
          hPrintResultOutputLabeledIndented h label' loc output' indent'

-- | Print a localised text representation of the specified output with the given indent.
printResultOutputIndented :: ResultLocalisation
                             -- ^ a localisation
                             -> ResultOutput
                             -- ^ the output to represent
                             -> Int
                             -- ^ an ident
                             -> Event ()
printResultOutputIndented = hPrintResultOutputIndented stdout

-- | Print a localised text representation of the specified output.
hPrintResultOutput :: Handle
                      -- ^ a handle
                      -> ResultLocalisation
                      -- ^ a localisation
                      -> ResultOutput
                      -- ^ the output to represent
                      -> Event ()
hPrintResultOutput h loc output = hPrintResultOutputIndented h loc output 0

-- | Print a localised text representation of the specified output.
printResultOutput :: ResultLocalisation
                     -- ^ a localisation
                     -> ResultOutput
                     -- ^ the output to represent
                     -> Event ()
printResultOutput = hPrintResultOutput stdout

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
