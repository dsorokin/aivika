
-- |
-- Module     : Simulation.Aivika.Results.IO
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module allows printing and converting the 'Simulation' 'Results' to a 'String'.
--
module Simulation.Aivika.Results.IO where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import qualified Data.Vector as V

import System.IO

import Simulation.Aivika.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Results
import Simulation.Aivika.Results.Locale

-- | This is a function that shows the simulation results within
-- the 'Event' computation synchronized with the event queue.
type ResultSourceShowS = ResultSource -> Event ShowS

-- | This is a function that prints the simulation results within
-- the 'Event' computation synchronized with the event queue.
type ResultSourcePrint = ResultSource -> Event ()

-- | Print a localised text representation of the results by the specified source
-- and with the given indent.
hPrintResultSourceIndented :: Handle
                              -- ^ a handle
                              -> Int
                              -- ^ an indent
                              -> ResultLocalisation
                              -- ^ a localisation
                              -> ResultSourcePrint
hPrintResultSourceIndented h indent loc source@(ResultItemSource x) =
  hPrintResultSourceIndentedLabelled h indent (resultItemName x) loc source
hPrintResultSourceIndented h indent loc source@(ResultVectorSource x) =
  hPrintResultSourceIndentedLabelled h indent (resultVectorName x) loc source
hPrintResultSourceIndented h indent loc source@(ResultObjectSource x) =
  hPrintResultSourceIndentedLabelled h indent (resultObjectName x) loc source
hPrintResultSourceIndented h indent loc source@(ResultSeparatorSource x) =
  hPrintResultSourceIndentedLabelled h indent (resultSeparatorText x) loc source

-- | Print an indented and labelled text representation of the results by
-- the specified source.
hPrintResultSourceIndentedLabelled :: Handle
                                      -- ^ a handle
                                      -> Int
                                      -- ^ an indent
                                      -> ResultName
                                      -- ^ a label
                                      -> ResultLocalisation
                                      -- ^ a localisation
                                      -> ResultSourcePrint
hPrintResultSourceIndentedLabelled h indent label loc (ResultItemSource x) =
  case resultItemData x of
    StringResultData m ->
      do a <- m
         let tab = replicate indent ' '
         liftIO $
           do hPutStr h tab
              hPutStr h "-- "
              hPutStr h (loc $ resultItemId x)
              hPutStrLn h ""
              hPutStr h tab
              hPutStr h label
              hPutStr h " = "
              hPutStrLn h a
              hPutStrLn h ""
    _ ->
      error $
      "Expected to see a string value for variable " ++
      (resultItemName x) ++ ": hPrintResultSourceIndentedLabelled"
hPrintResultSourceIndentedLabelled h indent label loc (ResultVectorSource x) =
  do let tab = replicate indent ' '
     liftIO $
       do hPutStr h tab
          hPutStr h "-- "
          hPutStr h (loc $ resultVectorId x)
          hPutStrLn h ""
          hPutStr h tab
          hPutStr h label
          hPutStrLn h ":"
          hPutStrLn h ""
     let items = V.toList (resultVectorItems x)
         subscript = V.toList (resultVectorSubscript x)
     forM_ (zip items subscript) $ \(i, s) ->
       hPrintResultSourceIndentedLabelled h (indent + 2) (label ++ s) loc i
hPrintResultSourceIndentedLabelled h indent label loc (ResultObjectSource x) =
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
              source' = resultPropertySource p
          hPrintResultSourceIndentedLabelled h indent' label' loc source'
hPrintResultSourceIndentedLabelled h indent label loc (ResultSeparatorSource x) =
  do let tab = replicate indent ' '
     liftIO $
       do hPutStr h tab
          hPutStr h label
          hPutStrLn h ""
          hPutStrLn h ""

-- | Print a localised text representation of the results by the specified source
-- and with the given indent.
printResultSourceIndented :: Int
                             -- ^ an indent
                             -> ResultLocalisation
                             -- ^ a localisation
                             -> ResultSourcePrint
printResultSourceIndented = hPrintResultSourceIndented stdout

-- | Print a localised text representation of the results by the specified source.
hPrintResultSource :: Handle
                      -- ^ a handle
                      -> ResultLocalisation
                      -- ^ a localisation
                      -> ResultSourcePrint
hPrintResultSource h = hPrintResultSourceIndented h 0

-- | Print a localised text representation of the results by the specified source.
printResultSource :: ResultLocalisation
                     -- ^ a localisation
                     -> ResultSourcePrint
printResultSource = hPrintResultSource stdout

-- | Print in Russian a text representation of the results by the specified source.
hPrintResultSourceInRussian :: Handle -> ResultSourcePrint
hPrintResultSourceInRussian h = hPrintResultSource h russianResultLocalisation

-- | Print in English a text representation of the results by the specified source.
hPrintResultSourceInEnglish :: Handle -> ResultSourcePrint
hPrintResultSourceInEnglish h = hPrintResultSource h englishResultLocalisation

-- | Print in Russian a text representation of the results by the specified source.
printResultSourceInRussian :: ResultSourcePrint
printResultSourceInRussian = hPrintResultSourceInRussian stdout

-- | Print in English a text representation of the results by the specified source.
printResultSourceInEnglish :: ResultSourcePrint
printResultSourceInEnglish = hPrintResultSourceInEnglish stdout

-- | Show a localised text representation of the results by the specified source
-- and with the given indent.
showResultSourceIndented :: Int
                            -- ^ an indent
                            -> ResultLocalisation
                            -- ^ a localisation
                            -> ResultSourceShowS
showResultSourceIndented indent loc source@(ResultItemSource x) =
  showResultSourceIndentedLabelled indent (resultItemName x) loc source
showResultSourceIndented indent loc source@(ResultVectorSource x) =
  showResultSourceIndentedLabelled indent (resultVectorName x) loc source
showResultSourceIndented indent loc source@(ResultObjectSource x) =
  showResultSourceIndentedLabelled indent (resultObjectName x) loc source

-- | Show an indented and labelled text representation of the results by the specified source.
showResultSourceIndentedLabelled :: Int
                                   -- ^ an indent
                                   -> String
                                   -- ^ a label
                                   -> ResultLocalisation
                                   -- ^ a localisation
                                   -> ResultSourceShowS
showResultSourceIndentedLabelled indent label loc (ResultItemSource x) =
  case resultItemData x of
    StringResultData m ->
      do a <- m
         let tab = replicate indent ' '
         return $
           showString tab .
           showString "-- " .
           showString (loc $ resultItemId x) .
           showString "\n" .
           showString tab .
           showString label .
           showString " = " .
           showString a .
           showString "\n\n"
    _ ->
      error $
      "Expected to see a string value for variable " ++
      (resultItemName x) ++ ": showResultSourceIndentedLabelled"
showResultSourceIndentedLabelled indent label loc (ResultVectorSource x) =
  do let tab = replicate indent ' '
         items = V.toList (resultVectorItems x)
         subscript = V.toList (resultVectorSubscript x)
     contents <-
       forM (zip items subscript) $ \(i, s) ->
       showResultSourceIndentedLabelled (indent + 2) (label ++ s) loc i
     let showContents = foldr (.) id contents
     return $
       showString tab .
       showString "-- " .
       showString (loc $ resultVectorId x) .
       showString "\n" .
       showString tab .
       showString label .
       showString ":\n\n" .
       showContents
showResultSourceIndentedLabelled indent label loc (ResultObjectSource x) =
  do let tab = replicate indent ' '
     contents <-
       forM (resultObjectProperties x) $ \p ->
       do let indent' = 2 + indent
              tab'    = "  " ++ tab
              label'  = resultPropertyLabel p
              output' = resultPropertySource p
          showResultSourceIndentedLabelled indent' label' loc output'
     let showContents = foldr (.) id contents
     return $
       showString tab .
       showString "-- " .
       showString (loc $ resultObjectId x) .
       showString "\n" .
       showString tab .
       showString label .
       showString ":\n\n" .
       showContents
showResultSourceIndentedLabelled indent label loc (ResultSeparatorSource x) =
  do let tab = replicate indent ' '
     return $
       showString tab .
       showString label .
       showString "\n\n"

-- | Show a localised text representation of the results by the specified source.
showResultSource :: ResultLocalisation
                    -- ^ a localisation
                    -> ResultSourceShowS
showResultSource = showResultSourceIndented 0

-- | Show in Russian a text representation of the results by the specified source.
showResultSourceInRussian :: ResultSourceShowS
showResultSourceInRussian = showResultSource russianResultLocalisation

-- | Show in English a text representation of the results by the specified source.
showResultSourceInEnglish :: ResultSourceShowS
showResultSourceInEnglish = showResultSource englishResultLocalisation

-- | Print the results with the information about the modeling time.
printResultsWithTime :: ResultSourcePrint -> Results -> Event ()
printResultsWithTime print results =
  do let x1 = makeTextSource "----------"
         x2 = timeSource
         x3 = makeTextSource ""
         xs = results
         tr = mapResultItems (retypeResultItem StringResultType)
         y1 = tr x1
         y2 = tr x2
         y3 = tr x3
         ys = resultSourceList (retypeResults StringResultType xs)
     print y1
     print y2
     -- print y3
     mapM_ print ys
     -- print y3

-- | Print the simulation results in start time.
printResultsInStartTime :: ResultSourcePrint -> Results -> Simulation ()
printResultsInStartTime print results =
  runEventInStartTime $ printResultsWithTime print results

-- | Print the simulation results in stop time.
printResultsInStopTime :: ResultSourcePrint -> Results -> Simulation ()
printResultsInStopTime print results =
  runEventInStopTime $ printResultsWithTime print results

-- | Print the simulation results in integration time points.
printResultsInIntegTimes :: ResultSourcePrint -> Results -> Simulation ()
printResultsInIntegTimes print results =
  do let loop (m : ms) = m >> loop ms
         loop [] = return ()
     ms <- runDynamicsInIntegTimes $ runEvent $
           printResultsWithTime print results
     liftIO $ loop ms

-- | Print in Russian the simulation results in start time.
printInitResultsInRussian :: Results -> Simulation ()
printInitResultsInRussian = printResultsInStartTime  printResultSourceInRussian

-- | Print in English the simulation results in start time.
printInitResultsInEnglish :: Results -> Simulation ()
printInitResultsInEnglish = printResultsInStartTime  printResultSourceInEnglish

-- | Print in Russian the simulation results in stop time.
printFinalResultsInRussian :: Results -> Simulation ()
printFinalResultsInRussian = printResultsInStopTime  printResultSourceInRussian

-- | Print in English the simulation results in stop time.
printFinalResultsInEnglish :: Results -> Simulation ()
printFinalResultsInEnglish = printResultsInStopTime  printResultSourceInEnglish

-- | Print in Russian the simulation results in integration time points.
printIntegResultsInRussian :: Results -> Simulation ()
printIntegResultsInRussian = printResultsInIntegTimes  printResultSourceInRussian

-- | Print in English the simulation results in integration time points.
printIntegResultsInEnglish :: Results -> Simulation ()
printIntegResultsInEnglish = printResultsInIntegTimes  printResultSourceInEnglish

-- | Run the simulation and output to the results in the start time.
outputResultsInStartTime :: ResultSourcePrint -> Simulation Results -> Specs -> IO ()
outputResultsInStartTime print model specs =
  flip runSimulation specs $
  model >>= printResultsInStartTime print

-- | Run the simulation and output to the results in the final time.
outputResultsInStopTime :: ResultSourcePrint -> Simulation Results -> Specs -> IO ()
outputResultsInStopTime print model specs =
  flip runSimulation specs $
  model >>= printResultsInStopTime print

-- | Run the simulation and output to the results in the integration time points.
outputResultsInIntegTimes :: ResultSourcePrint -> Simulation Results -> Specs -> IO ()
outputResultsInIntegTimes print model specs =
  flip runSimulation specs $
  model >>= printResultsInIntegTimes print

-- | Run the simulation and output in Russian the results in the start time.
outputInitResultsInRussian :: Simulation Results -> Specs -> IO ()
outputInitResultsInRussian = outputResultsInStartTime printResultSourceInRussian

-- | Run the simulation and output in English the results in the start time.
outputInitResultsInEnglish :: Simulation Results -> Specs -> IO ()
outputInitResultsInEnglish = outputResultsInStartTime printResultSourceInEnglish

-- | Run the simulation and output in Russian the results in the final time.
outputFinalResultsInRussian :: Simulation Results -> Specs -> IO ()
outputFinalResultsInRussian = outputResultsInStopTime printResultSourceInRussian

-- | Run the simulation and output in English the results in the final time.
outputFinalResultsInEnglish :: Simulation Results -> Specs -> IO ()
outputFinalResultsInEnglish = outputResultsInStopTime printResultSourceInEnglish

-- | Run the simulation and output in Russian the results in the integration time points.
outputIntegResultsInRussian :: Simulation Results -> Specs -> IO ()
outputIntegResultsInRussian = outputResultsInIntegTimes printResultSourceInRussian

-- | Run the simulation and output in English the results in the integration time points.
outputIntegResultsInEnglish :: Simulation Results -> Specs -> IO ()
outputIntegResultsInEnglish = outputResultsInIntegTimes printResultSourceInEnglish
