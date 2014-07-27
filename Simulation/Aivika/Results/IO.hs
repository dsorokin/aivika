
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

-- | Run the simulation and output to the results in the start time.
outputResultsInStartTime :: ResultOutputPrint -> Simulation Results -> Specs -> IO ()
outputResultsInStartTime print model specs =
  flip runSimulation specs $
  model >>= printResultsInStartTime print

-- | Run the simulation and output to the results in the final time.
outputResultsInStopTime :: ResultOutputPrint -> Simulation Results -> Specs -> IO ()
outputResultsInStopTime print model specs =
  flip runSimulation specs $
  model >>= printResultsInStopTime print

-- | Run the simulation and output to the results in the integration time points.
outputResultsInIntegTimes :: ResultOutputPrint -> Simulation Results -> Specs -> IO ()
outputResultsInIntegTimes print model specs =
  flip runSimulation specs $
  model >>= printResultsInIntegTimes print

-- | Run the simulation and output in Russian the results in the start time.
outputInitResultsInRussian :: Simulation Results -> Specs -> IO ()
outputInitResultsInRussian = outputResultsInStartTime printResultOutputInRussian

-- | Run the simulation and output in English the results in the start time.
outputInitResultsInEnglish :: Simulation Results -> Specs -> IO ()
outputInitResultsInEnglish = outputResultsInStartTime printResultOutputInEnglish

-- | Run the simulation and output in Russian the results in the final time.
outputFinalResultsInRussian :: Simulation Results -> Specs -> IO ()
outputFinalResultsInRussian = outputResultsInStopTime printResultOutputInRussian

-- | Run the simulation and output in English the results in the final time.
outputFinalResultsInEnglish :: Simulation Results -> Specs -> IO ()
outputFinalResultsInEnglish = outputResultsInStopTime printResultOutputInEnglish

-- | Run the simulation and output in Russian the results in the integration time points.
outputIntegResultsInRussian :: Simulation Results -> Specs -> IO ()
outputIntegResultsInRussian = outputResultsInIntegTimes printResultOutputInRussian

-- | Run the simulation and output in English the results in the integration time points.
outputIntegResultsInEnglish :: Simulation Results -> Specs -> IO ()
outputIntegResultsInEnglish = outputResultsInIntegTimes printResultOutputInEnglish
