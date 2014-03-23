
-- Example: Work Stations in Series
--
-- This is a model of two work stations connected in a series and separated by finite queues.
--
-- It is described in different sources [1, 2]. So, this is chapter 7 of [2] and section 5.14 of [1].
--
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
--
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

import Prelude hiding (id, (.)) 

import Control.Monad
import Control.Monad.Trans
import Control.Arrow
import Control.Category (id, (.))

import Simulation.Aivika
import Simulation.Aivika.Queue

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 300.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- the mean delay of the input arrivals distributed exponentially
meanOrderDelay = 0.4 

-- the capacity of the queue before the first work places
queueMaxCount1 = 4

-- the capacity of the queue before the second work places
queueMaxCount2 = 2

-- the mean processing time distributed exponentially in
-- the first work stations
meanProcessingTime1 = 0.25

-- the mean processing time distributed exponentially in
-- the second work stations
meanProcessingTime2 = 0.5

-- the number of the first work stations
-- (in parallel but the commented code allocates them sequentially)
workStationCount1 = 1

-- the number of the second work stations
-- (in parallel but the commented code allocates them sequentially)
workStationCount2 = 1

-- create an accumulator to gather the queue size statistics 
newQueueSizeAccumulator queue =
  newTimingStatsAccumulator $
  Signalable (queueCount queue) (queueCountChanged_ queue)

-- create a work station (server) with the exponential processing time
newWorkStationExponential meanTime =
  newServer $ \a ->
  do holdProcess =<<
       (liftParameter $
        randomExponential meanTime)
     return a

-- interpose the prefetch processor between two processors
interposePrefetchProcessor x y = 
  x >>> prefetchProcessor >>> y

model :: Simulation ()
model = do
  -- it will gather the statistics of the processing time
  arrivalTimer <- newArrivalTimer
  -- define a stream of input events
  let inputStream = randomExponentialStream meanOrderDelay 
  -- create a queue before the first work stations
  queue1 <- newFCFSQueue queueMaxCount1
  -- create a queue before the second work stations
  queue2 <- newFCFSQueue queueMaxCount2
  -- the first queue size statistics
  queueSizeAcc1 <- 
    runEventInStartTime $
    newQueueSizeAccumulator queue1
  -- the second queue size statistics
  queueSizeAcc2 <- 
    runEventInStartTime $
    newQueueSizeAccumulator queue2
  -- create the first work stations (servers)
  workStation1s <- forM [1 .. workStationCount1] $ \_ ->
    newWorkStationExponential meanProcessingTime1
  -- create the second work stations (servers)
  workStation2s <- forM [1 .. workStationCount2] $ \_ ->
    newWorkStationExponential meanProcessingTime2
  -- processor for the queue before the first work station
  let queueProcessor1 =
        queueProcessor
        (\a -> liftEvent $ enqueueOrLost_ queue1 a)
        (dequeue queue1)
  -- processor for the queue before the second work station
  let queueProcessor2 =
        queueProcessor
        (enqueue queue2)
        (dequeue queue2)
  -- the entire processor from input to output
  let entireProcessor =
        queueProcessor1 >>>
        processorParallel (map serverProcessor workStation1s) >>>
        -- foldr1 interposePrefetchProcessor (map serverProcessor workStation1s) >>>
        queueProcessor2 >>>
        processorParallel (map serverProcessor workStation2s) >>>
        -- foldr1 interposePrefetchProcessor (map serverProcessor workStation2s) >>>
        arrivalTimerProcessor arrivalTimer
  -- start simulating the model
  runProcessInStartTime $
    sinkStream $ runProcessor entireProcessor inputStream
  -- show the results in the final time
  runEventInStopTime $
    do queueSum1 <- queueSummary queue1 2
       queueSum2 <- queueSummary queue2 2
       workStationSum1s <- forM workStation1s $ \x -> serverSummary x 2
       workStationSum2s <- forM workStation2s $ \x -> serverSummary x 2
       processingTime <- arrivalProcessingTime arrivalTimer
       queueSize1 <- timingStatsAccumulated queueSizeAcc1
       queueSize2 <- timingStatsAccumulated queueSizeAcc2
       liftIO $
         do putStrLn ""
            putStrLn "--- the first queue summary (in the final time) ---"
            putStrLn ""
            putStrLn $ queueSum1 []
            putStrLn ""
            forM_ (zip [1..] workStationSum1s) $ \(i, x) ->
              do putStrLn $ "--- the first work station no. " ++ show i ++ " (in the final time) ---"
                 putStrLn ""
                 putStrLn $ x []
                 putStrLn ""
            putStrLn "--- the second queue summary (in the final time) ---"
            putStrLn ""
            putStrLn $ queueSum2 []
            putStrLn ""
            forM_ (zip [1..] workStationSum2s) $ \(i, x) ->
              do putStrLn $ "--- the second work station no. " ++ show i ++ " (in the final time) ---"
                 putStrLn ""
                 putStrLn $ x []
                 putStrLn ""
            putStrLn "--- the processing time summary ---"
            putStrLn ""
            putStrLn $ samplingStatsSummary processingTime 2 []
            putStrLn ""
            putStrLn "--- the first queue size summary ---"
            putStrLn ""
            putStrLn $ timingStatsSummary queueSize1 2 []
            putStrLn ""
            putStrLn "--- the second queue size summary ---"
            putStrLn ""
            putStrLn $ timingStatsSummary queueSize2 2 []
            putStrLn ""

main = runSimulation model specs
