
{-# LANGUAGE RecursiveDo, Arrows #-}

-- This is a model of the workflow with a loop. Also there are two infinite queues.
--
-- It is described in different sources [1, 2]. So, this is chapter 8 of [2].
--
-- [1] { add a foreign source in English }
--
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

import Prelude hiding (id, (.)) 

import Control.Monad
import Control.Monad.Trans
import Control.Arrow
import Control.Category (id, (.))

import Simulation.Aivika
import Simulation.Aivika.Queue.Infinite

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 480.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- the minimum delay for arriving the next TV set
minArrivalDelay = 3.5

-- the maximum delay for arriving the next TV set
maxArrivalDelay = 7.5

-- the minimum test time
minTestTime = 6

-- the maximum test time
maxTestTime = 12

-- the probability of passing the test
testPassingProb = 0.85

-- how many testers are there?
testerWorkplaceCount = 2

-- the minimum time of tuning the TV set 
-- that has not passed the test
minTuningTime = 20

-- the maximum time of tuning the TV set
-- that has not passed the test
maxTuningTime = 40

-- how many persons perform a tuning of TV sets?
tunerWorkplaceCount = 1

-- create an accumulator to gather the queue size statistics 
newQueueSizeAccumulator queue =
  newTimingStatsAccumulator $
  Signalable (queueCount queue) (queueCountChanged_ queue)

-- create a tester's workplace
newTesterWorkplace =
  newServer $ \a ->
  do holdProcess =<<
       (liftParameter $
        randomUniform minTestTime maxTestTime)
     passed <- 
       liftParameter $
       randomTrue testPassingProb
     if passed
       then return $ Right a
       else return $ Left a 

-- create a tuner's workplace
newTunerWorkplace =
  newServer $ \a ->
  do holdProcess =<<
       (liftParameter $
        randomUniform minTuningTime maxTuningTime)
     return a
  
model :: Simulation ()
model = mdo
  -- it will gather the statistics of the processing time
  arrivalTimer <- newArrivalTimer
  -- define a stream of input events
  let inputStream =
        randomUniformStream minArrivalDelay maxArrivalDelay 
  -- create a queue before the tester's work place
  testerQueue <- newFCFSQueue
  -- create a queue before the tuner's work place
  tunerQueue <- newFCFSQueue
  -- the tester's queue size statistics
  testerQueueSizeAcc <- 
    runEventInStartTime IncludingCurrentEvents $
    newQueueSizeAccumulator testerQueue
  -- the tuner's queue size statistics
  tunerQueueSizeAcc <- 
    runEventInStartTime IncludingCurrentEvents $
    newQueueSizeAccumulator tunerQueue
  -- create the tester's work places, i.e. the "servers"
  testerWorkplaces <-
    forM [1 .. testerWorkplaceCount] $ \_ ->
    newTesterWorkplace
  -- create the tuner's work places, i.e. the "servers"
  tunerWorkplaces <-
    forM [1 .. tunerWorkplaceCount] $ \_ ->
    newTunerWorkplace
  -- a processor loop for the tester's queue
  let testerQueueProcessorLoop =
        queueProcessorLoopSeq
        (liftEvent . enqueue testerQueue)
        (dequeue testerQueue)
        (proc x ->
          do test <- testerProcessor -< x
             case test of
               Left x  ->
                 do y <- (tunerProcessor <<< 
                          tunerQueueProcessor) -< x
                    returnA -< Left y
               Right x -> 
                 returnA -< Right x)
  -- a processor for the tuner's queue
  let tunerQueueProcessor =
        queueProcessor
        (liftEvent . enqueue tunerQueue)
        (dequeue tunerQueue)
  -- the parallel work of all the testers
  let testerProcessor =
        processorParallel (map serverProcessor testerWorkplaces)
  -- the parallel work of all the tuners
  let tunerProcessor =
        processorParallel (map serverProcessor tunerWorkplaces)
  -- the entire processor from input to output
  let entireProcessor =
        testerQueueProcessorLoop >>>
        arrivalTimerProcessor arrivalTimer
  -- start simulating the model
  runProcessInStartTime IncludingCurrentEvents $
    sinkStream $ runProcessor entireProcessor inputStream
  -- show the results in the final time
  runEventInStopTime IncludingCurrentEvents $
    do testerQueueSum <- queueSummary testerQueue 2
       tunerQueueSum  <- queueSummary tunerQueue 2
       testerWorkplaceSums <- 
         forM testerWorkplaces $ \x -> serverSummary x 2
       tunerWorkplaceSums <- 
         forM tunerWorkplaces  $ \x -> serverSummary x 2
       processingTime  <- arrivalProcessingTime arrivalTimer
       testerQueueSize <- timingStatsAccumulated testerQueueSizeAcc
       tunerQueueSize  <- timingStatsAccumulated tunerQueueSizeAcc
       liftIO $
         do putStrLn ""
            putStrLn "--- the tester's queue summary (in the final time) ---"
            putStrLn ""
            putStrLn $ testerQueueSum []
            putStrLn ""
            forM_ (zip [1..] testerWorkplaceSums) $ \(i, x) ->
              do putStrLn $ "--- the tester's work place no."
                   ++ show i ++ " (in the final time) ---"
                 putStrLn ""
                 putStrLn $ x []
                 putStrLn ""
            putStrLn "--- the tuner's queue summary (in the final time) ---"
            putStrLn ""
            putStrLn $ tunerQueueSum []
            putStrLn ""
            forM_ (zip [1..] tunerWorkplaceSums) $ \(i, x) ->
              do putStrLn $ "--- the tuner's work place no. "
                   ++ show i ++ " (in the final time) ---"
                 putStrLn ""
                 putStrLn $ x []
                 putStrLn ""
            putStrLn "--- the processing time summary ---"
            putStrLn ""
            putStrLn $ samplingStatsSummary processingTime 2 []
            putStrLn ""
            putStrLn "--- the tester's queue size summary ---"
            putStrLn ""
            putStrLn $ timingStatsSummary testerQueueSize 2 []
            putStrLn ""
            putStrLn "--- the tuner's queue size summary ---"
            putStrLn ""
            putStrLn $ timingStatsSummary tunerQueueSize 2 []
            putStrLn ""

main = runSimulation model specs
