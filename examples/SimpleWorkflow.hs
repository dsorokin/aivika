
-- This is a model of the workflow with 2 work places. It is described in different sources [1, 2].
--
-- [1] { add a foreign source in English }
--
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006
--
-- There are two work places with finite queues in one workflow.

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

-- the mean delay of input orders
meanOrderDelay = 0.4 

-- the capacity of the queue before the first work places
queueMaxCount1 = 4

-- the capacity of the queue before the second work places
queueMaxCount2 = 2

-- the mean time of processing an order in the first work places
meanProcessingTime1 = 0.25

-- the mean time of processing an order in the second work places
meanProcessingTime2 = 0.5

-- the number of the first work places
workplaceCount1 = 1

-- the number of the second work places
workplaceCount2 = 1

-- create an accumulator to gather the queue size statistics 
newQueueSizeAccumulator queue =
  newTimingStatsAccumulator $
  Signalable (queueCount queue) (queueCountChanged_ queue)

-- create a workflow with the exponential processing time
newWorkplaceExponential meanTime =
  newServer $ \a ->
  do holdProcess =<<
       (liftParameter $
        randomExponential meanTime)
     return a
  
model :: Simulation ()
model = do
  -- the statistics of the processing time
  processingTimeStats <- newRef emptySamplingStats
  -- define a stream of input orders
  let inputStream =
        mapStreamM (const $ liftDynamics time) $
        randomExponentialStream meanOrderDelay 
  -- create a queue before the first work place
  queue1 <- newFCFSQueue queueMaxCount1
  -- create a queue before the second work place
  queue2 <- newFCFSQueue queueMaxCount2
  -- the first queue size statistics
  queueSizeAcc1 <- 
    runEventInStartTime IncludingCurrentEvents $
    newQueueSizeAccumulator queue1
  -- the second queue size statistics
  queueSizeAcc2 <- 
    runEventInStartTime IncludingCurrentEvents $
    newQueueSizeAccumulator queue2
  -- create the first work places, i.e. the "servers"
  workplace1s <- forM [1 .. workplaceCount1] $ \_ ->
    newWorkplaceExponential meanProcessingTime1
  -- create the second work places, i.e. the "servers"
  workplace2s <- forM [1 .. workplaceCount2] $ \_ ->
    newWorkplaceExponential meanProcessingTime2
  -- processor for the queue before the first work place
  let queueProcessor1 =
        queueProcessor
        (\a -> liftEvent $ enqueueOrLost_ queue1 a)
        (dequeue queue1)
  -- processor for the queue before the second work place
  let queueProcessor2 =
        queueProcessor
        (enqueue queue2)
        (dequeue queue2)
  -- the entire processor from input to output
  let entireProcessor =
        queueProcessor1 >>>
        processorParallel (map serverProcessor workplace1s) >>>
        -- foldr (>>>) id (map serverProcessor workplace1s) >>>
        queueProcessor2 >>>
        processorParallel (map serverProcessor workplace2s)
        -- foldr (>>>) id (map serverProcessor workplace2s)
  -- start simulating the model
  runProcessInStartTime IncludingCurrentEvents $
    consumeStream
    (\a -> do
        t <- liftDynamics time
        liftEvent $
          modifyRef processingTimeStats $
          addSamplingStats (t - a))
    (runProcessor entireProcessor inputStream)
  -- show the results in the final time
  runEventInStopTime IncludingCurrentEvents $
    do queueSum1 <- queueSummary queue1 2
       queueSum2 <- queueSummary queue2 2
       workplaceSum1s <- forM workplace1s $ \x -> serverSummary x 2
       workplaceSum2s <- forM workplace2s $ \x -> serverSummary x 2
       timeStats <- readRef processingTimeStats
       queueSize1 <- timingStatsAccumulated queueSizeAcc1
       queueSize2 <- timingStatsAccumulated queueSizeAcc2
       liftIO $
         do putStrLn ""
            putStrLn "--- the first queue summary (in the final time) ---"
            putStrLn ""
            putStrLn $ queueSum1 []
            putStrLn ""
            forM_ (zip [1..] workplaceSum1s) $ \(i, x) ->
              do putStrLn $ "--- the first work place no." ++ show i ++ " (in the final time) ---"
                 putStrLn ""
                 putStrLn $ x []
                 putStrLn ""
            putStrLn "--- the second queue summary (in the final time) ---"
            putStrLn ""
            putStrLn $ queueSum2 []
            putStrLn ""
            forM_ (zip [1..] workplaceSum2s) $ \(i, x) ->
              do putStrLn $ "--- the second work place no. " ++ show i ++ " (in the final time) ---"
                 putStrLn ""
                 putStrLn $ x []
                 putStrLn ""
            putStrLn "--- the processing time summary ---"
            putStrLn ""
            putStrLn $ samplingStatsSummary timeStats 2 []
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
