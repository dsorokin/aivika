
-- This is a model of the workflow with 2 work places. It is described in different sources [1, 2].
--
-- [1] { add a foreign source in English }
--
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006
--
-- There are two work places with finite queues in one workflow.

import Control.Monad
import Control.Monad.Trans
import Control.Arrow

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

-- the capacity of the queue before the first work place
queueMaxCount1 = 4

-- the capacity of the queue before the second work place
queueMaxCount2 = 2

-- the mean time of processing an order in the first work place
meanProcessingTime1 = 0.25

-- the mean time of processing an order in the second work place
meanProcessingTime2 = 0.5

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
  -- statistics of the queue size 
  countStats1 <- newRef emptySamplingStats
  countStats2 <- newRef emptySamplingStats
  runEventInStartTime IncludingCurrentEvents $
    do handleSignal_ (queueCountChanged queue1) $ \a ->
          modifyRef countStats1 $ 
          addSamplingStats a
       handleSignal_ (queueCountChanged queue2) $ \a ->
          modifyRef countStats2 $
          addSamplingStats a
  -- create the first work place, i.e. a "server"
  workplace1 <- newServer $ \a ->
    do holdProcess =<<
         (liftParameter $
          randomExponential meanProcessingTime1)
       return a
  -- create the second work place, i.e. a "server"
  workplace2 <- newServer $ \a ->
    do holdProcess =<<
         (liftParameter $
          randomExponential meanProcessingTime2)
       return a
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
  -- -- the entire processor from input to output
  -- let entireProcessor =
  --       queueProcessor1 >>>
  --       processorParallel [serverProcessor workplace1] >>>
  --       queueProcessor2 >>>
  --       processorParallel [serverProcessor workplace2]
  -- the entire processor from input to output
  let entireProcessor =
        queueProcessor1 >>>
        serverProcessor workplace1 >>>
        queueProcessor2 >>>
        serverProcessor workplace2
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
       workplaceSum1 <- serverSummary workplace1 2
       workplaceSum2 <- serverSummary workplace2 2
       timeStats <- readRef processingTimeStats
       countStats1 <- readRef countStats1
       countStats2 <- readRef countStats2
       liftIO $
         do putStrLn ""
            putStrLn "--- the first queue summary (in the final time) ---"
            putStrLn ""
            putStrLn $ queueSum1 []
            putStrLn ""
            putStrLn "--- the first work place (in the final time) ---"
            putStrLn ""
            putStrLn $ workplaceSum1 []
            putStrLn ""
            putStrLn "--- the second queue summary (in the final time) ---"
            putStrLn ""
            putStrLn $ queueSum2 []
            putStrLn ""
            putStrLn "--- the second work place (in the final time) ---"
            putStrLn ""
            putStrLn $ workplaceSum2 []
            putStrLn ""
            putStrLn "--- the processing time summary ---"
            putStrLn ""
            putStrLn $ samplingStatsSummary timeStats 2 []

            putStrLn ""
            putStrLn "--- the first queue size ---"
            putStrLn ""
            putStrLn $ samplingStatsSummary countStats1 2 []
            putStrLn ""
            putStrLn "--- the second queue size ---"
            putStrLn ""
            putStrLn $ samplingStatsSummary countStats2 2 []
            putStrLn ""

main = runSimulation model specs
