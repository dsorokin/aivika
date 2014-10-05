
{-# LANGUAGE RecursiveDo, Arrows #-}

-- Example: Inspection and Adjustment Stations on a Production Line
-- 
-- This is a model of the workflow with a loop. Also there are two infinite queues.
--
-- It is described in different sources [1, 2]. So, this is chapter 8 of [2] and section 5.15 of [1].
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
import Simulation.Aivika.Queue.Infinite

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 480.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- the minimum delay of arriving the next TV set
minArrivalDelay = 3.5

-- the maximum delay of arriving the next TV set
maxArrivalDelay = 7.5

-- the minimum time to inspect the TV set
minInspectionTime = 6

-- the maximum time to inspect the TV set
maxInspectionTime = 12

-- the probability of passing the inspection phase
inspectionPassingProb = 0.85

-- how many are inspection stations?
inspectionStationCount = 2

-- the minimum time to adjust an improper TV set
minAdjustmentTime = 20

-- the maximum time to adjust an improper TV set
maxAdjustmentTime = 40

-- how many are adjustment stations?
adjustmentStationCount = 1

-- create an inspection station (server)
newInspectionStation =
  newServer $ \a ->
  do holdProcess =<<
       (liftParameter $
        randomUniform minInspectionTime maxInspectionTime)
     passed <- 
       liftParameter $
       randomTrue inspectionPassingProb
     if passed
       then return $ Right a
       else return $ Left a 

-- create an adjustment station (server)
newAdjustmentStation =
  newServer $ \a ->
  do holdProcess =<<
       (liftParameter $
        randomUniform minAdjustmentTime maxAdjustmentTime)
     return a
  
model :: Simulation Results
model = mdo
  -- to count the arrived TV sets for inspecting and adjusting
  inputArrivalTimer <- newArrivalTimer
  -- it will gather the statistics of the processing time
  outputArrivalTimer <- newArrivalTimer
  -- define a stream of input events
  let inputStream =
        randomUniformStream minArrivalDelay maxArrivalDelay 
  -- create a queue before the inspection stations
  inspectionQueue <-
    runEventInStartTime newFCFSQueue
  -- create a queue before the adjustment stations
  adjustmentQueue <-
    runEventInStartTime newFCFSQueue
  -- create the inspection stations (servers)
  inspectionStations <-
    forM [1 .. inspectionStationCount] $ \_ ->
    newInspectionStation
  -- create the adjustment stations (servers)
  adjustmentStations <-
    forM [1 .. adjustmentStationCount] $ \_ ->
    newAdjustmentStation
  -- a processor loop for the inspection stations' queue
  let inspectionQueueProcessorLoop =
        queueProcessorLoopSeq
        (liftEvent . enqueue inspectionQueue)
        (dequeue inspectionQueue)
        inspectionProcessor
        (adjustmentQueueProcessor >>> adjustmentProcessor)
  -- a processor for the adjustment stations' queue
  let adjustmentQueueProcessor =
        queueProcessor
        (liftEvent . enqueue adjustmentQueue)
        (dequeue adjustmentQueue)
  -- a parallel work of the inspection stations
  let inspectionProcessor =
        processorParallel (map serverProcessor inspectionStations)
  -- a parallel work of the adjustment stations
  let adjustmentProcessor =
        processorParallel (map serverProcessor adjustmentStations)
  -- the entire processor from input to output
  let entireProcessor =
        arrivalTimerProcessor inputArrivalTimer >>>
        inspectionQueueProcessorLoop >>>
        arrivalTimerProcessor outputArrivalTimer
  -- start simulating the model
  runProcessInStartTime $
    sinkStream $ runProcessor entireProcessor inputStream
  -- return the simulation results in start time
  return $
    results
    [resultSource
     "inspectionQueue" "the inspection queue"
     inspectionQueue,
     --
     resultSource
     "adjustmentQueue" "the adjustment queue"
     adjustmentQueue,
     --
     resultSource
     "inputArrivalTimer" "the input arrival timer"
     inputArrivalTimer,
     --
     resultSource
     "outputArrivalTimer" "the output arrival timer"
     outputArrivalTimer,
     --
     resultSource
     "inspectionStations" "the inspection stations"
     inspectionStations,
     --
     resultSource
     "adjustmentStations" "the adjustment stations"
     adjustmentStations]

modelSummary :: Simulation Results
modelSummary = fmap resultSummary model

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  modelSummary specs
