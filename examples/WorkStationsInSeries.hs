
-- Example: Work Stations in Series
--
-- This is a model of two work stations connected in a series and separated by finite queues.
--
-- It is described in different sources [1, 2]. So, this is chapter 7 of [2] and section 5.14 of [1].
--
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
--
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006
--
-- The maintenance facility of a large manufacturer performs two operations. 
-- These operations must be performed in series; operation 2 always follows operation 1. 
-- The units that are maintained are bulky, and space is available for only eight units 
-- including the units being worked on. A proposed design leaves space for two units 
-- between the work stations, and space for four units before work station 1. [..] 
-- Current company policy is to subcontract the maintenance of a unit if it cannot 
-- gain access to the in-house facility.
-- 
-- Historical data indicates that the time interval between requests for maintenance 
-- is exponentially distributed with a mean of 0.4 time units. Service times are also 
-- exponentially distributed with the first station requiring on the average 0.25 time 
-- units and the second station, 0.5 time units. Units are transported automatically 
-- from work station 1 to work station 2 in a negligible amount of time. If the queue of 
-- work station 2 is full, that is, if there are two units awaiting for work station 2, 
-- the first station is blocked and a unit cannot leave the station. A blocked work 
-- station cannot server other units.

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

-- create a work station (server) with the exponential processing time
newWorkStationExponential meanTime =
  newServer $ \a ->
  do holdProcess =<<
       (liftParameter $
        randomExponential meanTime)
     return a

model :: Simulation Results
model = do
  -- it will gather the statistics of the processing time
  arrivalTimer <- newArrivalTimer
  -- define a stream of input events
  let inputStream = randomExponentialStream meanOrderDelay 
  -- create a queue before the first work stations
  queue1 <-
    runEventInStartTime $
    newFCFSQueue queueMaxCount1
  -- create a queue before the second work stations
  queue2 <-
    runEventInStartTime $
    newFCFSQueue queueMaxCount2
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
        -- processorSeq (map serverProcessor workStation1s) >>>
        queueProcessor2 >>>
        processorParallel (map serverProcessor workStation2s) >>>
        -- processorSeq (map serverProcessor workStation2s) >>>
        arrivalTimerProcessor arrivalTimer
  -- start simulating the model
  runProcessInStartTime $
    sinkStream $ runProcessor entireProcessor inputStream
  -- return the simulation results
  return $
    results
    [resultSource
     "queue1" "Queue no. 1"
     queue1,
     --
     resultSource
     "workStation1s" "Work Stations of line no. 1"
     workStation1s,
     --
     resultSource
     "queue2" "Queue no. 2"
     queue2,
     --
     resultSource
     "workStation2s" "Work Stations of line no. 2"
     workStation2s,
     --
     resultSource
     "arrivalTimer" "The arrival timer"
     arrivalTimer]

modelSummary :: Simulation Results
modelSummary =
  fmap resultSummary model

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  -- model specs
  modelSummary specs
