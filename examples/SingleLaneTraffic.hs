
-- Example: Single-Lane Traffic Analysis 
--
-- It is described in different sources [1, 2]. So, this is chapter 15 of [2] and section 6.18 of [1].
--
-- The system to be modeled in this example consists of the traffic flow from
-- two directions along a two-lane road, one lane of which has been closed for
-- 500 meters for repairs. Traffic lights have been placed at each end of
-- the closed lane to control the flow of traffic through the repair section.
-- The lights allow traffic to flow for a specified time interval from only
-- one direction. When a light turns green, the waiting cars start and pass
-- the light every two seconds. If a car arrives at a green light when there
-- are no waiting cars, the car passes through the light without delay. The car
-- arrival pattern is exponentially distributed, with an average of 9 seconds
-- between cars from direction 1 and 12 seconds between cars from direction 2.
-- A light cycle consists of green in direction 1, both red, green in direction 2,
-- both red, and then the cycle is repeated. Both lights remain red for 55 seconds
-- to allow the cars in transit to leave the repair section before traffic from
-- the other direction can be initiated.
-- 
-- The objective is to simulate the above system to determine values for
-- the green time for direction 1 and the green time for direction 2 which
-- yield a low average waiting time for all cars.
-- 
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import Data.Array

import Simulation.Aivika
import qualified Simulation.Aivika.Resource as R

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 3600.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

data LightTime =
  LightTime { greenLightTime1 :: Double,
              greenLightTime2 :: Double }

model :: LightTime -> Simulation Results
model lightTime = do
  let greenLightTime =
        array (1, 2)
        [(1, return $ greenLightTime1 lightTime :: Event Double),
         (2, return $ greenLightTime2 lightTime :: Event Double)]
  waitTime1 <- newRef emptySamplingStats
  waitTime2 <- newRef emptySamplingStats
  let waitTime =
        array (1, 2) [(1, waitTime1), (2, waitTime2)]
  start1 <-
    runEventInStartTime $
    R.newFCFSResource 1
  start2 <-
    runEventInStartTime $
    R.newFCFSResource 1
  let start =
        array (1, 2) [(1, start1), (2, start2)]
  light1 <- newGateClosed
  light2 <- newGateClosed
  let stream1 = randomExponentialStream 9
      stream2 = randomExponentialStream 12
  runProcessInStartTime $
    flip consumeStream stream1 $ \x ->
    liftEvent $
    runProcess $
    do R.requestResource start1
       awaitGateOpened light1
       t <- liftDynamics time
       liftEvent $
         modifyRef waitTime1 $
         addSamplingStats (t - arrivalTime x)
       when (t > arrivalTime x) $
         holdProcess 2
       R.releaseResource start1
  runProcessInStartTime $
    flip consumeStream stream2 $ \x ->
    liftEvent $
    runProcess $
    do R.requestResource start2
       awaitGateOpened light2
       t <- liftDynamics time
       liftEvent $
         modifyRef waitTime2 $
         addSamplingStats (t - arrivalTime x)
       when (t > arrivalTime x) $
         holdProcess 2
       R.releaseResource start2
  let lighting =
        do holdProcess 55
           liftEvent $
             openGate light1
           holdProcess $
             greenLightTime1 lightTime
           liftEvent $
             closeGate light1
           holdProcess 55
           liftEvent $
             openGate light2
           holdProcess $
             greenLightTime2 lightTime
           liftEvent $
             closeGate light2
           lighting
  runProcessInStartTime lighting
  return $
    results
    [resultSource
     "start" "Start Resource"
     start,
     --
     resultSource
     "waitTime" "Wait Time"
     waitTime,
     --
     resultSource
     "greenLightTime" "Green Light Time"
     greenLightTime]

modelSummary :: LightTime -> Simulation Results
modelSummary lightTime =
  fmap resultSummary $ model lightTime

lightTime1 = LightTime 60 45
lightTime2 = LightTime 80 60
lightTime3 = LightTime 40 30

model1 = model lightTime1
model2 = model lightTime2
model3 = model lightTime3

modelSummary1 = fmap resultSummary model1
modelSummary2 = fmap resultSummary model2
modelSummary3 = fmap resultSummary model3

main =
  do printSimulationResultsInStopTime
       printResultSourceInEnglish
       modelSummary3 specs
