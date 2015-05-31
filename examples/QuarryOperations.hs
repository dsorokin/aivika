
{-# LANGUAGE Arrows #-}

-- Example: In this example, the operations of a quarry are modeled.
--
-- It is described in different sources [1, 2]. So, this is chapter 10 of [2] and section 5.16 of [1].
--
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

import Control.Monad
import Control.Monad.Trans
import Control.Category

import Simulation.Aivika
import qualified Simulation.Aivika.Queue.Infinite as IQ

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 480.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- | The average loading time for twenty-ton truck
avgLoadingTime20 = 5

-- | A constant travel time for twenty-ton truck
travelTime20 = 2.5

-- | The average dumping time for twenty-ton truck
avgDumpingTime20 = 2

-- | A constant return trip time for twenty-ton truck
returnTripTime20 = 1.5

-- | A priority of the twenty-ton truck (less is higher)
crushingPriority20 = 2

-- | The average loading time for fifty-ton truck
avgLoadingTime50 = 10

-- | A constant travel time for fifty-ton truck
travelTime50 = 3

-- | The average dumping time for fifty-ton truck
avgDumpingTime50 = 4

-- | A constant return trip time for fifty-ton truck
returnTripTime50 = 2

-- | A priority of the fifty-ton truck (less is higher)
crushingPriority50 = 1

-- | It models a truck assigned to some queue.
data Truck =
  Truck { truckQueue :: TruckQueue,
          -- ^ a queue to which the truck is assigned
          truckTonSize :: TruckTonSize,
          -- ^ the truck ton size
          truckAvgLoadingTime :: Double,
          -- ^ the average loading time
          truckTravelTime :: Double,
          -- ^ a constant travel time
          truckCrushingPriority :: Double,
          -- ^ a priority for crushing (less is higher)
          truckAvgDumpingTime :: Double,
          -- ^ the average dumping time
          truckReturnTripTime :: Double
          -- ^ a constant return trip time
        }

-- | It defines the truck ton size
data TruckTonSize = TwentyTonSize | FiftyTonSize

-- | Specifies a queue to which the truck is assigned
data TruckQueue = TruckQueue1 | TruckQueue2 | TruckQueue3

-- | Return a truck assigned to the specified queue with the given ton size.
truck :: TruckQueue -> TruckTonSize -> Truck
truck tq TwentyTonSize =
  Truck { truckQueue = tq,
          truckTonSize = TwentyTonSize,
          truckAvgLoadingTime = avgLoadingTime20,
          truckTravelTime = travelTime20,
          truckCrushingPriority = crushingPriority20,
          truckAvgDumpingTime = avgDumpingTime20,
          truckReturnTripTime = returnTripTime20 }
truck tq FiftyTonSize =
  Truck { truckQueue = tq,
          truckTonSize = FiftyTonSize,
          truckAvgLoadingTime = avgLoadingTime50,
          truckTravelTime = travelTime50,
          truckCrushingPriority = crushingPriority50,
          truckAvgDumpingTime = avgDumpingTime50,
          truckReturnTripTime = returnTripTime50 }
  
model :: Simulation Results
model = do
  -- create a queue for the first shovel
  shovelQueue1 <-
    runEventInStartTime IQ.newFCFSQueue
  -- create another queue for the second shovel
  shovelQueue2 <-
    runEventInStartTime IQ.newFCFSQueue
  -- create a queue for the thrid shovel
  shovelQueue3 <-
    runEventInStartTime IQ.newFCFSQueue
  -- add initial trucks to the queue
  let initShovelQueue q tq =
        do IQ.enqueue q $ truck tq TwentyTonSize
           IQ.enqueue q $ truck tq TwentyTonSize
           IQ.enqueue q $ truck tq FiftyTonSize
  -- initiate the three shovel queues
  runEventInStartTime $
    do initShovelQueue shovelQueue1 TruckQueue1
       initShovelQueue shovelQueue2 TruckQueue2
       initShovelQueue shovelQueue3 TruckQueue3
  -- create a priority queue for the crusher
  crusherQueue <-
    runEventInStartTime IQ.newPriorityQueue
  -- define how the specified truck travels from the shovel to the crusher
  let truckTravel t =
        spawnProcess $
        do holdProcess (truckTravelTime t)
           liftEvent $
             IQ.enqueueWithStoringPriority crusherQueue (truckCrushingPriority t) t
  -- define how the specified truck returns to the queue
  let truckReturnTrip t =
        spawnProcess $
        do holdProcess (truckReturnTripTime t)
           let q = case truckQueue t of
                 TruckQueue1 -> shovelQueue1
                 TruckQueue2 -> shovelQueue2
                 TruckQueue3 -> shovelQueue3
           liftEvent $
             IQ.enqueue q t
  -- utilise the crusher's activity
  let utiliseCrusher q t =
        do dumpingTime <-
             liftParameter $
             randomExponential $
             truckAvgDumpingTime t
           holdProcess dumpingTime
           return t
  -- utilise the shovel's activity
  let utiliseShovel q t =
        do loadingTime <-
             liftParameter $
             randomExponential $
             truckAvgLoadingTime t
           holdProcess loadingTime
           return t
  -- create shovel activities
  shovelAct1 <-
    newActivity $ utiliseShovel shovelQueue1
  shovelAct2 <-
    newActivity $ utiliseShovel shovelQueue2
  shovelAct3 <-
    newActivity $ utiliseShovel shovelQueue3
  -- create the crusher's activity
  crusherAct <-
    newActivity $ utiliseCrusher crusherQueue
  -- define how we should iterate the crusher
  let crusherNet act q =
        proc () ->
        do t  <- arrNet (const $ IQ.dequeue q) -< ()
           t' <- activityNet act  -< t
           arrNet truckReturnTrip -< t'
  let shovelNet act q =
        proc () ->
        do t  <- arrNet (const $ IQ.dequeue q) -< ()
           t' <- activityNet act -< t
           arrNet truckTravel    -< t'
  -- start processing the cursher's queue
  runProcessInStartTime $
    iterateNet (crusherNet crusherAct crusherQueue) ()
  -- start processing the shovel queues
  runProcessInStartTime $
    iterateNet (shovelNet shovelAct1 shovelQueue1) ()
  runProcessInStartTime $
    iterateNet (shovelNet shovelAct2 shovelQueue2) ()
  runProcessInStartTime $
    iterateNet (shovelNet shovelAct3 shovelQueue3) ()
  -- return the simulation results in start time
  return $
    results
    [resultSource
     "shovelQueue" "the shovel's queue"
     [shovelQueue1, shovelQueue2, shovelQueue3],
     --
     resultSource
     "crusherQueue" "the crusher's queue"
     crusherQueue,
     --
     resultSource
     "shovelActvty" "the shovel's activity"
     [shovelAct1, shovelAct2, shovelAct3],
     --
     resultSource
     "crusherActvty" "the crusher's activity"
     crusherAct]

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  (fmap resultSummary model) specs
