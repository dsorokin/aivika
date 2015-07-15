
{-# LANGUAGE Arrows #-}

-- Example: A Truck Hauling Situation (A More Complete Version)
--
-- It is described in different sources [1, 2]. So, this is chapter 9 of [2] and section 7.16 of [1].
-- 
-- The system to be modeled in this example consists of one bulldozer, four trucks,
-- and two man-machine loaders. The bulldozer stockpiles material for the loaders.
-- Two piles of material must be stocked prior to the initiation of any load operation.
-- The time for the bulldozer to stockpile material is Erlang distributed and consists
-- of the sum of two exponential variables each with a men of 4. (This corresponds to
-- an Erlang variable with a mean of 8 and a variance of 32.) In addition to this
-- material, a loader and an unloaded truck must be available before the loading
-- operations can begin. Loading time is exponentially distributed with a mean time of
-- 14 minutes for server 1 and 12 minutes for server 2.
-- 
-- After a truck is loaded, it is hauled, then dumped and must be returned before
-- the truck is available for further loading. Hauling time is normally distributed.
-- When loaded, the average hauling time is 22 minutes. When unloaded, the average
-- time is 18 minutes. In both cases, the standard deviation is 3 minutes. Dumping
-- time is uniformly distributed between 2 and 8 minutes. Following a loading
-- operation, the loaded must rest for a 5 minute period before he is available
-- to begin loading again. The system is to be analyzed for 8 hours and all operations
-- in progress at the end of 8 hours should be completed before terminating
-- the operations for a run.
-- 
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import Data.Monoid
import Data.List
import Data.Array

import Simulation.Aivika
import qualified Simulation.Aivika.Queue.Infinite as IQ

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

data Truck = Truck

data Pile = Pile

data Loader = Loader1
            | Loader2
              deriving (Eq, Ord, Show, Ix)

awaitQueuesNonEmpty q1 q2 q3 =
  do n1 <- liftEvent $ IQ.queueCount q1
     n2 <- liftEvent $ IQ.queueCount q2
     n3 <- liftEvent $ IQ.queueCount q3
     when (n1 == 0 || n2 == 0 || n3 == 0) $
       do let signal = IQ.queueCountChanged_ q1 <>
                       IQ.queueCountChanged_ q2 <>
                       IQ.queueCountChanged_ q3
          processAwait signal
          awaitQueuesNonEmpty q1 q2 q3

-- | The simulation model.
model :: Simulation Results
model = do
  truckQueue <- runEventInStartTime IQ.newFCFSQueue
  loadQueue <- runEventInStartTime IQ.newFCFSQueue
  loaderQueue <- runEventInStartTime IQ.newFCFSQueue
  loaderActvty1 <- newRandomExponentialActivity 14
  loaderActvty2 <- newRandomExponentialActivity 12
  let loaderActvties =
        array (Loader1, Loader2)
        [(Loader1, loaderActvty1),
         (Loader2, loaderActvty2)]
  let start :: Process ()
      start =
        do randomErlangProcess_ 4 2
           randomErlangProcess_ 4 2
           liftEvent $
             IQ.enqueue loadQueue Pile
           t <- liftDynamics time
           when (t <= 480) start
      actvty :: Net () ()
      actvty =
        (arrNet $ \() ->
         do awaitQueuesNonEmpty truckQueue loadQueue loaderQueue
            x1 <- IQ.dequeue truckQueue
            x2 <- IQ.dequeue loadQueue
            x3 <- IQ.dequeue loaderQueue
            return (x1, x3)) >>>
        (proc (truck, loader) ->
          case loader of
            Loader1 ->
              activityNet (loaderActvties ! Loader1) -< (truck, loader)
            Loader2 ->
              activityNet (loaderActvties ! Loader2) -< (truck, loader)) >>>
        (arrNet $ \(truck, loader) ->
          do spawnProcess $
               do holdProcess 5
                  liftEvent $
                    IQ.enqueue loaderQueue loader
             spawnProcess $
               do randomNormalProcess_ 22 3
                  randomUniformProcess_ 2 8
                  randomNormalProcess_ 18 3
                  liftEvent $
                    IQ.enqueue truckQueue truck
             return ())
  runEventInStartTime $
    do forM_ [1..4] $ \i ->
         IQ.enqueue truckQueue Truck
       IQ.enqueue loaderQueue Loader1
       IQ.enqueue loaderQueue Loader2
  runProcessInStartTime $ iterateNet actvty ()
  runProcessInStartTime $ iterateNet actvty ()
  runProcessInStartTime start
  return $
    results
    [ resultSource
     "loadQueue" "Queue Load"
     loadQueue,
     --
     resultSource
     "truckQueue" "Queue Trucks"
     truckQueue,
     --
     resultSource
     "loaderQueue" "Queue Loader"
     loaderQueue,
     --
     resultSource
     "loaderActvties" "Activity Loader"
     loaderActvties]

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  (fmap resultSummary model) specs
