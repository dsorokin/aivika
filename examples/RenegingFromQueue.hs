
-- Example: Reneging from a Queue
--
-- It is described in [1]. This is section 7.8.
-- 
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import qualified Simulation.Aivika.Queue.Infinite as IQ
import qualified Simulation.Aivika.Resource as R

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 10000.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

mu = 10
nu = 2

logMu = log mu - logNu * logNu / 2
logNu = sqrt $ log ((nu * nu) / (mu * mu) + 1) 

model :: Simulation Results
model = do
  let customers = randomExponentialStream 20
  timeInSystem <- newRef emptySamplingStats
  renegeEvents <- newRef (0 :: Int)
  queue <- runEventInStartTime $ IQ.newFCFSQueue
  let queueProcess =
        do x <- IQ.dequeue queue
           randomUniformProcess_ 15 25
           t <- liftDynamics time
           liftEvent $
             modifyRef timeInSystem $
             addSamplingStats (t - arrivalTime x)
           queueProcess
      renegingProcess x =
        do randomLogNormalProcess_ logMu logNu
           liftEvent $
             do f <- IQ.queueDelete queue x
                when f $ modifyRef renegeEvents ((+) 1)
  runProcessInStartTime queueProcess
  runProcessInStartTime $
    flip consumeStream customers $ \x ->
    liftEvent $
    do IQ.enqueue queue x
       runProcess $ renegingProcess x
  return $
    results
    [resultSource
     "queue" "the queue"
     queue,
     --
     resultSource
     "timeInSystem" "time in system"
     timeInSystem,
     --
     resultSource
     "renegeEvents" "reneging events"
     renegeEvents]

modelSummary :: Simulation Results
modelSummary =
  fmap resultSummary model

main =
  do printSimulationResultsInStopTime
       printResultSourceInEnglish
       modelSummary specs
