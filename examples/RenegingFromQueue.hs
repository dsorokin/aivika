
-- Example: Reneging from a Queue
--
-- It is described in [1]. This is section 7.8.
-- 
-- This example models customers arriving to a queue and leaving a queue
-- after a prescribed period of time. The time between arrivals is 
-- exponentially distributed with a mean of 20 minutes. The service time 
-- for customers is uniformly distributed between 15 and 25 minutes. 
-- Customers will only wait for service if the waiting time is less than
-- a renege time which is lognormally distributed with a mean of 10 minutes
-- and a standard deviation of 2 minutes. It is desired to estimate the time
-- in the system for those customers served, the percent of customers that
-- renege and the length of the waiting time.
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
     "renegeEvents" "renege events"
     renegeEvents]

modelSummary :: Simulation Results
modelSummary =
  fmap resultSummary model

main =
  do printSimulationResultsInStopTime
       printResultSourceInEnglish
       modelSummary specs
