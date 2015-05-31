
-- Example: Machine Tool with Breakdowns 
--
-- It is described in different sources [1, 2]. So, this is chapter 13 of [2] and section 6.12 of [1].
--
-- Jobs arrive to a machine tool on the average of one per hour. The distribution of 
-- these interarrival times is exponential. During normal operation, the jobs are 
-- processed on a first-in, first-out basis. The time to process a job in hours is 
-- normally distributed with a mean of 0.5 and a standard deviation of 0.1. In addition 
-- to the processing time, there is a set up time that is uniformly distributed between 
-- 0.2 and 0.5 of an hour. Jobs that have been processed by the machine tool are routed 
-- to a different section of the shop and are considered to have left the machine tool 
-- area.
-- 
-- The machine tool experiences breakdowns during which time it can no longer process 
-- jobs. The time between breakdowns is normally distributed with a mean of 20 hours 
-- and a standard deviation of 2 hours. When a breakdown occurs, the job being processed 
-- is removed from the machine tool and is placed at the head of the queue of jobs 
-- waiting to be processed. Jobs preempted restart from the point at which they were 
-- interrupted.
-- 
-- When the machine tool breaks down, a repair process is initiated which is 
-- accomplished in three phases. Each phase is exponentially distributed with a mean of 
-- 3/4 of an hour. Since the repair time is the sum of independent and identically 
-- distributed exponential random variables, the repair time is Erlang distributed. 
-- The machine tool is to be analyzed for 500 hours to obtain information on 
-- the utilization of the machine tool and the time required to process a job. 
-- Statistics are to be collected for thousand simulation runs.
--
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

import Control.Monad
import Control.Monad.Trans
import Control.Category

import Data.Monoid
import Data.List

import Simulation.Aivika
import qualified Simulation.Aivika.Queue.Infinite as IQ
import qualified Simulation.Aivika.Resource.Preemption as PR

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 500.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- | How often do jobs arrive to a machine tool (exponential)?
jobArrivingMu = 1

-- | A mean of time to process a job (normal). 
jobProcessingMu = 0.5

-- | The standard deviation of time to process a job (normal).
jobProcessingSigma = 0.1

-- | The minimum set-up time (uniform).
minSetUpTime = 0.2

-- | The maximum set-up time (uniform).
maxSetUpTime = 0.5

-- | A mean of time between breakdowns (normal).
breakdownMu = 20

-- | The standard deviation of time between breakdowns (normal).
breakdownSigma = 2

-- | A mean of each of the three repair phases (Erlang).
repairMu = 3/4

-- | A priority of the job (less is higher)
jobPriority = 1

-- | A priority of the breakdown (less is higher)
breakdownPriority = 0

-- | The simulation model.
model :: Simulation Results
model = do
  -- create an input queue
  inputQueue <- runEventInStartTime IQ.newFCFSQueue
  -- a counter of jobs completed
  jobsCompleted <- newArrivalTimer
  -- a counter of interrupted jobs
  jobsInterrupted <- newRef (0 :: Int)
  -- create an input stream
  let inputStream =
        randomExponentialStream jobArrivingMu
  -- create a preemptible resource
  tool <- PR.newResource 1
  -- the machine setting up
  machineSettingUp <-
    newPreemptibleRandomUniformServer True minSetUpTime maxSetUpTime
  -- the machine processing
  machineProcessing <-
    newPreemptibleRandomNormalServer True jobProcessingMu jobProcessingSigma
  -- the machine breakdown
  let machineBreakdown =
        do randomNormalProcess_ breakdownMu breakdownSigma
           PR.usingResourceWithPriority tool breakdownPriority $
             randomErlangProcess_ repairMu 3
           machineBreakdown
  -- start the process of breakdowns
  runProcessInStartTime machineBreakdown
  -- update a counter of job interruptions
  runEventInStartTime $
    handleSignal_ (serverTaskPreemptionBeginning machineProcessing) $ \a ->
    modifyRef jobsInterrupted (+ 1)
  -- define the queue network
  let network = 
        queueProcessor
        (\a -> liftEvent $ IQ.enqueue inputQueue a)
        (IQ.dequeue inputQueue) >>>
        (withinProcessor $ PR.requestResourceWithPriority tool jobPriority) >>>
        serverProcessor machineSettingUp >>>
        serverProcessor machineProcessing >>>
        (withinProcessor $ PR.releaseResource tool) >>>
        arrivalTimerProcessor jobsCompleted
  -- start the machine tool
  runProcessInStartTime $
    sinkStream $ runProcessor network inputStream
  -- return the simulation results in start time
  return $
    results
    [resultSource
     "inputQueue" "the queue of jobs"
     inputQueue,
     --
     resultSource
     "machineSettingUp" "the machine setting up"
     machineSettingUp,
     --
     resultSource
     "machineProcessing" "the machine processing"
     machineProcessing,
     --
     resultSource
     "jobsInterrupted" "a counter of the interrupted jobs"
     jobsInterrupted,
     --
     resultSource
     "jobsCompleted" "a counter of the completed jobs"
     jobsCompleted]

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  (fmap resultSummary model) specs
