
-- Example: Machine Tool with Breakdowns 
--
-- It is described in different sources [1, 2]. So, this is chapter 13 of [2] and section 6.12 of [1].
--
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
--
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
        traceStream Nothing (Just "a new job") $
        randomExponentialStream jobArrivingMu
  -- create a preemptible resource
  tool <- PR.newResource 1
  -- the machine setting up
  machineSettingUp <-
    newPreemptibleServer True $ \a ->
    PR.usingResourceWithPriority tool jobPriority $
    do setUpTime <-
         liftParameter $
         randomUniform minSetUpTime maxSetUpTime
       traceProcess "setting up the machine" $
         holdProcess setUpTime
       return a
  -- the machine processing
  machineProcessing <-
    newPreemptibleServer True $ \a ->
    PR.usingResourceWithPriority tool jobPriority $
    do jobProcessingTime <-
         liftParameter $
         randomNormal jobProcessingMu jobProcessingSigma
       traceProcess "proceeeding to the job" $
         when (jobProcessingTime > 0) $
         holdProcess jobProcessingTime
       return a
  -- the machine breakdown
  let machineBreakdown =
        do upTime <-
             liftParameter $
             randomNormal breakdownMu breakdownSigma
           when (upTime > 0) $
             holdProcess upTime
           traceProcess "breakdown" $
             PR.usingResourceWithPriority tool breakdownPriority $
             do repairTime <- liftParameter $
                              randomErlang repairMu 3
                holdProcess repairTime
           traceProcess "repaired" $
             machineBreakdown
  -- start the process of breakdowns
  runProcessInStartTime machineBreakdown
  -- update a counter of job interruptions
  runEventInStartTime $
    handleSignal_ (serverTaskPreempting machineProcessing) $ \a ->
    modifyRef jobsInterrupted (+ 1)
  -- define the queue network
  let network = 
        traceProcessor Nothing (Just "the job completed") $
        queueProcessor
        (\a -> liftEvent $ IQ.enqueue inputQueue a)
        (IQ.dequeue inputQueue) >>>
        serverProcessor machineSettingUp >>>
        serverProcessor machineProcessing >>>
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
