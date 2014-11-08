
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

import Simulation.Aivika
import qualified Simulation.Aivika.Queue.Infinite as IQ

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

-- | It defines a job.
data Job = Job { jobProcessingTime :: Double,
                 -- ^ the job processing time defined when arriving.
                 jobRemainingTime :: Double
                 -- ^ the remaining processing time (may differ after return).
               }

model :: Simulation Results
model = do
  -- create an input queue
  inputQueue <- runEventInStartTime $ IQ.newFCFSQueue
  -- a counter of jobs completed
  jobsCompleted <- newArrivalTimer
  -- a counter of interrupted jobs but then returned for the further processing
  jobsInterrupted <- newRef (0 :: Int)
  -- create an input stream
  let inputStream =
        traceStream Nothing (Just "taking a job from the queue") $
        repeatProcess $ IQ.dequeue inputQueue
  -- create the setting up phase of processing
  machineSettingUp <-
    newServer $ \a ->
    do -- set up the machine
       setUpTime <-
         liftParameter $
         randomUniform minSetUpTime maxSetUpTime
       holdProcess setUpTime
       return a
  -- create the processing phase itself
  machineProcessing <-
    newInterruptibleServer True $ \a ->
    do -- process the job
       let job = arrivalValue a
       holdProcess $ jobRemainingTime job
       -- return the completed job
       return a { arrivalValue = job { jobRemainingTime = 0 } }
  -- define the network
  let network =
        traceProcessor Nothing (Just "the job completed") $
        serverProcessor machineSettingUp >>>
        serverProcessor machineProcessing >>>
        arrivalTimerProcessor jobsCompleted
  -- enqueue the interrupted jobs again
  runEventInStartTime $
    handleSignal_ (serverTaskInterrupted machineProcessing) $ \x ->
    traceEvent "interrupting the job.." $
    do let t1 = serverStartProcessingTime x
           t2 = serverInterruptionTime x
           dt = t2 - t1
           a  = serverInterruptedInput x
           a' = a { arrivalValue = job' }
           job  = arrivalValue a
           job' = job { jobRemainingTime =
                           max 0 $ jobRemainingTime job - dt }
       modifyRef jobsInterrupted (+ 1)
       IQ.enqueue inputQueue a'
  -- launch the machine tool
  let launch = do
        -- breakdown the machine tool in time (a bound child process)
        spawnProcess $ do
          breakdownTime <-
            liftParameter $
            randomNormal breakdownMu breakdownSigma
          holdProcess breakdownTime
          traceProcess "breakdown" $
            cancelProcess
        -- model the machine tool itself
        let loop =
              -- process the jobs until interrupting
              sinkStream $
                runProcessor network inputStream
        -- model the repairing of the tool
        let repair = do
              -- at first repair the machine
              repairTime <- liftParameter $
                            randomErlang repairMu 3
              holdProcess repairTime
              -- then launch it again (an independent process)
              traceProcess "repaired" $
                liftEvent $
                runProcess launch
        -- start simulating the machine tool with an ability to repair
        finallyProcess loop repair
  -- start the machine tool
  runProcessInStartTime launch
  -- model a stream of jobs
  let jobs =
        traceStream Nothing (Just "a new job") $
        randomExponentialStream jobArrivingMu
  -- start the processing of jobs by enqueueing them
  runProcessInStartTime $
    flip consumeStream jobs $ \a ->
    liftEvent $ do
      -- define the processing time for the job
      jobProcessingTime <-
        liftParameter $
        randomNormal jobProcessingMu jobProcessingSigma
      -- enqueue the job
      IQ.enqueue inputQueue $
        a { arrivalValue =
               Job jobProcessingTime jobProcessingTime }
  -- return the simulation results in start time
  return $
    results
    [resultSource
     "inputQueue" "the queue of jobs"
     inputQueue,
     --
     resultSource
     "machineSettingUp" "the machine tool (the setting up phase)"
     machineSettingUp,
     --
     resultSource
     "machineProcessing" "the machine tool (the processing phase)"
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
