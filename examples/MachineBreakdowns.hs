
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
  -- create an output queue (to count the completed jobs)
  outputQueue <- runEventInStartTime $ IQ.newFCFSQueue
  -- a counter of jobs completed
  jobsCompleted <- newArrivalTimer
  -- launch the machine tool
  let launch = do
        -- get the process Id
        pid <- processId
        -- breakdown the machine tool in time (a bound child process)
        spawnProcess $ do
          breakdownTime <-
            liftParameter $
            randomNormal breakdownMu breakdownSigma
          holdProcess breakdownTime
          traceProcess "breakdown" $
            cancelProcess
        -- model the machine tool itself
        let loop = do
              -- set up the machine
              setUpTime <-
                liftParameter $
                randomUniform minSetUpTime maxSetUpTime
              holdProcess setUpTime
              -- process the job
              t0 <- liftDynamics time
              jobArrival <- IQ.dequeue inputQueue
              let job = arrivalValue jobArrival 
              finallyProcess
                (do holdProcess $ jobRemainingTime job
                    -- count the job completed
                    t <- liftDynamics time
                    liftEvent $
                      traceEvent "the job is complete" $
                      do IQ.enqueue outputQueue $
                           jobArrival {
                             arrivalValue =
                                job { jobRemainingTime = 0 } })
                (liftEvent $
                 do cancelled <- processCancelled pid
                    -- if the process was cancelled then return the job
                    when cancelled $
                      traceEvent "interrupting the job.." $
                      do t <- liftDynamics time
                         IQ.enqueue inputQueue $
                           jobArrival {
                             arrivalValue =
                                job { jobRemainingTime =
                                         max 0 $ jobRemainingTime job - (t - t0) } })
              -- proceed to the next job
              loop
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
  -- create an output stream
  let outputStream =
        repeatProcess (IQ.dequeue outputQueue)
  -- start counting the jobs completed
  runProcessInStartTime $
    sinkStream $
    runProcessor (arrivalTimerProcessor jobsCompleted) outputStream
  -- return the simulation results in start time
  return $
    results
    [resultSource
     "inputQueue" "the queue of jobs"
     inputQueue,
     --
     resultSource
     "outputQueue" "the queue of jobs completed"
     outputQueue,
     --
     resultSource
     "jobsCompleted" "a counter of the completed jobs"
     jobsCompleted]

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  (fmap resultSummary model) specs
