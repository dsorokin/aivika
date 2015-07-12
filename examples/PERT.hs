
{-# LANGUAGE RecursiveDo #-}

-- Example: Analysis of a PERT-type Network 
--
-- It is described in different sources [1, 2]. So, this is chapter 14 of [2] and section 7.11 of [1].
--
-- PERT is a technique for evaluating and reviewing a project consisting of
-- interdependent activities. A number of books have been written that describe
-- PERT modeling and analysis procedures. A PERT network activity descriptions
-- are given in a table stated below. All activity times will be assumed to be
-- triangularly distributed. For ease of description, activities have been
-- aggregated. The activities relate to power units, instrumentation, and
-- a new assembly and involve standard types of operations.
-- 
-- In the following description of the project, activity numbers are given
-- in parentheses. At the beginning of the project, three parallel activities
-- can be performed that involve: the disassembly of power units and
-- instrumentation (1); the installation of a new assembly (2); and
-- the preparation for a retrofit check (3). Cleaning, inspecting, and
-- repairing the power units (4) and calibrating the instrumentation (5)
-- can be done only after the power units and instrumentation have been
-- disassembled. Thus, activities 4 and 5 must follow activity 1 in the network.
-- Following the installation of the new assembly (2) and after the instrumentation
-- have been calibrated (5), a check of interfaces (6) and a check of
-- the new assembly (7) can be made. The retrofit check (9) can be made
-- after the assembly is checked (7) and the preparation for the retrofit
-- check (3) has been completed. The assembly and test of power units (8)
-- can be performed following the cleaning and maintenance of power units (4).
-- The project is considered completed when all nine activities are completed.
-- Since activities 6, 8, and 9 require the other activities to precede them,
-- their completion signifies the end of the project. This is indicated on
-- the network by having activities 6, 8, and 9 incident to node 6, the sink
-- node for the project. The objective of this example is to illustrate
-- the procedures for using Aivika to model and simulate project planning network.
-- 
-- Activity    Description                                  Mode Minimum Maximum Average
-- 
--  1          Disassemble power units and instrumentation    3      1       5       3
--  2          Install new assembly                           6      3       9       6
--  3          Prepare for retrofit check                    13     10      19      14
--  4          Clean, inspect, and repair power units         9      3      12       8
--  5          Calibrate instrumentation                      3      1       8       4
--  6          Check interfaces                               9      8      16      11
--  7          Check assembly                                 7      4      13       8
--  8          Assemble and test power units                  6      3       9       6
--  9          Retrofit check                                 3      1       8       4
-- 
-- Node 	Depends of Activities
-- 
--  1              -
--  2              1
--  3              2, 5
--  4              3, 7
--  5              4
--  6              6, 8, 9 
-- 
-- Activity    Depends on Node
-- 
--  1              1
--  2              1
--  3              1
--  4              2
--  5              2
--  6              3
--  7              3
--  8              5
--  9              4
-- 
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import Data.Array
import Data.Maybe
import Data.Monoid

import Simulation.Aivika

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

model :: Simulation Results
model = mdo
  timers' <- forM [2..5] $ \i -> newArrivalTimer
  projCompletionTimer <- newArrivalTimer
  let timers = array (2, 5) $ zip [2..] timers'
      p1 = randomTriangularProcessor 1 3 5
      p2 = randomTriangularProcessor 3 6 9
      p3 = randomTriangularProcessor 10 13 19
      p4 = randomTriangularProcessor 3 9 12
      p5 = randomTriangularProcessor 1 3 8
      p6 = randomTriangularProcessor 8 9 16
      p7 = randomTriangularProcessor 4 7 13
      p8 = randomTriangularProcessor 3 6 9
      p9 = randomTriangularProcessor 1 3 8
  let c2 = arrivalTimerProcessor (timers ! 2)
      c3 = arrivalTimerProcessor (timers ! 3)
      c4 = arrivalTimerProcessor (timers ! 4)
      c5 = arrivalTimerProcessor (timers ! 5)
      c6 = arrivalTimerProcessor projCompletionTimer
  [i1, i2, i3] <- cloneStream 3 n1
  [i4, i5] <- cloneStream 2 n2
  [i6, i7] <- cloneStream 2 n3
  let i9 = n4
      i8 = n5
  let s1 = runProcessor p1 i1
      s2 = runProcessor p2 i2
      s3 = runProcessor p3 i3
      s4 = runProcessor p4 i4
      s5 = runProcessor p5 i5
      s6 = runProcessor p6 i6
      s7 = runProcessor p7 i7
      s8 = runProcessor p8 i8
      s9 = runProcessor p9 i9
  let n1 = takeStream 1 $ randomStream $ return (0, 0)
      n2 = runProcessor c2 s1
      n3 = runProcessor c3 $ firstArrivalStream 2 (s2 <> s5)
      n4 = runProcessor c4 $ firstArrivalStream 2 (s3 <> s7)
      n5 = runProcessor c5 s4
      n6 = runProcessor c6 $ firstArrivalStream 3 (s6 <> s8 <> s9)
  runProcessInStartTime $ sinkStream n6
  return $
    results
    [resultSource
     "timers" "Timers"
     timers,
     --
     resultSource
     "projCompletion" "Project Completion Timer"
     projCompletionTimer]

modelSummary :: Simulation Results
modelSummary =
  fmap resultSummary model

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  -- model specs
  modelSummary specs
