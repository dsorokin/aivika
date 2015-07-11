
{-# LANGUAGE RecursiveDo #-}

-- Example: Analysis of a PERT-type Network 
--
-- It is described in different sources [1, 2]. So, this is chapter 14 of [2] and section 7.11 of [1].
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
