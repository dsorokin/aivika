
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika

specs = Specs 0 10 0.01 RungeKutta4 SimpleGenerator

model :: Simulation Results
model =
  do x <- runCompositeInStartTime_ $
          newRandomExponentialSignal 0.5

     timer <- newArrivalTimer

     let y = traceSignal "Y" $
             arrivalTimerSignal timer $
             delaySignal 1 $
             traceSignal "X" x

     runCompositeInStartTime_ $
       sinkSignal y

     return $
       results
       [resultSource "timer" "the arrival timer" timer]

main =
  printSimulationResultsInStopTime
  printResultSourceInRussian
  model specs
