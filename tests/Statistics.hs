
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika

specs = Specs 0 10 0.01 RungeKutta4 SimpleGenerator

model :: Simulation Results
model =
  do x <- memoRandomExponentialDynamics 0.5

     s1 <- newRef emptySamplingStats
     s2 <- newRef emptyTimingStats

     runEventInStartTime $
       enqueueEventWithIntegTimes $
       do t <- liftDynamics time
          a <- liftDynamics x
          modifyRef s1 $ addSamplingStats a
          modifyRef s2 $ addTimingStats t a

     return $
       results
       [resultSource "s1" "sampling statistics | E(s1) ~ 0.5" s1,
        resultSource "s2" "timing statistics | E(s2) ~ 0.5" s2]

main =
  printSimulationResultsInStopTime
  printResultSourceInRussian
  model specs
