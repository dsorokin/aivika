
import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import Simulation.Aivika

specs = Specs 0 10 1 RungeKutta4 SimpleGenerator

model :: Simulation Results
model =
  do let swap (x, y) = (y, x)
         -- k = loop (arr swap)
         k = loop (arr id)

     y <- runTransform (k timeTransform) undefined

     return $
       results
       [resultSource "LoopedY" "Looped Y" y]

main =
  printSimulationResultsInIntegTimes
  printResultSourceInEnglish
  model specs
