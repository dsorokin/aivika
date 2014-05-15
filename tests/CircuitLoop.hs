
import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import Simulation.Aivika

specs = Specs 0 10 1 RungeKutta4 SimpleGenerator

model :: Simulation ()
model =
  do let swap (x, y) = (y, x)
         -- k = loop (arr swap)
         k = loop (arr id)

     s <-
       runEventInStartTime
       newSignalInIntegTimes

     runEventInStartTime $
       handleSignal_ (k s) $ \x ->
       liftIO $
       do putStr "Received signal: "
          putStrLn (show x)

     runEventInStopTime $
       return ()

main = runSimulation model specs
