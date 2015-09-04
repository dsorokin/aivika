
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika

specs = Specs 0 10 1 RungeKutta4 SimpleGenerator

model :: Simulation ()
model =
  do signalA <-
       fmap (traceSignal "Signal A") $
       runEventInStartTime
       newSignalInIntegTimes

     let signalB =
           traceSignal "Signal B" $
           delaySignal 3.1 signalA

     h <- runEventInStartTime $
          handleSignal signalB $ \a -> return ()

     runEventInStartTime $
       enqueueEvent 5.2 $
       disposeEvent h
       
     runEventInStopTime $
       return ()

main = runSimulation model specs
