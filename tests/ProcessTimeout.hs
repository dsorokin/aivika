
import Control.Exception
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika
import Simulation.Aivika.Queue

specs = Specs 0 10 0.1 RungeKutta4 SimpleGenerator

model :: Simulation ()
model =
  do let test :: String -> Double -> ProcessId -> Process a -> Process (Maybe a)
         test n t pid p =
           do liftEvent $
                handleSignal_ (processCancelling pid) $ \() ->
                traceEvent ("Process " ++ n ++ " is cancelling") $
                return ()
              timeoutProcessUsingId t pid p

     let t = 5
         
     let p1 = holdProcess (t / 2) >> return 1
     let p2 = holdProcess (t / 2) >> cancelProcess >> return 2
     let p3 = holdProcess (t + 1) >> return 3

     pid1 <- newProcessId
     pid2 <- newProcessId
     pid3 <- newProcessId

     runProcessInStartTime $
       do x <- test "N1" t pid1 p1
          traceProcess ("N1: x = " ++ show x) $
            return ()

     runProcessInStartTime $
       do x <- test "N2" t pid2 p2
          traceProcess ("N2: x = " ++ show x) $
            return ()

     runProcessInStartTime $
       do x <- test "N3" t pid3 p3
          traceProcess ("N3: x = " ++ show x) $
            return ()

     runEventInStopTime $
       return ()

main = runSimulation model specs >>= print
