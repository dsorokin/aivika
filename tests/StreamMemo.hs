
import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import Simulation.Aivika

specs = Specs 0 10 0.1 RungeKutta4 SimpleGenerator

model :: Simulation ()
model =
  do let display n a =
           do t <- liftDynamics time
              liftIO $
                do putStr "n = "
                   putStr $ show n
                   putStr ", t = "
                   putStr $ show t
                   putStr ", a = "
                   putStrLn $ show a
                   
     let trace :: String -> Stream Arrival -> Process ()
         trace name =
           consumeStream (\a -> display name a)
     
     s <- memoStream $ randomUniformStream 1 2
         
     runProcessInStartTime IncludingCurrentEvents $
       trace "MemoizedS    " s
     
     runProcessInStartTime IncludingCurrentEvents $
       trace "MemoizedS(2) " s
     
     runProcessInStartTime IncludingCurrentEvents $
       trace "MemoizedS(3) " s
     
     runProcessInStartTime IncludingCurrentEvents $
       trace "MemoizedS(4) " s
 
     runEventInStopTime IncludingCurrentEvents $
       return ()

main = runSimulation model specs
