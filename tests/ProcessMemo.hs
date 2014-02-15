
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

     let p = do x <- liftParameter $ randomExponential 0.1
                holdProcess x
                return x

     memoizedP <- memoProcess p

     runProcessInStartTime IncludingCurrentEvents $
       memoizedP >>= display "MemoizedP   "

     runProcessInStartTime IncludingCurrentEvents $
       memoizedP >>= display "MemoizedP(2)"

     runProcessInStartTime IncludingCurrentEvents $
       memoizedP >>= display "MemoizedP(3)"

     runProcessInStartTime IncludingCurrentEvents $
       memoizedP >>= display "MemoizedP(4)"

     runEventInStopTime IncludingCurrentEvents $
       return ()

main = runSimulation model specs
