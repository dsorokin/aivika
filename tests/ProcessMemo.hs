
import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import System.Random

import Simulation.Aivika.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Ref
import Simulation.Aivika.QueueStrategy
import Simulation.Aivika.Queue
import Simulation.Aivika.Resource
import Simulation.Aivika.Process
import Simulation.Aivika.Processor
import Simulation.Aivika.Random
import Simulation.Aivika.Stream
import Simulation.Aivika.Stream.Random

specs = Specs 0 1 0.1 RungeKutta4

n1 = 3
n2 = 5

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

     let p = do x <- liftIO $ exponentialGen 0.1
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
