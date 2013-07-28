
import System.Random
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Event
import Simulation.Aivika.Event
import Simulation.Aivika.Ref
import Simulation.Aivika.QueueStrategy
import Simulation.Aivika.Queue
import Simulation.Aivika.Resource
import Simulation.Aivika.Process

upRate = 1.0 / 1.0       -- reciprocal of mean up time
repairRate = 1.0 / 0.5   -- reciprocal of mean repair time

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4 }
        
exprnd :: Double -> IO Double
exprnd lambda =
  do x <- getStdRandom random
     return (- log x / lambda)

n1 = 10
n2 = 3

model :: Simulation ()
model =
  do q <- newQueue FCFS StaticPriorities FCFS n2

     pid1 <- newProcessId
     pid2 <- newProcessId

     runProcessInStartTime IncludingCurrentEvents pid1 $
       let loop =
             do liftIO $
                  putStrLn "Preparing to dequeue..."
                x <- dequeue q
                liftIO $
                  do putStr "Dequeued: "
                     putStrLn $ show x
                loop
       in loop

     runProcessInStartTime IncludingCurrentEvents pid2 $
       let loop i =
             do liftIO $
                  do putStr "Preparing to enqueue: "
                     putStr $ show i
                     putStrLn "... "
                enqueueWithStoringPriority q (100 - 1) i
                liftIO $
                  do putStr "Enqueued: "
                     putStrLn $ show i
                -- -- pause
                -- holdProcess 0
                when (i < n1) $
                  loop $ i + 1
       in loop 1

     runEventInStopTime IncludingCurrentEvents $
       return ()
  
main = runSimulation model specs >>= print
