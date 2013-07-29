
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

specs = Specs 0 1 0.1 RungeKutta4

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
