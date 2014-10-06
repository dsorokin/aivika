
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika
import Simulation.Aivika.Queue

specs = Specs 0 1 0.1 RungeKutta4 SimpleGenerator

n1 = 10
n2 = 3

model :: Simulation ()
model =
  do q <-
       runEventInStartTime $
       newQueue FCFS StaticPriorities FCFS n2

     runProcessInStartTime $
       let loop =
             do liftIO $
                  putStrLn "Preparing to dequeue..."
                x <- dequeue q
                liftIO $
                  do putStr "Dequeued: "
                     putStrLn $ show x
                loop
       in loop

     runProcessInStartTime $
       let loop i =
             do liftIO $
                  do putStr "Preparing to enqueue: "
                     putStr $ show i
                     putStrLn "... "
                enqueueWithStoringPriority q (100 - 1) i
                liftIO $
                  do putStr "Enqueued: "
                     putStrLn $ show i
                -- -- allow dequeueing immediately
                -- holdProcess 0
                when (i < n1) $
                  loop $ i + 1
       in loop 1

     runEventInStopTime $
       return ()
  
main = runSimulation model specs >>= print
