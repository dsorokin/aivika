
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Ref
import Simulation.Aivika.QueueStrategy
import Simulation.Aivika.Queue
import Simulation.Aivika.Resource
import Simulation.Aivika.Process

specs = Specs 0 1 0.1 RungeKutta4

n1 = 3
n2 = 5

model :: Simulation ()
model =
  do q <- newQueue FCFS StaticPriorities FCFS n2

     pid <- newProcessId

     let timer :: Int -> Int -> Process Int
         timer i n =
           do t'  <- liftDynamics time
              dt' <- liftDynamics dt
              liftIO $
                do putStr "Current nested process (number "
                   putStr $ show i
                   putStr ", cycle "
                   putStr $ show n
                   putStr "): time = "
                   putStrLn $ show t'
              holdProcess dt'
              when ((i == 3) && (n == 1)) $
                do pid <- processId
                   --
                   -- N.B. uncomment this to cancel all processes immediately
                   -- liftEvent $ cancelProcess pid
                   --
                   -- N.B. or, uncomment this to raise the IO exception
                   -- liftIO $ ioError $ userError "Test error"
                   --
                   return ()
              if (n < n1)
                then timer i (n + 1)
                else return i

     runProcessInStartTimeUsingId IncludingCurrentEvents pid $
       do let m1 =
                do xs <- processParallel $ map (\i -> timer i 0) [1..n2]
                   liftIO $
                     do putStr $ "The result after all nested processes completed is "
                        putStrLn $ show xs
              m2 =
                liftIO $ putStrLn "Finalization"
              m3 e =
                liftIO $ putStrLn "An error has happened"
          finallyProcess
            (catchProcess m1 m3)
            m2

     runEventInStopTime IncludingCurrentEvents $ return ()

main = runSimulation model specs >>= print
