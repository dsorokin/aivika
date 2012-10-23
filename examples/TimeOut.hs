
-- It corresponds to model TimeOut described in document 
-- Advanced Features of the SimPy Language
-- [http://heather.cs.ucdavis.edu/~matloff/156/PLN/AdvancedSimPy.pdf]. 
-- SimPy is available on [http://simpy.sourceforge.net/].
--   
-- The model description is as follows.
--
-- Introductory example to illustrate the modeling of "competing
-- events" such as timeouts, especially using the cancelProcess function. A
-- network node sends a message but also sets a timeout period; if the
-- node times out, it assumes the message it had sent was lost, and it
-- will send again. The time to get an acknowledgement for a message is
-- exponentially distributed with mean 1.0, and the timeout period is
-- 0.5. Immediately after receiving an acknowledgement, the node sends
-- out a new message.
--
-- We find the proportion of messages which timeout. The output should
-- be about 0.61.

import System.Random
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Base
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Ref
import Simulation.Aivika.Dynamics.Process

ackRate = 1.0 / 1.0  -- reciprocal of the acknowledge mean time
toPeriod = 0.5       -- timeout period

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 10000.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4 }
        
exprnd :: Double -> IO Double
exprnd lambda =
  do x <- getStdRandom random
     return (- log x / lambda)
     
model :: Simulation Double
model =
  do queue <- newQueue
     
     -- number of messages sent
     nMsgs <- newRef queue 0
     
     -- number of timeouts which have occured
     nTimeOuts <- newRef queue 0
     
     -- reactivatedCode will 1 if timeout occurred, 
     -- 2 ACK if received
     reactivatedCode <- newRef queue 0
     
     nodePid <- newProcessID queue
     
     let node :: Process ()
         node =
           do liftDynamics $ modifyRef nMsgs $ (+) 1
              -- create process IDs
              timeoutPid <- liftSimulation $ newProcessID queue
              ackPid <- liftSimulation $ newProcessID queue
              -- set up the timeout
              liftDynamics $ runProcessNow (timeout ackPid) timeoutPid
              -- set up the message send/ACK
              liftDynamics $ runProcessNow (acknowledge timeoutPid) ackPid
              passivateProcess
              code <- liftDynamics $ readRef reactivatedCode
              when (code == 1) $
                liftDynamics $ modifyRef nTimeOuts $ (+) 1
              liftDynamics $ writeRef reactivatedCode 0
              node
              
         timeout :: ProcessID -> Process ()
         timeout ackPid =
           do holdProcess toPeriod
              liftDynamics $
                do writeRef reactivatedCode 1
                   reactivateProcess nodePid
                   cancelProcess ackPid
         
         acknowledge :: ProcessID -> Process ()
         acknowledge timeoutPid =
           do ackTime <- liftIO $ exprnd ackRate
              holdProcess ackTime
              liftDynamics $
                do writeRef reactivatedCode 2
                   reactivateProcess nodePid
                   cancelProcess timeoutPid

     runDynamicsInStart $
       runProcessNow node nodePid 
     
     runDynamicsInFinal $
       do x <- readRef nTimeOuts
          y <- readRef nMsgs
          return $ x / y
  
main = 
  do putStr "The percentage of timeout was "
     runSimulation model specs >>= print
