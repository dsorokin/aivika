
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

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika

ackRate = 1.0 / 1.0  -- reciprocal of the acknowledge mean time
toPeriod = 0.5       -- timeout period

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 10000.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
     
model :: Simulation Double
model =
  do -- number of messages sent
     nMsgs <- newRef 0
     
     -- number of timeouts which have occured
     nTimeOuts <- newRef 0
     
     -- reactivatedCode will 1 if timeout occurred, 
     -- 2 ACK if received
     reactivatedCode <- newRef 0
     
     nodePid <- newProcessId
     
     let node :: Process ()
         node =
           do liftEvent $ modifyRef nMsgs $ (+) 1
              -- create process IDs
              timeoutPid <- liftSimulation newProcessId
              ackPid <- liftSimulation newProcessId
              -- set up the timeout
              liftEvent $ runProcessUsingId timeoutPid (timeout ackPid)
              -- set up the message send/ACK
              liftEvent $ runProcessUsingId ackPid (acknowledge timeoutPid)
              passivateProcess
              liftEvent $
                do code <- readRef reactivatedCode
                   when (code == 1) $
                     modifyRef nTimeOuts $ (+) 1
                   writeRef reactivatedCode 0
              node
              
         timeout :: ProcessId -> Process ()
         timeout ackPid =
           do holdProcess toPeriod
              liftEvent $
                do writeRef reactivatedCode 1
                   reactivateProcess nodePid
                   cancelProcessWithId ackPid
         
         acknowledge :: ProcessId -> Process ()
         acknowledge timeoutPid =
           do ackTime <-
                liftParameter $
                randomExponential (1 / ackRate)
              holdProcess ackTime
              liftEvent $
                do writeRef reactivatedCode 2
                   reactivateProcess nodePid
                   cancelProcessWithId timeoutPid

     runProcessInStartTimeUsingId
       nodePid node
     
     runEventInStopTime $
       do x <- readRef nTimeOuts
          y <- readRef nMsgs
          return $ x / y
  
main = 
  do putStr "The percentage of timeout was "
     runSimulation model specs >>= print
