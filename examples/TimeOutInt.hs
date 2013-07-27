
-- It corresponds to model TimeOutInt described in document 
-- Advanced Features of the SimPy Language
-- [http://heather.cs.ucdavis.edu/~matloff/156/PLN/AdvancedSimPy.pdf]. 
-- SimPy is available on [http://simpy.sourceforge.net/].
--   
-- The model description is as follows.
--
-- Same as TimeOut.hs but using interrupts. A network node sends a message
-- but also sets a timeout period; if the node times out, it assumes the
-- message it had sent was lost, and it will send again. The time to get
-- an acknowledgement for a message is exponentially distributed with
-- mean 1.0, and the timeout period is 0.5. Immediately after receiving
-- an acknowledgement, the node sends out a new message.
--
-- We find the proportion of messages which timeout. The output should
-- be about 0.61.

import System.Random
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Ref
import Simulation.Aivika.Process

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
  do -- number of messages sent
     nMsgs <- newRef 0
     
     -- number of timeouts which have occured
     nTimeOuts <- newRef 0
     
     nodePid <- newProcessId
     
     let node :: Process ()
         node =
           do liftEvent $ modifyRef nMsgs $ (+) 1
              -- create the process ID
              timeoutPid <- liftSimulation newProcessId
              -- set up the timeout
              liftEvent $ runProcess timeoutPid timeout
              -- wait for ACK, but could be timeout
              ackTime <- liftIO $ exprnd ackRate 
              holdProcess ackTime
              liftEvent $
                do interrupted <- processInterrupted nodePid
                   if interrupted
                     then modifyRef nTimeOuts $ (+) 1
                     else cancelProcess timeoutPid
              node
              
         timeout :: Process ()
         timeout =
           do holdProcess toPeriod
              liftEvent $ interruptProcess nodePid

     runProcessInStartTime IncludingCurrentEvents
       nodePid node 
     
     runEventInStopTime IncludingCurrentEvents $
       do x <- readRef nTimeOuts
          y <- readRef nMsgs
          return $ x / y
  
main = 
  do putStr "The percentage of timeout was "
     runSimulation model specs >>= print
