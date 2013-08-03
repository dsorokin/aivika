
-- It corresponds to model TimeOut described in document 
-- Advanced Features of the SimPy Language
-- [http://heather.cs.ucdavis.edu/~matloff/156/PLN/AdvancedSimPy.pdf]. 
-- SimPy is available on [http://simpy.sourceforge.net/].
--   
-- The model description is as follows.
--
-- Introductory example to illustrate the modeling of "competing
-- events" such as timeouts, especially using the timeoutProcess and
-- awaitSignal function. A network node starts a process within
-- the specified timeout and receives a signal that notifies whether
-- the process has finished successfully within the timeout; if the node
-- times out, it assumes the message it had sent was lost, and it
-- will send again. The time to get an acknowledgement for a message is
-- exponentially distributed with mean 1.0, and the timeout period is
-- 0.5. Immediately after receiving an acknowledgement, the node sends
-- out a new message.
--
-- We find the proportion of messages which timeout. The output should
-- be about 0.61.

import Control.Monad
import Control.Monad.Trans

import Data.Maybe

import Simulation.Aivika.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Event
import Simulation.Aivika.Ref
import Simulation.Aivika.Process
import Simulation.Aivika.Signal
import Simulation.Aivika.Random

ackRate = 1.0 / 1.0  -- reciprocal of the acknowledge mean time
toPeriod = 0.5       -- timeout period

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 10000.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4 }
        
model :: Simulation Double
model =
  do -- number of messages sent
     nMsgs <- newRef 0
     
     -- number of timeouts which have occured
     nTimeOuts <- newRef 0
     
     let node :: Process ()
         node =
           do liftEvent $ modifyRef nMsgs $ (+) 1
              signal <-
                liftEvent $
                timeoutProcess toPeriod $
                do ackTime <-
                     liftIO $ exponentialGen (1 / ackRate)
                   holdProcess ackTime
              result <- awaitSignal signal
              liftEvent $
                when (isNothing result) $
                modifyRef nTimeOuts $ (+) 1
              node

     runProcessInStartTime IncludingCurrentEvents
       node
     
     runEventInStopTime IncludingCurrentEvents $
       do x <- readRef nTimeOuts
          y <- readRef nMsgs
          return $ x / y
  
main = 
  do putStr "The percentage of timeout was "
     runSimulation model specs >>= print
