
-- It corresponds to model MachRep1 described in document 
-- Introduction to Discrete-Event Simulation and the SimPy Language
-- [http://heather.cs.ucdavis.edu/~matloff/156/PLN/DESimIntro.pdf]. 
-- SimPy is available on [http://simpy.sourceforge.net/].
--   
-- The model description is as follows.
--
-- Two machines, which sometimes break down.
-- Up time is exponentially distributed with mean 1.0, and repair time is
-- exponentially distributed with mean 0.5. There are two repairpersons,
-- so the two machines can be repaired simultaneously if they are down
-- at the same time.
--
-- Output is long-run proportion of up time. Should get value of about
-- 0.66.

import System.Random
import Control.Monad.Trans

import Simulation.Aivika.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Ref

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
     
model :: Simulation Double
model =
  do totalUpTime <- newRef 0.0
     
     let machineBroken :: Double -> Event ()
         machineBroken startUpTime =
           
           do finishUpTime <- liftDynamics time
              modifyRef totalUpTime (+ (finishUpTime - startUpTime))
              repairTime <- liftIO $ exprnd repairRate
              
              -- enqueue a new event
              let t = finishUpTime + repairTime
              enqueueEvent t machineRepaired
              
         machineRepaired :: Event ()
         machineRepaired =
           
           do startUpTime <- liftDynamics time
              upTime <- liftIO $ exprnd upRate
              
              -- enqueue a new event
              let t = startUpTime + upTime
              enqueueEvent t $ machineBroken startUpTime

     runEventInStartTime IncludingCurrentEvents $
       do -- start the first machine
          machineRepaired
          -- start the second machine
          machineRepaired

     runEventInStopTime IncludingCurrentEvents $
       do x <- readRef totalUpTime
          y <- liftDynamics stoptime
          return $ x / (2 * y)
  
main = runSimulation model specs >>= print
