
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

import Random
import Control.Monad.Trans

import Simulation.Aivika.Dynamics

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
     
model :: Dynamics (Dynamics Double)
model =
  do queue <- newQueue
     totalUpTime <- newRef queue 0.0
     
     pid1 <- newPID queue
     pid2 <- newPID queue
     
     let machine :: DynamicsProc ()
         machine =
           do startUpTime <- liftD time
              upTime <- liftIO $ exprnd upRate
              holdProc upTime
              finishUpTime <- liftD time
              liftD $ modifyRef' totalUpTime
                (+ (finishUpTime - startUpTime))
              repairTime <- liftIO $ exprnd repairRate
              holdProc repairTime
              machine
         
     runProc machine pid1 starttime
     runProc machine pid2 starttime
     
     let system :: Dynamics Double
         system =
           do x <- readRef totalUpTime
              y <- stoptime
              return $ x / (2 * y)
     
     return system
  
main =         
  do a <- runDynamics1 model specs
     print a
