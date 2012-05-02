
-- It corresponds to model MachRep2 described in document 
-- Introduction to Discrete-Event Simulation and the SimPy Language
-- [http://heather.cs.ucdavis.edu/~matloff/156/PLN/DESimIntro.pdf]. 
-- SimPy is available on [http://simpy.sourceforge.net/].
--   
-- The model description is as follows.
--   
-- Two machines, but sometimes break down. Up time is exponentially 
-- distributed with mean 1.0, and repair time is exponentially distributed 
-- with mean 0.5. In this example, there is only one repairperson, so 
-- the two machines cannot be repaired simultaneously if they are down 
-- at the same time.
--
-- In addition to finding the long-run proportion of up time as in
-- model MachRep1, let’s also find the long-run proportion of the time 
-- that a given machine does not have immediate access to the repairperson 
-- when the machine breaks down. Output values should be about 0.6 and 0.67. 

import Random
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Base
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Ref
import Simulation.Aivika.Dynamics.Resource
import Simulation.Aivika.Dynamics.Process

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
     
model :: Simulation (Double, Double)
model =
  do queue <- newQueue
     
     -- number of times the machines have broken down
     nRep <- newRef queue 0 
     
     -- number of breakdowns in which the machine 
     -- started repair service right away
     nImmedRep <- newRef queue 0
     
     -- total up time for all machines
     totalUpTime <- newRef queue 0.0
     
     repairPerson <- newResource queue 1
     
     pid1 <- newProcessID queue
     pid2 <- newProcessID queue
     
     let machine :: Process ()
         machine =
           do startUpTime <- liftDynamics time
              upTime <- liftIO $ exprnd upRate
              holdProcess upTime
              finishUpTime <- liftDynamics time
              liftDynamics $ modifyRef totalUpTime 
                (+ (finishUpTime - startUpTime))
              
              -- check the resource availability
              liftDynamics $ modifyRef nRep (+ 1)
              n <- liftDynamics $ resourceCount repairPerson
              when (n == 1) $
                liftDynamics $ modifyRef nImmedRep (+ 1)
                
              requestResource repairPerson
              repairTime <- liftIO $ exprnd repairRate
              holdProcess repairTime
              releaseResource repairPerson
              
              machine
         
     runDynamicsInStart $
       do t0 <- starttime
          runProcess machine pid1 t0
          runProcess machine pid2 t0
     
     let system :: Dynamics (Double, Double)
         system =
           do x <- readRef totalUpTime
              y <- stoptime
              n <- readRef nRep
              nImmed <- readRef nImmedRep
              return (x / (2 * y), 
                      fromIntegral nImmed / fromIntegral n)
     
     runDynamicsInFinal system
  
main = runSimulation model specs
