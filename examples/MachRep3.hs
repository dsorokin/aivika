
-- It corresponds to model MachRep3 described in document 
-- Introduction to Discrete-Event Simulation and the SimPy Language
-- [http://heather.cs.ucdavis.edu/~matloff/156/PLN/DESimIntro.pdf]. 
-- SimPy is available on [http://simpy.sourceforge.net/].
--   
-- The model description is as follows.
--
-- Variation of models MachRep1, MachRep2. Two machines, but
-- sometimes break down. Up time is exponentially distributed with mean
-- 1.0, and repair time is exponentially distributed with mean 0.5. In
-- this example, there is only one repairperson, and she is not summoned
-- until both machines are down. We find the proportion of up time. It
-- should come out to about 0.45.

import System.Random
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
     
model :: Simulation Double
model =
  do queue <- newQueue
     
     -- number of machines currently up
     nUp <- newRef queue 2
     
     -- total up time for all machines
     totalUpTime <- newRef queue 0.0
     
     repairPerson <- newResource queue 1
     
     pid1 <- newProcessID queue
     pid2 <- newProcessID queue
     
     let machine :: ProcessID -> Process ()
         machine pid =
           do startUpTime <- liftDynamics time
              upTime <- liftIO $ exprnd upRate
              holdProcess upTime
              finishUpTime <- liftDynamics time
              liftDynamics $ modifyRef totalUpTime 
                (+ (finishUpTime - startUpTime))
                
              liftDynamics $ modifyRef nUp $ \a -> a - 1
              nUp' <- liftDynamics $ readRef nUp
              if nUp' == 1
                then passivateProcess
                else liftDynamics $
                     do n <- resourceCount repairPerson
                        when (n == 1) $ 
                          reactivateProcess pid
              
              requestResource repairPerson
              repairTime <- liftIO $ exprnd repairRate
              holdProcess repairTime
              liftDynamics $ modifyRef nUp $ \a -> a + 1
              releaseResource repairPerson
              
              machine pid

     runDynamicsInStartTime $
       do t0 <- starttime
          runProcess (machine pid2) pid1 t0
          runProcess (machine pid1) pid2 t0
     
     runDynamicsInStopTime $
       do x <- readRef totalUpTime
          y <- stoptime
          return $ x / (2 * y)
  
main = runSimulation model specs >>= print
