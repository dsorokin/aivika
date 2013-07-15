
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

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Base
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Ref

upRate = 1.0 / 1.0       -- reciprocal of mean up time
repairRate = 1.0 / 0.5   -- reciprocal of mean repair time

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 0.05,
                spcMethod = RungeKutta4 }
        
exprnd :: Double -> IO Double
exprnd lambda =
  do x <- getStdRandom random
     return (- log x / lambda)
     
model :: Simulation Double
model =
  do queue <- newQueue
     totalUpTime <- newRef queue 0.0
     
     let machine :: Simulation (Dynamics ())
         machine =
           do startUpTime <- newRef queue 0.0 
             
              -- a number of iterations when 
              -- the machine works
              upNum <- newRef queue (-1)
              
              -- a number of iterations when 
              -- the machine is broken
              repairNum <- newRef queue (-1)
              
              -- create a simulation model
              return $
                do upNum' <- readRef upNum
                   repairNum' <- readRef repairNum
                   
                   let untilBroken = 
                         modifyRef upNum $ \a -> a - 1
                                                  
                       untilRepaired =
                         modifyRef repairNum $ \a -> a - 1
                                                      
                       broken =
                         do writeRef upNum (-1)
                            -- the machine is broken
                            startUpTime' <- readRef startUpTime
                            finishUpTime' <- time
                            dt' <- dt
                            modifyRef totalUpTime $ 
                              \a -> a +
                              (finishUpTime' - startUpTime')
                            repairTime' <- 
                              liftIO $ exprnd repairRate
                            writeRef repairNum $
                              round (repairTime' / dt')
                              
                       repaired =
                         do writeRef repairNum (-1)
                            -- the machine is repaired
                            t'  <- time
                            dt' <- dt
                            writeRef startUpTime t'
                            upTime' <- 
                              liftIO $ exprnd upRate
                            writeRef upNum $
                              round (upTime' / dt')
                              
                       result | upNum' > 0     = untilBroken
                              | upNum' == 0     = broken
                              | repairNum' > 0 = untilRepaired
                              | repairNum' == 0 = repaired
                              | otherwise      = repaired 
                   result
                            
     -- create two machines with type Dynamics ()
     m1 <- machine
     m2 <- machine

     -- start the time-driven simulation of the machines
     -- through the event queue
     runDynamicsInStartTime $
       do enqueueWithIntegTimes queue m1
          enqueueWithIntegTimes queue m2

     -- return the result in the stop time
     runDynamicsInStopTime $
       do x <- readRef totalUpTime
          y <- stoptime
          return $ x / (2 * y)
  
main = runSimulation model specs >>= print
