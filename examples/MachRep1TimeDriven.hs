
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

import Control.Monad.Trans

import Simulation.Aivika

meanUpTime = 1.0
meanRepairTime = 0.5

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 0.05,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation Results
model =
  do totalUpTime <- newRef 0.0
     
     let machine :: Simulation (Event ())
         machine =
           do startUpTime <- newRef 0.0 
             
              -- a number of iterations when 
              -- the machine works
              upNum <- newRef (-1)
              
              -- a number of iterations when 
              -- the machine is broken
              repairNum <- newRef (-1)
              
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
                            finishUpTime' <- liftDynamics time
                            dt' <- liftParameter dt
                            modifyRef totalUpTime $ 
                              \a -> a +
                              (finishUpTime' - startUpTime')
                            repairTime' <-
                              liftParameter $
                              randomExponential meanRepairTime
                            writeRef repairNum $
                              round (repairTime' / dt')
                              
                       repaired =
                         do writeRef repairNum (-1)
                            -- the machine is repaired
                            t'  <- liftDynamics time
                            dt' <- liftParameter dt
                            writeRef startUpTime t'
                            upTime' <-
                              liftParameter $
                              randomExponential meanUpTime
                            writeRef upNum $
                              round (upTime' / dt')
                              
                       result | upNum' > 0      = untilBroken
                              | upNum' == 0     = broken
                              | repairNum' > 0  = untilRepaired
                              | repairNum' == 0 = repaired
                              | otherwise       = repaired 
                   result
                            
     -- create two machines with type Event ()
     m1 <- machine
     m2 <- machine

     -- start the time-driven simulation of the machines
     runEventInStartTime $
       -- in the integration time points
       enqueueEventWithIntegTimes $
       do m1
          m2

     let upTimeProp =
           do x <- readRef totalUpTime
              y <- liftDynamics time
              return $ x / (2 * y)

     return $
       results
       [resultSource
        "upTimeProp"
        "The long-run proportion of up time (~ 0.66)"
        upTimeProp]
  
main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
