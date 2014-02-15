
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

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika

meanUpTime = 1.0
meanRepairTime = 0.5

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
     
model :: Simulation (Double, Double)
model =
  do -- number of times the machines have broken down
     nRep <- newRef 0 
     
     -- number of breakdowns in which the machine 
     -- started repair service right away
     nImmedRep <- newRef 0
     
     -- total up time for all machines
     totalUpTime <- newRef 0.0
     
     repairPerson <- newFCFSResource 1
     
     let machine :: Process ()
         machine =
           do upTime <-
                liftParameter $
                randomExponential meanUpTime
              holdProcess upTime
              liftEvent $
                modifyRef totalUpTime (+ upTime) 
              
              -- check the resource availability
              liftEvent $
                do modifyRef nRep (+ 1)
                   n <- resourceCount repairPerson
                   when (n == 1) $
                     modifyRef nImmedRep (+ 1)
                
              requestResource repairPerson
              repairTime <-
                liftParameter $
                randomExponential meanRepairTime
              holdProcess repairTime
              releaseResource repairPerson
              
              machine

     runProcessInStartTime machine
     runProcessInStartTime machine
          
     runEventInStopTime $
       do x <- readRef totalUpTime
          y <- liftParameter stoptime
          n <- readRef nRep
          nImmed <- readRef nImmedRep
          return (x / (2 * y), 
                  fromIntegral nImmed / fromIntegral n)
  
main = runSimulation model specs >>= print
