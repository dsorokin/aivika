
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
     
model :: Simulation Results
model =
  do -- number of machines currently up
     nUp <- newRef 2
     
     -- total up time for all machines
     totalUpTime <- newRef 0.0
     
     repairPerson <- newResource FCFS 1
     
     pid1 <- newProcessId
     pid2 <- newProcessId
     
     let machine :: ProcessId -> Process ()
         machine pid =
           do upTime <-
                liftParameter $
                randomExponential meanUpTime
              holdProcess upTime
              liftEvent $
                modifyRef totalUpTime (+ upTime) 
              
              liftEvent $
                modifyRef nUp (+ (-1))
              nUp' <- liftEvent $ readRef nUp
              if nUp' == 1
                then passivateProcess
                else liftEvent $
                     do n <- resourceCount repairPerson
                        when (n == 1) $ 
                          reactivateProcess pid
              
              requestResource repairPerson
              repairTime <-
                liftParameter $
                randomExponential meanRepairTime
              holdProcess repairTime
              liftEvent $
                modifyRef nUp (+ 1)
              releaseResource repairPerson
              
              machine pid

     runProcessInStartTimeUsingId
       pid1 (machine pid2)

     runProcessInStartTimeUsingId
       pid2 (machine pid1)

     let upTimeProp =
           do x <- readRef totalUpTime
              y <- liftDynamics time
              return $ x / (2 * y)

     return $
       results
       [resultSource
        "upTimeProp"
        "The long-run proportion of up time (~ 0.45)"
        upTimeProp]
  
main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
