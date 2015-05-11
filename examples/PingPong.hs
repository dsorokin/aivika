
-- A classic Ping-Pong example.

import Control.Monad.Trans

import Simulation.Aivika

delta = 1.0

specs = Specs { spcStartTime = 0,
                spcStopTime = 10 * delta,
                spcDT = delta,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation ()
model =
  do pingSource <- newSignalSource
     pongSource <- newSignalSource
     let pingSignal = publishSignal pingSource
         pongSignal = publishSignal pongSource
         ping =
           do holdProcess delta
              liftEvent $
                traceEvent "ping" $
                triggerSignal pingSource ()
              processAwait pongSignal
              ping
         pong =
           do processAwait pingSignal
              liftEvent $
                traceEvent "pong" $
                triggerSignal pongSource ()
              pong
     runProcessInStartTime ping
     runProcessInStartTime pong
     runEventInStopTime $
       traceEvent "end" $
       return ()

main = runSimulation model specs
