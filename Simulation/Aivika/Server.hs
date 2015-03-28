
-- |
-- Module     : Simulation.Aivika.Server
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It models the server that prodives a service.
module Simulation.Aivika.Server
       (-- * Server
        Server,
        newServer,
        newStateServer,
        newPreemptibleServer,
        newPreemptibleStateServer,
        -- * Processing
        serverProcessor,
        -- * Server Properties and Activities
        serverInitState,
        serverState,
        serverTotalInputWaitTime,
        serverTotalProcessingTime,
        serverTotalOutputWaitTime,
        serverTotalPreemptionTime,
        serverInputWaitTime,
        serverProcessingTime,
        serverOutputWaitTime,
        serverPreemptionTime,
        serverInputWaitFactor,
        serverProcessingFactor,
        serverOutputWaitFactor,
        serverPreemptionFactor,
        -- * Summary
        serverSummary,
        -- * Derived Signals for Properties
        serverStateChanged,
        serverStateChanged_,
        serverTotalInputWaitTimeChanged,
        serverTotalInputWaitTimeChanged_,
        serverTotalProcessingTimeChanged,
        serverTotalProcessingTimeChanged_,
        serverTotalOutputWaitTimeChanged,
        serverTotalOutputWaitTimeChanged_,
        serverTotalPreemptionTimeChanged,
        serverTotalPreemptionTimeChanged_,
        serverInputWaitTimeChanged,
        serverInputWaitTimeChanged_,
        serverProcessingTimeChanged,
        serverProcessingTimeChanged_,
        serverOutputWaitTimeChanged,
        serverOutputWaitTimeChanged_,
        serverPreemptionTimeChanged,
        serverPreemptionTimeChanged_,
        serverInputWaitFactorChanged,
        serverInputWaitFactorChanged_,
        serverProcessingFactorChanged,
        serverProcessingFactorChanged_,
        serverOutputWaitFactorChanged,
        serverOutputWaitFactorChanged_,
        serverPreemptionFactorChanged,
        serverPreemptionFactorChanged_,
        -- * Basic Signals
        serverInputReceived,
        serverTaskPreempting,
        serverTaskReentering,
        serverTaskProcessed,
        serverOutputProvided,
        -- * Overall Signal
        serverChanged_) where

import Data.IORef
import Data.Monoid

import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Signal
import Simulation.Aivika.Resource
import Simulation.Aivika.Cont
import Simulation.Aivika.Process
import Simulation.Aivika.Processor
import Simulation.Aivika.Stream
import Simulation.Aivika.Statistics

-- | It models a server that takes @a@ and provides @b@ having state @s@.
data Server s a b =
  Server { serverInitState :: s,
           -- ^ The initial state of the server.
           serverStateRef :: IORef s,
           -- ^ The current state of the server.
           serverProcess :: s -> a -> Process (s, b),
           -- ^ Provide @b@ by specified @a@.
           serverProcessPreemptible :: Bool,
           -- ^ Whether the process can be preempted.
           serverTotalInputWaitTimeRef :: IORef Double,
           -- ^ The counted total time spent in awating the input.
           serverTotalProcessingTimeRef :: IORef Double,
           -- ^ The counted total time spent to process the input and prepare the output.
           serverTotalOutputWaitTimeRef :: IORef Double,
           -- ^ The counted total time spent for delivering the output.
           serverTotalPreemptionTimeRef :: IORef Double,
           -- ^ The counted total time spent being preempted and waiting for the proceeding. 
           serverInputWaitTimeRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the time spent in awaiting the input.
           serverProcessingTimeRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the time spent to process the input and prepare the output.
           serverOutputWaitTimeRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the time spent for delivering the output.
           serverPreemptionTimeRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the time spent being preempted.
           serverInputReceivedSource :: SignalSource a,
           -- ^ A signal raised when the server recieves a new input to process.
           serverTaskPreemptingSource :: SignalSource a,
           -- ^ A signal raised when the task was preempted.
           serverTaskReenteringSource :: SignalSource a,
           -- ^ A signal raised when the task was proceeded after it had been preempted earlier.
           serverTaskProcessedSource :: SignalSource (a, b),
           -- ^ A signal raised when the input is processed and
           -- the output is prepared for deliverying.
           serverOutputProvidedSource :: SignalSource (a, b)
           -- ^ A signal raised when the server has supplied the output.
         }

-- | Create a new server that can provide output @b@ by input @a@.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newServer :: (a -> Process b)
             -- ^ provide an output by the specified input
             -> Simulation (Server () a b)
newServer = newPreemptibleServer False

-- | Create a new server that can provide output @b@ by input @a@
-- starting from state @s@.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newStateServer :: (s -> a -> Process (s, b))
                  -- ^ provide a new state and output by the specified 
                  -- old state and input
                  -> s
                  -- ^ the initial state
                  -> Simulation (Server s a b)
newStateServer = newPreemptibleStateServer False

-- | Create a new preemptible server that can provide output @b@ by input @a@.
newPreemptibleServer :: Bool
                        -- ^ whether the server process can be preempted
                        -> (a -> Process b)
                        -- ^ provide an output by the specified input
                        -> Simulation (Server () a b)
newPreemptibleServer preemptible provide =
  flip (newPreemptibleStateServer preemptible) () $ \s a ->
  do b <- provide a
     return (s, b)

-- | Create a new preemptible server that can provide output @b@ by input @a@
-- starting from state @s@.
newPreemptibleStateServer :: Bool
                             -- ^ whether the server process can be preempted
                             -> (s -> a -> Process (s, b))
                             -- ^ provide a new state and output by the specified 
                             -- old state and input
                             -> s
                             -- ^ the initial state
                             -> Simulation (Server s a b)
newPreemptibleStateServer preemptible provide state =
  do r0 <- liftIO $ newIORef state
     r1 <- liftIO $ newIORef 0
     r2 <- liftIO $ newIORef 0
     r3 <- liftIO $ newIORef 0
     r4 <- liftIO $ newIORef 0
     r5 <- liftIO $ newIORef emptySamplingStats
     r6 <- liftIO $ newIORef emptySamplingStats
     r7 <- liftIO $ newIORef emptySamplingStats
     r8 <- liftIO $ newIORef emptySamplingStats
     s1 <- newSignalSource
     s2 <- newSignalSource
     s3 <- newSignalSource
     s4 <- newSignalSource
     s5 <- newSignalSource
     let server = Server { serverInitState = state,
                           serverStateRef = r0,
                           serverProcess = provide,
                           serverProcessPreemptible = preemptible,
                           serverTotalInputWaitTimeRef = r1,
                           serverTotalProcessingTimeRef = r2,
                           serverTotalOutputWaitTimeRef = r3,
                           serverTotalPreemptionTimeRef = r4,
                           serverInputWaitTimeRef = r5,
                           serverProcessingTimeRef = r6,
                           serverOutputWaitTimeRef = r7,
                           serverPreemptionTimeRef = r8,
                           serverInputReceivedSource = s1,
                           serverTaskPreemptingSource = s2,
                           serverTaskReenteringSource = s3,
                           serverTaskProcessedSource = s4,
                           serverOutputProvidedSource = s5 }
     return server

-- | Return a processor for the specified server.
--
-- The processor updates the internal state of the server. The usual case is when 
-- the processor is applied only once in a chain of data processing. Otherwise; 
-- every time the processor is used, the state of the server changes. Sometimes 
-- it can be indeed useful if you want to aggregate the statistics for different 
-- servers simultaneously, but it would be more preferable to avoid this.
--
-- If you connect different server processors returned by this function in a chain 
-- with help of '>>>' or other category combinator then this chain will act as one 
-- whole, where the first server will take a new task only after the last server 
-- finishes its current task and requests for the next one from the previous processor 
-- in the chain. This is not always that thing you might need.
--
-- To model a sequence of the server processors working independently, you
-- should use the 'processorSeq' function which separates the processors with help of
-- the 'prefetchProcessor' that plays a role of a small one-place buffer in that case.
--
-- The queue processors usually have the prefetching capabilities per se, where
-- the items are already stored in the queue. Therefore, the server processor
-- should not be prefetched if it is connected directly to the queue processor.
serverProcessor :: Server s a b -> Processor a b
serverProcessor server =
  Processor $ \xs -> loop (serverInitState server) Nothing xs
  where
    loop s r xs =
      Cons $
      do t0 <- liftDynamics time
         liftEvent $
           case r of
             Nothing -> return ()
             Just (t', a', b') ->
               do liftIO $
                    do modifyIORef' (serverTotalOutputWaitTimeRef server) (+ (t0 - t'))
                       modifyIORef' (serverOutputWaitTimeRef server) $
                         addSamplingStats (t0 - t')
                  triggerSignal (serverOutputProvidedSource server) (a', b')
         -- get input
         (a, xs') <- runStream xs
         t1 <- liftDynamics time
         liftEvent $
           do liftIO $
                do modifyIORef' (serverTotalInputWaitTimeRef server) (+ (t1 - t0))
                   modifyIORef' (serverInputWaitTimeRef server) $
                     addSamplingStats (t1 - t0)
              triggerSignal (serverInputReceivedSource server) a
         -- provide the service
         (s', b, dt) <-
           if serverProcessPreemptible server
           then serverProcessPreempting server s a
           else do (s', b) <- serverProcess server s a
                   return (s', b, 0)
         t2 <- liftDynamics time
         liftEvent $
           do liftIO $
                do writeIORef (serverStateRef server) $! s'
                   modifyIORef' (serverTotalProcessingTimeRef server) (+ (t2 - t1 - dt))
                   modifyIORef' (serverProcessingTimeRef server) $
                     addSamplingStats (t2 - t1 - dt)
              triggerSignal (serverTaskProcessedSource server) (a, b)
         return (b, loop s' (Just (t2, a, b)) xs')

-- | Process the input with ability to handle a possible preemption.
serverProcessPreempting :: Server s a b -> s -> a -> Process (s, b, Double)
serverProcessPreempting server s a =
  do pid <- processId
     t1  <- liftDynamics time
     rs  <- liftIO $ newIORef 0
     r1  <- liftIO $ newIORef t1
     h1  <- liftEvent $
            handleSignal (processPreemptionBeginning pid) $ \() ->
            do t1 <- liftDynamics time
               liftIO $ writeIORef r1 t1
               triggerSignal (serverTaskPreemptingSource server) a
     h2  <- liftEvent $
            handleSignal (processPreemptionEnding pid) $ \() ->
            do t1 <- liftIO $ readIORef r1
               t2 <- liftDynamics time
               let dt = t2 - t1
               liftIO $
                 do modifyIORef' rs (+ dt)
                    modifyIORef' (serverTotalPreemptionTimeRef server) (+ dt)
                    modifyIORef' (serverPreemptionTimeRef server) $
                      addSamplingStats dt
               triggerSignal (serverTaskReenteringSource server) a 
     let m1 =
           do (s', b) <- serverProcess server s a
              dt <- liftIO $ readIORef rs
              return (s', b, dt)
         m2 =
           liftEvent $
           do disposeEvent h1
              disposeEvent h2
     finallyProcess m1 m2

-- | Return the current state of the server.
--
-- See also 'serverStateChanged' and 'serverStateChanged_'.
serverState :: Server s a b -> Event s
serverState server =
  Event $ \p -> readIORef (serverStateRef server)
  
-- | Signal when the 'serverState' property value has changed.
serverStateChanged :: Server s a b -> Signal s
serverStateChanged server =
  mapSignalM (const $ serverState server) (serverStateChanged_ server)
  
-- | Signal when the 'serverState' property value has changed.
serverStateChanged_ :: Server s a b -> Signal ()
serverStateChanged_ server =
  mapSignal (const ()) (serverTaskProcessed server)

-- | Return the counted total time when the server was locked while awaiting the input.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverTotalInputWaitTimeChanged' and 'serverTotalInputWaitTimeChanged_'.
serverTotalInputWaitTime :: Server s a b -> Event Double
serverTotalInputWaitTime server =
  Event $ \p -> readIORef (serverTotalInputWaitTimeRef server)
  
-- | Signal when the 'serverTotalInputWaitTime' property value has changed.
serverTotalInputWaitTimeChanged :: Server s a b -> Signal Double
serverTotalInputWaitTimeChanged server =
  mapSignalM (const $ serverTotalInputWaitTime server) (serverTotalInputWaitTimeChanged_ server)
  
-- | Signal when the 'serverTotalInputWaitTime' property value has changed.
serverTotalInputWaitTimeChanged_ :: Server s a b -> Signal ()
serverTotalInputWaitTimeChanged_ server =
  mapSignal (const ()) (serverInputReceived server)

-- | Return the counted total time spent by the server while processing the tasks.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverTotalProcessingTimeChanged' and 'serverTotalProcessingTimeChanged_'.
serverTotalProcessingTime :: Server s a b -> Event Double
serverTotalProcessingTime server =
  Event $ \p -> readIORef (serverTotalProcessingTimeRef server)
  
-- | Signal when the 'serverTotalProcessingTime' property value has changed.
serverTotalProcessingTimeChanged :: Server s a b -> Signal Double
serverTotalProcessingTimeChanged server =
  mapSignalM (const $ serverTotalProcessingTime server) (serverTotalProcessingTimeChanged_ server)
  
-- | Signal when the 'serverTotalProcessingTime' property value has changed.
serverTotalProcessingTimeChanged_ :: Server s a b -> Signal ()
serverTotalProcessingTimeChanged_ server =
  mapSignal (const ()) (serverTaskProcessed server)

-- | Return the counted total time when the server was locked while trying
-- to deliver the output.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverTotalOutputWaitTimeChanged' and 'serverTotalOutputWaitTimeChanged_'.
serverTotalOutputWaitTime :: Server s a b -> Event Double
serverTotalOutputWaitTime server =
  Event $ \p -> readIORef (serverTotalOutputWaitTimeRef server)
  
-- | Signal when the 'serverTotalOutputWaitTime' property value has changed.
serverTotalOutputWaitTimeChanged :: Server s a b -> Signal Double
serverTotalOutputWaitTimeChanged server =
  mapSignalM (const $ serverTotalOutputWaitTime server) (serverTotalOutputWaitTimeChanged_ server)
  
-- | Signal when the 'serverTotalOutputWaitTime' property value has changed.
serverTotalOutputWaitTimeChanged_ :: Server s a b -> Signal ()
serverTotalOutputWaitTimeChanged_ server =
  mapSignal (const ()) (serverOutputProvided server)

-- | Return the counted total time spent by the server while it was preempted
-- waiting for the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverTotalPreemptionTimeChanged' and 'serverTotalPreemptionTimeChanged_'.
serverTotalPreemptionTime :: Server s a b -> Event Double
serverTotalPreemptionTime server =
  Event $ \p -> readIORef (serverTotalPreemptionTimeRef server)
  
-- | Signal when the 'serverTotalPreemptionTime' property value has changed.
serverTotalPreemptionTimeChanged :: Server s a b -> Signal Double
serverTotalPreemptionTimeChanged server =
  mapSignalM (const $ serverTotalPreemptionTime server) (serverTotalPreemptionTimeChanged_ server)
  
-- | Signal when the 'serverTotalPreemptionTime' property value has changed.
serverTotalPreemptionTimeChanged_ :: Server s a b -> Signal ()
serverTotalPreemptionTimeChanged_ server =
  mapSignal (const ()) (serverTaskReentering server)

-- | Return the statistics of the time when the server was locked while awaiting the input.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverInputWaitTimeChanged' and 'serverInputWaitTimeChanged_'.
serverInputWaitTime :: Server s a b -> Event (SamplingStats Double)
serverInputWaitTime server =
  Event $ \p -> readIORef (serverInputWaitTimeRef server)
  
-- | Signal when the 'serverInputWaitTime' property value has changed.
serverInputWaitTimeChanged :: Server s a b -> Signal (SamplingStats Double)
serverInputWaitTimeChanged server =
  mapSignalM (const $ serverInputWaitTime server) (serverInputWaitTimeChanged_ server)
  
-- | Signal when the 'serverInputWaitTime' property value has changed.
serverInputWaitTimeChanged_ :: Server s a b -> Signal ()
serverInputWaitTimeChanged_ server =
  mapSignal (const ()) (serverInputReceived server)

-- | Return the statistics of the time spent by the server while processing the tasks.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverProcessingTimeChanged' and 'serverProcessingTimeChanged_'.
serverProcessingTime :: Server s a b -> Event (SamplingStats Double)
serverProcessingTime server =
  Event $ \p -> readIORef (serverProcessingTimeRef server)
  
-- | Signal when the 'serverProcessingTime' property value has changed.
serverProcessingTimeChanged :: Server s a b -> Signal (SamplingStats Double)
serverProcessingTimeChanged server =
  mapSignalM (const $ serverProcessingTime server) (serverProcessingTimeChanged_ server)
  
-- | Signal when the 'serverProcessingTime' property value has changed.
serverProcessingTimeChanged_ :: Server s a b -> Signal ()
serverProcessingTimeChanged_ server =
  mapSignal (const ()) (serverTaskProcessed server)

-- | Return the statistics of the time when the server was locked while trying
-- to deliver the output. 
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverOutputWaitTimeChanged' and 'serverOutputWaitTimeChanged_'.
serverOutputWaitTime :: Server s a b -> Event (SamplingStats Double)
serverOutputWaitTime server =
  Event $ \p -> readIORef (serverOutputWaitTimeRef server)
  
-- | Signal when the 'serverOutputWaitTime' property value has changed.
serverOutputWaitTimeChanged :: Server s a b -> Signal (SamplingStats Double)
serverOutputWaitTimeChanged server =
  mapSignalM (const $ serverOutputWaitTime server) (serverOutputWaitTimeChanged_ server)
  
-- | Signal when the 'serverOutputWaitTime' property value has changed.
serverOutputWaitTimeChanged_ :: Server s a b -> Signal ()
serverOutputWaitTimeChanged_ server =
  mapSignal (const ()) (serverOutputProvided server)

-- | Return the statistics of the time spent by the server while it was preempted
-- waiting for the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverPreemptionTimeChanged' and 'serverPreemptionTimeChanged_'.
serverPreemptionTime :: Server s a b -> Event (SamplingStats Double)
serverPreemptionTime server =
  Event $ \p -> readIORef (serverPreemptionTimeRef server)
  
-- | Signal when the 'serverPreemptionTime' property value has changed.
serverPreemptionTimeChanged :: Server s a b -> Signal (SamplingStats Double)
serverPreemptionTimeChanged server =
  mapSignalM (const $ serverPreemptionTime server) (serverPreemptionTimeChanged_ server)
  
-- | Signal when the 'serverPreemptionTime' property value has changed.
serverPreemptionTimeChanged_ :: Server s a b -> Signal ()
serverPreemptionTimeChanged_ server =
  mapSignal (const ()) (serverTaskReentering server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was awaiting for the next input task.
--
-- This factor is calculated as
--
-- @
--   totalInputWaitTime \/ (totalInputWaitTime + totalProcessingTime + totalOutputWaitTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'serverInputWaitFactorChanged' and 'serverInputWaitFactorChanged_'.
serverInputWaitFactor :: Server s a b -> Event Double
serverInputWaitFactor server =
  Event $ \p ->
  do x1 <- readIORef (serverTotalInputWaitTimeRef server)
     x2 <- readIORef (serverTotalProcessingTimeRef server)
     x3 <- readIORef (serverTotalOutputWaitTimeRef server)
     x4 <- readIORef (serverTotalPreemptionTimeRef server)
     return (x1 / (x1 + x2 + x3 + x4))
  
-- | Signal when the 'serverInputWaitFactor' property value has changed.
serverInputWaitFactorChanged :: Server s a b -> Signal Double
serverInputWaitFactorChanged server =
  mapSignalM (const $ serverInputWaitFactor server) (serverInputWaitFactorChanged_ server)
  
-- | Signal when the 'serverInputWaitFactor' property value has changed.
serverInputWaitFactorChanged_ :: Server s a b -> Signal ()
serverInputWaitFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server) <>
  mapSignal (const ()) (serverTaskReentering server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was busy with direct processing its tasks.
--
-- This factor is calculated as
--
-- @
--   totalProcessingTime \/ (totalInputWaitTime + totalProcessingTime + totalOutputWaitTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'serverProcessingFactorChanged' and 'serverProcessingFactorChanged_'.
serverProcessingFactor :: Server s a b -> Event Double
serverProcessingFactor server =
  Event $ \p ->
  do x1 <- readIORef (serverTotalInputWaitTimeRef server)
     x2 <- readIORef (serverTotalProcessingTimeRef server)
     x3 <- readIORef (serverTotalOutputWaitTimeRef server)
     x4 <- readIORef (serverTotalPreemptionTimeRef server)
     return (x2 / (x1 + x2 + x3 + x4))
  
-- | Signal when the 'serverProcessingFactor' property value has changed.
serverProcessingFactorChanged :: Server s a b -> Signal Double
serverProcessingFactorChanged server =
  mapSignalM (const $ serverProcessingFactor server) (serverProcessingFactorChanged_ server)
  
-- | Signal when the 'serverProcessingFactor' property value has changed.
serverProcessingFactorChanged_ :: Server s a b -> Signal ()
serverProcessingFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server) <>
  mapSignal (const ()) (serverTaskReentering server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was locked trying to deliver the output after the task is finished.
--
-- This factor is calculated as
--
-- @
--   totalOutputWaitTime \/ (totalInputWaitTime + totalProcessingTime + totalOutputWaitTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'serverOutputWaitFactorChanged' and 'serverOutputWaitFactorChanged_'.
serverOutputWaitFactor :: Server s a b -> Event Double
serverOutputWaitFactor server =
  Event $ \p ->
  do x1 <- readIORef (serverTotalInputWaitTimeRef server)
     x2 <- readIORef (serverTotalProcessingTimeRef server)
     x3 <- readIORef (serverTotalOutputWaitTimeRef server)
     x4 <- readIORef (serverTotalPreemptionTimeRef server)
     return (x3 / (x1 + x2 + x3 + x4))
  
-- | Signal when the 'serverOutputWaitFactor' property value has changed.
serverOutputWaitFactorChanged :: Server s a b -> Signal Double
serverOutputWaitFactorChanged server =
  mapSignalM (const $ serverOutputWaitFactor server) (serverOutputWaitFactorChanged_ server)
  
-- | Signal when the 'serverOutputWaitFactor' property value has changed.
serverOutputWaitFactorChanged_ :: Server s a b -> Signal ()
serverOutputWaitFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server) <>
  mapSignal (const ()) (serverTaskReentering server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was preempted waiting for the further proceeding.
--
-- This factor is calculated as
--
-- @
--   totalPreemptionTime \/ (totalInputWaitTime + totalProcessingTime + totalOutputWaitTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'serverPreemptionFactorChanged' and 'serverPreemptionFactorChanged_'.
serverPreemptionFactor :: Server s a b -> Event Double
serverPreemptionFactor server =
  Event $ \p ->
  do x1 <- readIORef (serverTotalInputWaitTimeRef server)
     x2 <- readIORef (serverTotalProcessingTimeRef server)
     x3 <- readIORef (serverTotalOutputWaitTimeRef server)
     x4 <- readIORef (serverTotalPreemptionTimeRef server)
     return (x4 / (x1 + x2 + x3 + x4))
  
-- | Signal when the 'serverPreemptionFactor' property value has changed.
serverPreemptionFactorChanged :: Server s a b -> Signal Double
serverPreemptionFactorChanged server =
  mapSignalM (const $ serverPreemptionFactor server) (serverPreemptionFactorChanged_ server)
  
-- | Signal when the 'serverPreemptionFactor' property value has changed.
serverPreemptionFactorChanged_ :: Server s a b -> Signal ()
serverPreemptionFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server) <>
  mapSignal (const ()) (serverTaskReentering server)

-- | Raised when the server receives a new input task.
serverInputReceived :: Server s a b -> Signal a
serverInputReceived = publishSignal . serverInputReceivedSource

-- | Raised when the task processing by the server was preempted.
serverTaskPreempting :: Server s a b -> Signal a
serverTaskPreempting = publishSignal . serverTaskPreemptingSource

-- | Raised when the task processing by the server was proceeded after it has been preempeted earlier.
serverTaskReentering :: Server s a b -> Signal a
serverTaskReentering = publishSignal . serverTaskReenteringSource

-- | Raised when the server has just processed the task.
serverTaskProcessed :: Server s a b -> Signal (a, b)
serverTaskProcessed = publishSignal . serverTaskProcessedSource

-- | Raised when the server has just delivered the output.
serverOutputProvided :: Server s a b -> Signal (a, b)
serverOutputProvided = publishSignal . serverOutputProvidedSource

-- | Signal whenever any property of the server changes.
serverChanged_ :: Server s a b -> Signal ()
serverChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server) <>
  mapSignal (const ()) (serverTaskReentering server)

-- | Return the summary for the server with desciption of its
-- properties and activities using the specified indent.
serverSummary :: Server s a b -> Int -> Event ShowS
serverSummary server indent =
  Event $ \p ->
  do tx1 <- readIORef (serverTotalInputWaitTimeRef server)
     tx2 <- readIORef (serverTotalProcessingTimeRef server)
     tx3 <- readIORef (serverTotalOutputWaitTimeRef server)
     tx4 <- readIORef (serverTotalPreemptionTimeRef server)
     let xf1 = tx1 / (tx1 + tx2 + tx3 + tx4)
         xf2 = tx2 / (tx1 + tx2 + tx3 + tx4)
         xf3 = tx3 / (tx1 + tx2 + tx3 + tx4)
         xf4 = tx4 / (tx1 + tx3 + tx3 + tx4)
     xs1 <- readIORef (serverInputWaitTimeRef server)
     xs2 <- readIORef (serverProcessingTimeRef server)
     xs3 <- readIORef (serverOutputWaitTimeRef server)
     xs4 <- readIORef (serverPreemptionTimeRef server)
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "total input wait time (locked while awaiting the input) = " . shows tx1 .
       showString "\n" .
       showString tab .
       showString "total processing time = " . shows tx2 .
       showString "\n" .
       showString tab .
       showString "total output wait time (locked while delivering the output) = " . shows tx3 .
       showString "\n\n" .
       showString tab .
       showString "total preemption time = " . shows tx4 .
       showString "\n" .
       showString tab .
       showString "input wait factor (from 0 to 1) = " . shows xf1 .
       showString "\n" .
       showString tab .
       showString "processing factor (from 0 to 1) = " . shows xf2 .
       showString "\n" .
       showString tab .
       showString "output wait factor (from 0 to 1) = " . shows xf3 .
       showString "\n\n" .
       showString tab .
       showString "output preemption factor (from 0 to 1) = " . shows xf4 .
       showString "\n\n" .
       showString tab .
       showString "input wait time (locked while awaiting the input):\n\n" .
       samplingStatsSummary xs1 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "processing time:\n\n" .
       samplingStatsSummary xs2 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "output wait time (locked while delivering the output):\n\n" .
       samplingStatsSummary xs3 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "preemption time (waiting for the proceeding after preemption):\n\n" .
       samplingStatsSummary xs4 (2 + indent)
