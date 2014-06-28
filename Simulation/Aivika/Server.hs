
-- |
-- Module     : Simulation.Aivika.Server
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- It models the server that prodives a service.
module Simulation.Aivika.Server
       (-- * Server
        Server,
        newServer,
        newStateServer,
        -- * Processing
        serverProcessor,
        -- * Server Properties and Activities
        serverInitState,
        serverState,
        serverTotalInputWaitTime,
        serverTotalProcessingTime,
        serverTotalOutputWaitTime,
        serverInputWaitTime,
        serverProcessingTime,
        serverOutputWaitTime,
        serverInputWaitFactor,
        serverProcessingFactor,
        serverOutputWaitFactor,
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
        serverInputWaitTimeChanged,
        serverInputWaitTimeChanged_,
        serverProcessingTimeChanged,
        serverProcessingTimeChanged_,
        serverOutputWaitTimeChanged,
        serverOutputWaitTimeChanged_,
        serverInputWaitFactorChanged,
        serverInputWaitFactorChanged_,
        serverProcessingFactorChanged,
        serverProcessingFactorChanged_,
        serverOutputWaitFactorChanged,
        serverOutputWaitFactorChanged_,
        -- * Basic Signals
        serverInputReceived,
        serverTaskProcessed,
        serverOutputProvided,
        -- * Overall Signal
        serverChanged_) where

import Data.IORef
import Data.Monoid

import Control.Monad.Trans
import Control.Arrow

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Signal
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
           serverTotalInputWaitTimeRef :: IORef Double,
           -- ^ The counted total time spent in awating the input.
           serverTotalProcessingTimeRef :: IORef Double,
           -- ^ The counted total time spent to process the input and prepare the output.
           serverTotalOutputWaitTimeRef :: IORef Double,
           -- ^ The counted total time spent for delivering the output.
           serverInputWaitTimeRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the time spent in awaiting the input.
           serverProcessingTimeRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the time spent to process the input and prepare the output.
           serverOutputWaitTimeRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the time spent for delivering the output.
           serverInputReceivedSource :: SignalSource a,
           -- ^ A signal raised when the server recieves a new input to process.
           serverTaskProcessedSource :: SignalSource (a, b),
           -- ^ A signal raised when the input is processed and
           -- the output is prepared for deliverying.
           serverOutputProvidedSource :: SignalSource (a, b)
           -- ^ A signal raised when the server has supplied the output.
         }

-- | Create a new server that can provide output @b@ by input @a@.
-- Also it returns the corresponded processor that being applied
-- updates the server state.
newServer :: (a -> Process b)
             -- ^ provide an output by the specified input
             -> Simulation (Server () a b)
newServer provide =
  flip newStateServer () $ \s a ->
  do b <- provide a
     return (s, b)

-- | Create a new server that can provide output @b@ by input @a@
-- starting from state @s@. Also it returns the corresponded processor
-- that being applied updates the server state.
newStateServer :: (s -> a -> Process (s, b))
                  -- ^ provide a new state and output by the specified 
                  -- old state and input
                  -> s
                  -- ^ the initial state
                  -> Simulation (Server s a b)
newStateServer provide state =
  do r0 <- liftIO $ newIORef state
     r1 <- liftIO $ newIORef 0
     r2 <- liftIO $ newIORef 0
     r3 <- liftIO $ newIORef 0
     r4 <- liftIO $ newIORef emptySamplingStats
     r5 <- liftIO $ newIORef emptySamplingStats
     r6 <- liftIO $ newIORef emptySamplingStats
     s1 <- newSignalSource
     s2 <- newSignalSource
     s3 <- newSignalSource
     let server = Server { serverInitState = state,
                           serverStateRef = r0,
                           serverProcess = provide,
                           serverTotalInputWaitTimeRef = r1,
                           serverTotalProcessingTimeRef = r2,
                           serverTotalOutputWaitTimeRef = r3,
                           serverInputWaitTimeRef = r4,
                           serverProcessingTimeRef = r5,
                           serverOutputWaitTimeRef = r6,
                           serverInputReceivedSource = s1,
                           serverTaskProcessedSource = s2,
                           serverOutputProvidedSource = s3 }
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
-- should separate them with help of the 'prefetchProcessor' that plays a role
-- of a small one-place buffer in that case.
--
-- The queue processors usually have the prefetching capabilities per se, where
-- the items are already stored in the queue. Therefore, the server processor
-- should not be prefetched if it is connected directly with the queue processor.
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
         (s', b) <- serverProcess server s a
         t2 <- liftDynamics time
         liftEvent $
           do liftIO $
                do writeIORef (serverStateRef server) $! s'
                   modifyIORef' (serverTotalProcessingTimeRef server) (+ (t2 - t1))
                   modifyIORef' (serverProcessingTimeRef server) $
                     addSamplingStats (t2 - t1)
              triggerSignal (serverTaskProcessedSource server) (a, b)
         return (b, loop s' (Just (t2, a, b)) xs')

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

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was awaiting for the next input task.
--
-- This factor is calculated as
--
-- @
--   totalInputWaitTime \/ (totalInputWaitTime + totalProcessingTime + totalOutputWaitTime)
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
     return (x1 / (x1 + x2 + x3))
  
-- | Signal when the 'serverInputWaitFactor' property value has changed.
serverInputWaitFactorChanged :: Server s a b -> Signal Double
serverInputWaitFactorChanged server =
  mapSignalM (const $ serverInputWaitFactor server) (serverInputWaitFactorChanged_ server)
  
-- | Signal when the 'serverInputWaitFactor' property value has changed.
serverInputWaitFactorChanged_ :: Server s a b -> Signal ()
serverInputWaitFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was busy with direct processing its tasks.
--
-- This factor is calculated as
--
-- @
--   totalProcessingTime \/ (totalInputWaitTime + totalProcessingTime + totalOutputWaitTime)
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
     return (x2 / (x1 + x2 + x3))
  
-- | Signal when the 'serverProcessingFactor' property value has changed.
serverProcessingFactorChanged :: Server s a b -> Signal Double
serverProcessingFactorChanged server =
  mapSignalM (const $ serverProcessingFactor server) (serverProcessingFactorChanged_ server)
  
-- | Signal when the 'serverProcessingFactor' property value has changed.
serverProcessingFactorChanged_ :: Server s a b -> Signal ()
serverProcessingFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was locked trying to deliver the output after the task is finished.
--
-- This factor is calculated as
--
-- @
--   totalOutputWaitTime \/ (totalInputWaitTime + totalProcessingTime + totalOutputWaitTime)
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
     return (x3 / (x1 + x2 + x3))
  
-- | Signal when the 'serverOutputWaitFactor' property value has changed.
serverOutputWaitFactorChanged :: Server s a b -> Signal Double
serverOutputWaitFactorChanged server =
  mapSignalM (const $ serverOutputWaitFactor server) (serverOutputWaitFactorChanged_ server)
  
-- | Signal when the 'serverOutputWaitFactor' property value has changed.
serverOutputWaitFactorChanged_ :: Server s a b -> Signal ()
serverOutputWaitFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server)

-- | Raised when the server receives a new input task.
serverInputReceived :: Server s a b -> Signal a
serverInputReceived = publishSignal . serverInputReceivedSource

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
  mapSignal (const ()) (serverOutputProvided server)

-- | Return the summary for the server with desciption of its
-- properties and activities using the specified indent.
serverSummary :: Server s a b -> Int -> Event ShowS
serverSummary server indent =
  Event $ \p ->
  do tx1 <- readIORef (serverTotalInputWaitTimeRef server)
     tx2 <- readIORef (serverTotalProcessingTimeRef server)
     tx3 <- readIORef (serverTotalOutputWaitTimeRef server)
     let xf1 = tx1 / (tx1 + tx2 + tx3)
         xf2 = tx2 / (tx1 + tx2 + tx3)
         xf3 = tx3 / (tx1 + tx2 + tx3)
     xs1 <- readIORef (serverInputWaitTimeRef server)
     xs2 <- readIORef (serverProcessingTimeRef server)
     xs3 <- readIORef (serverOutputWaitTimeRef server)
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
       showString "input wait factor (from 0 to 1) = " . shows xf1 .
       showString "\n" .
       showString tab .
       showString "processing factor (from 0 to 1) = " . shows xf2 .
       showString "\n" .
       showString tab .
       showString "output wait factor (from 0 to 1) = " . shows xf3 .
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
       samplingStatsSummary xs3 (2 + indent)
