
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
        newServerWithState,
        -- * Processing
        serverProcessor,
        -- * Server Properties and Activities
        serverInitState,
        serverState,
        serverTotalInputTime,
        serverTotalProcessingTime,
        serverTotalOutputTime,
        serverInputTime,
        serverProcessingTime,
        serverOutputTime,
        serverInputTimeFactor,
        serverProcessingTimeFactor,
        serverOutputTimeFactor,
        -- * Summary
        serverSummary,
        -- * Derived Signals for Properties
        serverStateChanged,
        serverStateChanged_,
        serverTotalInputTimeChanged,
        serverTotalInputTimeChanged_,
        serverTotalProcessingTimeChanged,
        serverTotalProcessingTimeChanged_,
        serverTotalOutputTimeChanged,
        serverTotalOutputTimeChanged_,
        serverInputTimeChanged,
        serverInputTimeChanged_,
        serverProcessingTimeChanged,
        serverProcessingTimeChanged_,
        serverOutputTimeChanged,
        serverOutputTimeChanged_,
        serverInputTimeFactorChanged,
        serverInputTimeFactorChanged_,
        serverProcessingTimeFactorChanged,
        serverProcessingTimeFactorChanged_,
        serverOutputTimeFactorChanged,
        serverOutputTimeFactorChanged_,
        -- * Basic Signals
        serverInputReceived,
        serverTaskProcessed,
        serverOutputProvided,
        -- * Overall Signal
        serverChanged_) where

import Data.IORef
import Data.Monoid
import Control.Monad.Trans

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
           serverProcess :: (s, a) -> Process (s, b),
           -- ^ Provide @b@ by specified @a@.
           serverTotalInputTimeRef :: IORef Double,
           -- ^ The counted total time spent in awating the input.
           serverTotalProcessingTimeRef :: IORef Double,
           -- ^ The counted total time spent to process the input and prepare the output.
           serverTotalOutputTimeRef :: IORef Double,
           -- ^ The counted total time spent for delivering the output.
           serverInputTimeRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the time spent in awaiting the input.
           serverProcessingTimeRef :: IORef (SamplingStats Double),
           -- ^ The statistics for the time spent to process the input and prepare the output.
           serverOutputTimeRef :: IORef (SamplingStats Double),
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
  newServerWithState () $ \(s, a) ->
  do b <- provide a
     return (s, b)

-- | Create a new server that can provide output @b@ by input @a@
-- starting from state @s@. Also it returns the corresponded processor
-- that being applied updates the server state.
newServerWithState :: s
                      -- ^ the initial state
                      -> ((s, a) -> Process (s, b))
                      -- ^ provide an output by the specified input
                      -- and update the state 
                      -> Simulation (Server s a b)
newServerWithState state provide =
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
                           serverTotalInputTimeRef = r1,
                           serverTotalProcessingTimeRef = r2,
                           serverTotalOutputTimeRef = r3,
                           serverInputTimeRef = r4,
                           serverProcessingTimeRef = r5,
                           serverOutputTimeRef = r6,
                           serverInputReceivedSource = s1,
                           serverTaskProcessedSource = s2,
                           serverOutputProvidedSource = s3 }
     return server

-- | Return a processor for the specified server.
--
-- The processor updates the internal state of the server. The usual case is when 
-- the processor is applied only once in a chain of data processing. Otherwise; 
-- every time the processor is used, the state of the server changes. 
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
                    do modifyIORef (serverTotalOutputTimeRef server) (+ (t0 - t'))
                       modifyIORef (serverOutputTimeRef server) $
                         addSamplingStats (t0 - t')
                  triggerSignal (serverOutputProvidedSource server) (a', b')
         -- get input
         (a, xs') <- runStream xs
         t1 <- liftDynamics time
         liftEvent $
           do liftIO $
                do modifyIORef (serverTotalInputTimeRef server) (+ (t1 - t0))
                   modifyIORef (serverInputTimeRef server) $
                     addSamplingStats (t1 - t0)
              triggerSignal (serverInputReceivedSource server) a
         -- provide the service
         (s', b) <- serverProcess server (s, a)
         t2 <- liftDynamics time
         liftEvent $
           do liftIO $
                do writeIORef (serverStateRef server) s'
                   modifyIORef (serverTotalProcessingTimeRef server) (+ (t2 - t1))
                   modifyIORef (serverProcessingTimeRef server) $
                     addSamplingStats (t2 - t1)
              triggerSignal (serverTaskProcessedSource server) (a, b)
         return (b, loop s' (Just $ (t2, a, b)) xs')

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
-- See also 'serverTotalInputTimeChanged' and 'serverTotalInputTimeChanged_'.
serverTotalInputTime :: Server s a b -> Event Double
serverTotalInputTime server =
  Event $ \p -> readIORef (serverTotalInputTimeRef server)
  
-- | Signal when the 'serverTotalInputTime' property value has changed.
serverTotalInputTimeChanged :: Server s a b -> Signal Double
serverTotalInputTimeChanged server =
  mapSignalM (const $ serverTotalInputTime server) (serverTotalInputTimeChanged_ server)
  
-- | Signal when the 'serverTotalInputTime' property value has changed.
serverTotalInputTimeChanged_ :: Server s a b -> Signal ()
serverTotalInputTimeChanged_ server =
  mapSignal (const ()) (serverInputReceived server)

-- | Return the counted total time spent by the server to process the tasks.
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
-- See also 'serverTotalOutputTimeChanged' and 'serverTotalOutputTimeChanged_'.
serverTotalOutputTime :: Server s a b -> Event Double
serverTotalOutputTime server =
  Event $ \p -> readIORef (serverTotalOutputTimeRef server)
  
-- | Signal when the 'serverTotalOutputTime' property value has changed.
serverTotalOutputTimeChanged :: Server s a b -> Signal Double
serverTotalOutputTimeChanged server =
  mapSignalM (const $ serverTotalOutputTime server) (serverTotalOutputTimeChanged_ server)
  
-- | Signal when the 'serverTotalOutputTime' property value has changed.
serverTotalOutputTimeChanged_ :: Server s a b -> Signal ()
serverTotalOutputTimeChanged_ server =
  mapSignal (const ()) (serverOutputProvided server)

-- | Return the statistics of the time when the server was locked while awaiting the input.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverInputTimeChanged' and 'serverInputTimeChanged_'.
serverInputTime :: Server s a b -> Event (SamplingStats Double)
serverInputTime server =
  Event $ \p -> readIORef (serverInputTimeRef server)
  
-- | Signal when the 'serverInputTime' property value has changed.
serverInputTimeChanged :: Server s a b -> Signal (SamplingStats Double)
serverInputTimeChanged server =
  mapSignalM (const $ serverInputTime server) (serverInputTimeChanged_ server)
  
-- | Signal when the 'serverInputTime' property value has changed.
serverInputTimeChanged_ :: Server s a b -> Signal ()
serverInputTimeChanged_ server =
  mapSignal (const ()) (serverInputReceived server)

-- | Return the statistics of the time spent by the server to process the tasks.
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
-- See also 'serverOutputTimeChanged' and 'serverOutputTimeChanged_'.
serverOutputTime :: Server s a b -> Event (SamplingStats Double)
serverOutputTime server =
  Event $ \p -> readIORef (serverOutputTimeRef server)
  
-- | Signal when the 'serverOutputTime' property value has changed.
serverOutputTimeChanged :: Server s a b -> Signal (SamplingStats Double)
serverOutputTimeChanged server =
  mapSignalM (const $ serverOutputTime server) (serverOutputTimeChanged_ server)
  
-- | Signal when the 'serverOutputTime' property value has changed.
serverOutputTimeChanged_ :: Server s a b -> Signal ()
serverOutputTimeChanged_ server =
  mapSignal (const ()) (serverOutputProvided server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was awaiting for the next input task.
--
-- This factor is calculated as
--
-- @
--   totalInputTime \/ (totalInputTime + totalProcessingTime + totalOutputTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'serverInputTimeFactorChanged' and 'serverInputTimeFactorChanged_'.
serverInputTimeFactor :: Server s a b -> Event Double
serverInputTimeFactor server =
  Event $ \p ->
  do x1 <- readIORef (serverTotalInputTimeRef server)
     x2 <- readIORef (serverTotalProcessingTimeRef server)
     x3 <- readIORef (serverTotalOutputTimeRef server)
     return (x1 / (x1 + x2 + x3))
  
-- | Signal when the 'serverInputTimeFactor' property value has changed.
serverInputTimeFactorChanged :: Server s a b -> Signal Double
serverInputTimeFactorChanged server =
  mapSignalM (const $ serverInputTimeFactor server) (serverInputTimeFactorChanged_ server)
  
-- | Signal when the 'serverInputTimeFactor' property value has changed.
serverInputTimeFactorChanged_ :: Server s a b -> Signal ()
serverInputTimeFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was busy with direct processing its tasks.
--
-- This factor is calculated as
--
-- @
--   totalProcessingTime \/ (totalInputTime + totalProcessingTime + totalOutputTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'serverProcessingTimeFactorChanged' and 'serverProcessingTimeFactorChanged_'.
serverProcessingTimeFactor :: Server s a b -> Event Double
serverProcessingTimeFactor server =
  Event $ \p ->
  do x1 <- readIORef (serverTotalInputTimeRef server)
     x2 <- readIORef (serverTotalProcessingTimeRef server)
     x3 <- readIORef (serverTotalOutputTimeRef server)
     return (x2 / (x1 + x2 + x3))
  
-- | Signal when the 'serverProcessingTimeFactor' property value has changed.
serverProcessingTimeFactorChanged :: Server s a b -> Signal Double
serverProcessingTimeFactorChanged server =
  mapSignalM (const $ serverProcessingTimeFactor server) (serverProcessingTimeFactorChanged_ server)
  
-- | Signal when the 'serverProcessingTimeFactor' property value has changed.
serverProcessingTimeFactorChanged_ :: Server s a b -> Signal ()
serverProcessingTimeFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was locked trying to deliver the output after the task is finished.
--
-- This factor is calculated as
--
-- @
--   totalOutputTime \/ (totalInputTime + totalProcessingTime + totalOutputTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'serverOutputTimeFactorChanged' and 'serverOutputTimeFactorChanged_'.
serverOutputTimeFactor :: Server s a b -> Event Double
serverOutputTimeFactor server =
  Event $ \p ->
  do x1 <- readIORef (serverTotalInputTimeRef server)
     x2 <- readIORef (serverTotalProcessingTimeRef server)
     x3 <- readIORef (serverTotalOutputTimeRef server)
     return (x3 / (x1 + x2 + x3))
  
-- | Signal when the 'serverOutputTimeFactor' property value has changed.
serverOutputTimeFactorChanged :: Server s a b -> Signal Double
serverOutputTimeFactorChanged server =
  mapSignalM (const $ serverOutputTimeFactor server) (serverOutputTimeFactorChanged_ server)
  
-- | Signal when the 'serverOutputTimeFactor' property value has changed.
serverOutputTimeFactorChanged_ :: Server s a b -> Signal ()
serverOutputTimeFactorChanged_ server =
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
  do tx1 <- readIORef (serverTotalInputTimeRef server)
     tx2 <- readIORef (serverTotalProcessingTimeRef server)
     tx3 <- readIORef (serverTotalOutputTimeRef server)
     let xf1 = tx1 / (tx1 + tx2 + tx3)
         xf2 = tx2 / (tx1 + tx2 + tx3)
         xf3 = tx3 / (tx1 + tx2 + tx3)
     xs1 <- readIORef (serverInputTimeRef server)
     xs2 <- readIORef (serverProcessingTimeRef server)
     xs3 <- readIORef (serverOutputTimeRef server)
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "total input time (locked while awaiting the input) = " . shows tx1 .
       showString "\n" .
       showString tab .
       showString "total processing time = " . shows tx2 .
       showString "\n" .
       showString tab .
       showString "total output time (locked while delivering the output) = " . shows tx3 .
       showString "\n\n" .
       showString tab .
       showString "input time factor (from 0 to 1) = " . shows xf1 .
       showString "\n" .
       showString tab .
       showString "processing time factor (from 0 to 1) = " . shows xf2 .
       showString "\n" .
       showString tab .
       showString "output time factor (from 0 to 1) = " . shows xf3 .
       showString "\n\n" .
       showString tab .
       showString "input time (locked while awaiting the input):\n\n" .
       samplingStatsSummary xs1 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "processing time:\n\n" .
       samplingStatsSummary xs2 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "output time (locked while delivering the output):\n\n" .
       samplingStatsSummary xs3 (2 + indent)
