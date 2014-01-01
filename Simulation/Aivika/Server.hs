
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
        newServerProcessor,
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
        -- * Signals
        serverInputReceived,
        serverTaskProcessed,
        serverOutputProvided) where

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

-- | Create a processor by the specified server.
--
-- You should not use more than one processor for the specified server as the processor changes
-- the state of the server and its properties when working.
newServerProcessor :: Server s a b -> Processor a b
newServerProcessor server =
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
serverState :: Server s a b -> Signalable s
serverState server =
  let read = Event $ \p -> readIORef (serverStateRef server)
  in Signalable { readSignalable = read,
                  signalableChanged_ =
                    mapSignal (const ()) (serverTaskProcessed server) }

-- | Return the counted total time spent by the server in awaiting the input.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
serverTotalInputTime :: Server s a b -> Signalable Double
serverTotalInputTime server =
  let read = Event $ \p -> readIORef (serverTotalInputTimeRef server)
  in Signalable { readSignalable = read,
                  signalableChanged_ =
                    mapSignal (const ()) (serverInputReceived server) }

-- | Return the counted total time spent by the server to process all tasks.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
serverTotalProcessingTime :: Server s a b -> Signalable Double
serverTotalProcessingTime server =
  let read = Event $ \p -> readIORef (serverTotalProcessingTimeRef server)
  in Signalable { readSignalable = read,
                  signalableChanged_ =
                    mapSignal (const ()) (serverTaskProcessed server) }

-- | Return the counted total time when the server was in the lock state trying
-- to deliver the output.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
serverTotalOutputTime :: Server s a b -> Signalable Double
serverTotalOutputTime server =
  let read = Event $ \p -> readIORef (serverTotalOutputTimeRef server)
  in Signalable { readSignalable = read,
                  signalableChanged_ =
                    mapSignal (const ()) (serverOutputProvided server) }

-- | Return the statistics of the time spent by the server in awaiting the input.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
serverInputTime :: Server s a b -> Signalable (SamplingStats Double)
serverInputTime server =
  let read = Event $ \p -> readIORef (serverInputTimeRef server)
  in Signalable { readSignalable = read,
                  signalableChanged_ =
                    mapSignal (const ()) (serverInputReceived server) }

-- | Return the statistics of the time spent by the server to process the tasks.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
serverProcessingTime :: Server s a b -> Signalable (SamplingStats Double)
serverProcessingTime server =
  let read = Event $ \p -> readIORef (serverProcessingTimeRef server)
  in Signalable { readSignalable = read,
                  signalableChanged_ =
                    mapSignal (const ()) (serverTaskProcessed server) }

-- | Return the statistics of the time when the server was in the lock state trying
-- to deliver the output. 
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
serverOutputTime :: Server s a b -> Signalable (SamplingStats Double)
serverOutputTime server =
  let read = Event $ \p -> readIORef (serverOutputTimeRef server)
  in Signalable { readSignalable = read,
                  signalableChanged_ =
                    mapSignal (const ()) (serverOutputProvided server) }

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
serverInputTimeFactor :: Server s a b -> Signalable Double
serverInputTimeFactor server =
  let read =
        Event $ \p ->
        do x1 <- readIORef (serverTotalInputTimeRef server)
           x2 <- readIORef (serverTotalProcessingTimeRef server)
           x3 <- readIORef (serverTotalOutputTimeRef server)
           return (x1 / (x1 + x2 + x3))
      signal =
        mapSignal (const ()) (serverInputReceived server) <>
        mapSignal (const ()) (serverTaskProcessed server) <>
        mapSignal (const ()) (serverOutputProvided server)
  in Signalable { readSignalable = read,
                  signalableChanged_ = signal }

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
serverProcessingTimeFactor :: Server s a b -> Signalable Double
serverProcessingTimeFactor server =
  let read =
        Event $ \p ->
        do x1 <- readIORef (serverTotalInputTimeRef server)
           x2 <- readIORef (serverTotalProcessingTimeRef server)
           x3 <- readIORef (serverTotalOutputTimeRef server)
           return (x2 / (x1 + x2 + x3))
      signal =
        mapSignal (const ()) (serverInputReceived server) <>
        mapSignal (const ()) (serverTaskProcessed server) <>
        mapSignal (const ()) (serverOutputProvided server)
  in Signalable { readSignalable = read,
                  signalableChanged_ = signal }

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
serverOutputTimeFactor :: Server s a b -> Signalable Double
serverOutputTimeFactor server =
  let read =
        Event $ \p ->
        do x1 <- readIORef (serverTotalInputTimeRef server)
           x2 <- readIORef (serverTotalProcessingTimeRef server)
           x3 <- readIORef (serverTotalOutputTimeRef server)
           return (x3 / (x1 + x2 + x3))
      signal =
        mapSignal (const ()) (serverInputReceived server) <>
        mapSignal (const ()) (serverTaskProcessed server) <>
        mapSignal (const ()) (serverOutputProvided server)
  in Signalable { readSignalable = read,
                  signalableChanged_ = signal }

-- | Raised when the server receives a new input task.
serverInputReceived :: Server s a b -> Signal a
serverInputReceived = publishSignal . serverInputReceivedSource

-- | Raised when the server has just processed the task.
serverTaskProcessed :: Server s a b -> Signal (a, b)
serverTaskProcessed = publishSignal . serverTaskProcessedSource

-- | Raised when the server has just delivered the output.
serverOutputProvided :: Server s a b -> Signal (a, b)
serverOutputProvided = publishSignal . serverOutputProvidedSource

-- | Return the summary for the server with desciption of its
-- properties and activities using the specified indent.
serverSummary :: Server s a b -> Int -> Signalable ShowS
serverSummary server indent =
  let read =
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
             showString "total input time (in awaiting the input) = " . shows tx1 .
             showString "\n" .
             showString tab .
             showString "total processing time = " . shows tx2 .
             showString "\n" .
             showString tab .
             showString "total output time (to deliver the output) = " . shows tx3 .
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
             showString "input time:\n\n" .
             samplingStatsSummary xs1 (2 + indent) .
             showString "\n\n" .
             showString tab .
             showString "processing time:\n\n" .
             samplingStatsSummary xs2 (2 + indent) .
             showString "\n\n" .
             showString tab .
             showString "output time:\n\n" .
             samplingStatsSummary xs3 (2 + indent)
      signal =
        mapSignal (const ()) (serverInputReceived server) <>
        mapSignal (const ()) (serverTaskProcessed server) <>
        mapSignal (const ()) (serverOutputProvided server)
  in Signalable { readSignalable = read,
                  signalableChanged_ = signal }
