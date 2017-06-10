
-- |
-- Module     : Simulation.Aivika.Operation
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It defines a stateless activity, some simplification of 'Server' and 'Activity'.
module Simulation.Aivika.Operation
       (-- * Operation
        Operation,
        newOperation,
        newPreemptibleOperation,
        -- * Processing
        operationProcess,
        -- * Operation Properties
        operationTotalUtilisationTime,
        operationTotalPreemptionTime,
        operationUtilisationTime,
        operationPreemptionTime,
        operationUtilisationFactor,
        operationPreemptionFactor,
        -- * Statistics Reset
        resetOperation,
        -- * Summary
        operationSummary,
        -- * Derived Signals for Properties
        operationTotalUtilisationTimeChanged,
        operationTotalUtilisationTimeChanged_,
        operationTotalPreemptionTimeChanged,
        operationTotalPreemptionTimeChanged_,
        operationUtilisationTimeChanged,
        operationUtilisationTimeChanged_,
        operationPreemptionTimeChanged,
        operationPreemptionTimeChanged_,
        operationUtilisationFactorChanged,
        operationUtilisationFactorChanged_,
        operationPreemptionFactorChanged,
        operationPreemptionFactorChanged_,
        -- * Basic Signals
        operationUtilising,
        operationUtilised,
        operationPreemptionBeginning,
        operationPreemptionEnding,
        -- * Overall Signal
        operationChanged_) where

import Data.IORef
import Data.Monoid

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Signal
import Simulation.Aivika.Cont
import Simulation.Aivika.Process
import Simulation.Aivika.Activity
import Simulation.Aivika.Server
import Simulation.Aivika.Statistics

-- | Like 'Server' it models an activity that takes @a@ and provides @b@.
-- But unlike the former this kind of activity has no state. Also it is destined
-- to be used within 'Process' computations.
data Operation a b =
  Operation { operationInitProcess :: a -> Process b,
              -- ^ Provide @b@ by specified @a@.
              operationProcessPreemptible :: Bool,
              -- ^ Whether the process is preemptible.
              operationStartTimeRef :: IORef Double,
              -- ^ The start time of creating the operation.
              operationLastTimeRef :: IORef Double,
              -- ^ The last time of utilising the operation activity.
              operationTotalUtilisationTimeRef :: IORef Double,
              -- ^ The counted total time of utilising the activity.
              operationTotalPreemptionTimeRef :: IORef Double,
              -- ^ The counted total time when the activity was preempted. 
              operationUtilisationTimeRef :: IORef (SamplingStats Double),
              -- ^ The statistics for the utilisation time.
              operationPreemptionTimeRef :: IORef (SamplingStats Double),
              -- ^ The statistics for the time when the activity was preempted.
              operationUtilisingSource :: SignalSource a,
              -- ^ A signal raised when starting to utilise the activity.
              operationUtilisedSource :: SignalSource (a, b),
              -- ^ A signal raised when the activity has been utilised.
              operationPreemptionBeginningSource :: SignalSource a,
              -- ^ A signal raised when the utilisation was preempted.
              operationPreemptionEndingSource :: SignalSource a
              -- ^ A signal raised when the utilisation was proceeded after it had been preempted earlier.
           }

-- | Create a new operation that can provide output @b@ by input @a@.
--
-- By default, it is assumed that the activity utilisation cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newOperation :: (a -> Process b)
                -- ^ provide an output by the specified input
                -> Event (Operation a b)
newOperation = newPreemptibleOperation False

-- | Create a new operation that can provide output @b@ by input @a@.
newPreemptibleOperation :: Bool
                           -- ^ whether the activity can be preempted
                           -> (a -> Process b)
                           -- ^ provide an output by the specified input
                           -> Event (Operation a b)
newPreemptibleOperation preemptible provide =
  do t0 <- liftDynamics time
     r' <- liftIO $ newIORef t0
     r0 <- liftIO $ newIORef t0
     r1 <- liftIO $ newIORef 0
     r2 <- liftIO $ newIORef 0
     r3 <- liftIO $ newIORef emptySamplingStats
     r4 <- liftIO $ newIORef emptySamplingStats
     s1 <- liftSimulation newSignalSource
     s2 <- liftSimulation newSignalSource
     s3 <- liftSimulation newSignalSource
     s4 <- liftSimulation newSignalSource
     return Operation { operationInitProcess = provide,
                        operationProcessPreemptible = preemptible,
                        operationStartTimeRef = r',
                        operationLastTimeRef = r0,
                        operationTotalUtilisationTimeRef = r1,
                        operationTotalPreemptionTimeRef = r2,
                        operationUtilisationTimeRef = r3,
                        operationPreemptionTimeRef = r4,
                        operationUtilisingSource = s1,
                        operationUtilisedSource = s2,
                        operationPreemptionBeginningSource = s3,
                        operationPreemptionEndingSource = s4 }

-- | Return a computation for the specified operation. It updates internal counters.
--
-- The computation can be used only within one process at any time.
operationProcess :: Operation a b -> a -> Process b
operationProcess op a =
  do t0 <- liftDynamics time
     liftEvent $
       triggerSignal (operationUtilisingSource op) a
     -- utilise the activity
     (b, dt) <- if operationProcessPreemptible op
                then operationProcessPreempting op a
                else do b <- operationInitProcess op a
                        return (b, 0)
     t1 <- liftDynamics time
     liftEvent $
       do liftIO $
            do modifyIORef' (operationTotalUtilisationTimeRef op) (+ (t1 - t0 - dt))
               modifyIORef' (operationUtilisationTimeRef op) $
                 addSamplingStats (t1 - t0 - dt)
               writeIORef (operationLastTimeRef op) t1
          triggerSignal (operationUtilisedSource op) (a, b)
     return b

-- | Process the input with ability to handle a possible preemption.
operationProcessPreempting :: Operation a b -> a -> Process (b, Double)
operationProcessPreempting op a =
  do pid <- processId
     t0  <- liftDynamics time
     rs  <- liftIO $ newIORef 0
     r0  <- liftIO $ newIORef t0
     h1  <- liftEvent $
            handleSignal (processPreemptionBeginning pid) $ \() ->
            do t0 <- liftDynamics time
               liftIO $ writeIORef r0 t0
               triggerSignal (operationPreemptionBeginningSource op) a
     h2  <- liftEvent $
            handleSignal (processPreemptionEnding pid) $ \() ->
            do t0 <- liftIO $ readIORef r0
               t1 <- liftDynamics time
               let dt = t1 - t0
               liftIO $
                 do modifyIORef' rs (+ dt)
                    modifyIORef' (operationTotalPreemptionTimeRef op) (+ dt)
                    modifyIORef' (operationPreemptionTimeRef op) $
                      addSamplingStats dt
                    writeIORef (operationLastTimeRef op) t1
               triggerSignal (operationPreemptionEndingSource op) a 
     let m1 =
           do b <- operationInitProcess op a
              dt <- liftIO $ readIORef rs
              return (b, dt)
         m2 =
           liftEvent $
           do disposeEvent h1
              disposeEvent h2
     finallyProcess m1 m2

-- | Return the counted total time when the operation activity was utilised.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationTotalUtilisationTimeChanged' and 'operationTotalUtilisationTimeChanged_'.
operationTotalUtilisationTime :: Operation a b -> Event Double
operationTotalUtilisationTime op =
  Event $ \p -> readIORef (operationTotalUtilisationTimeRef op)
  
-- | Signal when the 'operationTotalUtilisationTime' property value has changed.
operationTotalUtilisationTimeChanged :: Operation a b -> Signal Double
operationTotalUtilisationTimeChanged op =
  mapSignalM (const $ operationTotalUtilisationTime op) (operationTotalUtilisationTimeChanged_ op)
  
-- | Signal when the 'operationTotalUtilisationTime' property value has changed.
operationTotalUtilisationTimeChanged_ :: Operation a b -> Signal ()
operationTotalUtilisationTimeChanged_ op =
  mapSignal (const ()) (operationUtilised op)

-- | Return the counted total time when the operation activity was preemted waiting for
-- the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationTotalPreemptionTimeChanged' and 'operationTotalPreemptionTimeChanged_'.
operationTotalPreemptionTime :: Operation a b -> Event Double
operationTotalPreemptionTime op =
  Event $ \p -> readIORef (operationTotalPreemptionTimeRef op)
  
-- | Signal when the 'operationTotalPreemptionTime' property value has changed.
operationTotalPreemptionTimeChanged :: Operation a b -> Signal Double
operationTotalPreemptionTimeChanged op =
  mapSignalM (const $ operationTotalPreemptionTime op) (operationTotalPreemptionTimeChanged_ op)
  
-- | Signal when the 'operationTotalPreemptionTime' property value has changed.
operationTotalPreemptionTimeChanged_ :: Operation a b -> Signal ()
operationTotalPreemptionTimeChanged_ op =
  mapSignal (const ()) (operationPreemptionEnding op)

-- | Return the statistics for the time when the operation activity was utilised.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationUtilisationTimeChanged' and 'operationUtilisationTimeChanged_'.
operationUtilisationTime :: Operation a b -> Event (SamplingStats Double)
operationUtilisationTime op =
  Event $ \p -> readIORef (operationUtilisationTimeRef op)
  
-- | Signal when the 'operationUtilisationTime' property value has changed.
operationUtilisationTimeChanged :: Operation a b -> Signal (SamplingStats Double)
operationUtilisationTimeChanged op =
  mapSignalM (const $ operationUtilisationTime op) (operationUtilisationTimeChanged_ op)
  
-- | Signal when the 'operationUtilisationTime' property value has changed.
operationUtilisationTimeChanged_ :: Operation a b -> Signal ()
operationUtilisationTimeChanged_ op =
  mapSignal (const ()) (operationUtilised op)

-- | Return the statistics for the time when the operation activity was preempted
-- waiting for the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationPreemptionTimeChanged' and 'operationPreemptionTimeChanged_'.
operationPreemptionTime :: Operation a b -> Event (SamplingStats Double)
operationPreemptionTime op =
  Event $ \p -> readIORef (operationPreemptionTimeRef op)
  
-- | Signal when the 'operationPreemptionTime' property value has changed.
operationPreemptionTimeChanged :: Operation a b -> Signal (SamplingStats Double)
operationPreemptionTimeChanged op =
  mapSignalM (const $ operationPreemptionTime op) (operationPreemptionTimeChanged_ op)
  
-- | Signal when the 'operationPreemptionTime' property value has changed.
operationPreemptionTimeChanged_ :: Operation a b -> Signal ()
operationPreemptionTimeChanged_ op =
  mapSignal (const ()) (operationPreemptionEnding op)
  
-- | It returns the factor changing from 0 to 1, which estimates how often
-- the operation activity was utilised since the time of creating the operation.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationUtilisationFactorChanged' and 'operationUtilisationFactorChanged_'.
operationUtilisationFactor :: Operation a b -> Event Double
operationUtilisationFactor op =
  Event $ \p ->
  do t0 <- readIORef (operationStartTimeRef op)
     t1 <- readIORef (operationLastTimeRef op)
     x  <- readIORef (operationTotalUtilisationTimeRef op)
     return (x / (t1 - t0))
  
-- | Signal when the 'operationUtilisationFactor' property value has changed.
operationUtilisationFactorChanged :: Operation a b -> Signal Double
operationUtilisationFactorChanged op =
  mapSignalM (const $ operationUtilisationFactor op) (operationUtilisationFactorChanged_ op)
  
-- | Signal when the 'operationUtilisationFactor' property value has changed.
operationUtilisationFactorChanged_ :: Operation a b -> Signal ()
operationUtilisationFactorChanged_ op =
  mapSignal (const ()) (operationUtilised op) <>
  mapSignal (const ()) (operationPreemptionEnding op)
  
-- | It returns the factor changing from 0 to 1, which estimates how often
-- the operation activity was preempted waiting for the further proceeding
-- since the time of creating the operation.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationPreemptionFactorChanged' and 'operationPreemptionFactorChanged_'.
operationPreemptionFactor :: Operation a b -> Event Double
operationPreemptionFactor op =
  Event $ \p ->
  do t0 <- readIORef (operationStartTimeRef op)
     t1 <- readIORef (operationLastTimeRef op)
     x  <- readIORef (operationTotalPreemptionTimeRef op)
     return (x / (t1 - t0))
  
-- | Signal when the 'operationPreemptionFactor' property value has changed.
operationPreemptionFactorChanged :: Operation a b -> Signal Double
operationPreemptionFactorChanged op =
  mapSignalM (const $ operationPreemptionFactor op) (operationPreemptionFactorChanged_ op)
  
-- | Signal when the 'operationPreemptionFactor' property value has changed.
operationPreemptionFactorChanged_ :: Operation a b -> Signal ()
operationPreemptionFactorChanged_ op =
  mapSignal (const ()) (operationUtilised op) <>
  mapSignal (const ()) (operationPreemptionEnding op)
  
-- | Raised when starting to utilise the operation activity after a new input task is received.
operationUtilising :: Operation a b -> Signal a
operationUtilising = publishSignal . operationUtilisingSource

-- | Raised when the operation activity has been utilised after the current task is processed.
operationUtilised :: Operation a b -> Signal (a, b)
operationUtilised = publishSignal . operationUtilisedSource

-- | Raised when the operation activity utilisation was preempted.
operationPreemptionBeginning :: Operation a b -> Signal a
operationPreemptionBeginning = publishSignal . operationPreemptionBeginningSource

-- | Raised when the operation activity utilisation was proceeded after it had been preempted earlier.
operationPreemptionEnding :: Operation a b -> Signal a
operationPreemptionEnding = publishSignal . operationPreemptionEndingSource

-- | Signal whenever any property of the operation changes.
operationChanged_ :: Operation a b -> Signal ()
operationChanged_ op =
  mapSignal (const ()) (operationUtilising op) <>
  mapSignal (const ()) (operationUtilised op) <>
  mapSignal (const ()) (operationPreemptionEnding op)

-- | Return the summary for the operation with desciption of its
-- properties using the specified indent.
operationSummary :: Operation a b -> Int -> Event ShowS
operationSummary op indent =
  Event $ \p ->
  do t0  <- readIORef (operationStartTimeRef op)
     t1  <- readIORef (operationLastTimeRef op)
     tx1 <- readIORef (operationTotalUtilisationTimeRef op)
     tx2 <- readIORef (operationTotalPreemptionTimeRef op)
     let xf1 = tx1 / (t1 - t0)
         xf2 = tx2 / (t1 - t0)
     xs1 <- readIORef (operationUtilisationTimeRef op)
     xs2 <- readIORef (operationPreemptionTimeRef op)
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "total utilisation time = " . shows tx1 .
       showString "\n" .
       showString tab .
       showString "total preemption time = " . shows tx2 .
       showString "\n" .
       showString tab .
       showString "utilisation factor (from 0 to 1) = " . shows xf1 .
       showString "\n" .
       showString tab .
       showString "preemption factor (from 0 to 1) = " . shows xf2 .
       showString "\n" .
       showString tab .
       showString "utilisation time:\n\n" .
       samplingStatsSummary xs1 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "preemption time:\n\n" .
       samplingStatsSummary xs2 (2 + indent)

-- | Reset the statistics.
resetOperation :: Operation a b -> Event ()
resetOperation op =
  Event $ \p ->
  do let t0 = pointTime p
     writeIORef (operationStartTimeRef op) t0
     writeIORef (operationLastTimeRef op) t0
     writeIORef (operationTotalUtilisationTimeRef op) 0
     writeIORef (operationTotalPreemptionTimeRef op) 0
     writeIORef (operationUtilisationTimeRef op) emptySamplingStats
     writeIORef (operationPreemptionTimeRef op) emptySamplingStats
     
