
-- |
-- Module     : Simulation.Aivika.Activity
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It models an activity that can be utilised. The activity is similar to a 'Server'
-- but destined for simulation within 'Net' computation.
module Simulation.Aivika.Activity
       (-- * Activity
        Activity,
        newActivity,
        newStateActivity,
        newPreemptibleActivity,
        newPreemptibleStateActivity,
        -- * Processing
        activityNet,
        -- * Activity Properties
        activityInitState,
        activityState,
        activityTotalUtilisationTime,
        activityTotalIdleTime,
        activityTotalPreemptionTime,
        activityUtilisationTime,
        activityIdleTime,
        activityPreemptionTime,
        activityUtilisationFactor,
        activityIdleFactor,
        activityPreemptionFactor,
        -- * Summary
        activitySummary,
        -- * Derived Signals for Properties
        activityStateChanged,
        activityStateChanged_,
        activityTotalUtilisationTimeChanged,
        activityTotalUtilisationTimeChanged_,
        activityTotalIdleTimeChanged,
        activityTotalIdleTimeChanged_,
        activityTotalPreemptionTimeChanged,
        activityTotalPreemptionTimeChanged_,
        activityUtilisationTimeChanged,
        activityUtilisationTimeChanged_,
        activityIdleTimeChanged,
        activityIdleTimeChanged_,
        activityPreemptionTimeChanged,
        activityPreemptionTimeChanged_,
        activityUtilisationFactorChanged,
        activityUtilisationFactorChanged_,
        activityIdleFactorChanged,
        activityIdleFactorChanged_,
        activityPreemptionFactorChanged,
        activityPreemptionFactorChanged_,
        -- * Basic Signals
        activityUtilising,
        activityUtilised,
        activityPreempting,
        activityReentering,
        -- * Overall Signal
        activityChanged_) where

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
import Simulation.Aivika.Net
import Simulation.Aivika.Server
import Simulation.Aivika.Statistics

-- | Like 'Server' it models an activity that takes @a@ and provides @b@ having state @s@.
-- But unlike the former the activity is destined for simulation within 'Net' computation.
data Activity s a b =
  Activity { activityInitState :: s,
             -- ^ The initial state of the activity.
             activityStateRef :: IORef s,
             -- ^ The current state of the activity.
             activityProcess :: s -> a -> Process (s, b),
             -- ^ Provide @b@ by specified @a@.
             activityProcessPreemptible :: Bool,
             -- ^ Whether the process is preemptible.
             activityTotalUtilisationTimeRef :: IORef Double,
             -- ^ The counted total time of utilising the activity.
             activityTotalIdleTimeRef :: IORef Double,
             -- ^ The counted total time when the activity was idle.
             activityTotalPreemptionTimeRef :: IORef Double,
             -- ^ The counted total time when the activity was preempted. 
             activityUtilisationTimeRef :: IORef (SamplingStats Double),
             -- ^ The statistics for the utilisation time.
             activityIdleTimeRef :: IORef (SamplingStats Double),
             -- ^ The statistics for the time when the activity was idle.
             activityPreemptionTimeRef :: IORef (SamplingStats Double),
             -- ^ The statistics for the time when the activity was preempted.
             activityUtilisingSource :: SignalSource a,
             -- ^ A signal raised when starting to utilise the activity.
             activityUtilisedSource :: SignalSource (a, b),
             -- ^ A signal raised when the activity has been utilised.
             activityPreemptingSource :: SignalSource a,
             -- ^ A signal raised when the utilisation was preempted.
             activityReenteringSource :: SignalSource a
             -- ^ A signal raised when the utilisation was proceeded after it had been preempted earlier.
           }

-- | Create a new activity that can provide output @b@ by input @a@.
--
-- By default, it is assumed that the activity utilisation cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newActivity :: (a -> Process b)
               -- ^ provide an output by the specified input
               -> Simulation (Activity () a b)
newActivity = newPreemptibleActivity False

-- | Create a new activity that can provide output @b@ by input @a@
-- starting from state @s@.
--
-- By default, it is assumed that the activity utilisation cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newStateActivity :: (s -> a -> Process (s, b))
                    -- ^ provide a new state and output by the specified 
                    -- old state and input
                    -> s
                    -- ^ the initial state
                    -> Simulation (Activity s a b)
newStateActivity = newPreemptibleStateActivity False

-- | Create a new preemptible activity that can provide output @b@ by input @a@.
newPreemptibleActivity :: Bool
                          -- ^ whether the activity can be preempted
                          -> (a -> Process b)
                          -- ^ provide an output by the specified input
                          -> Simulation (Activity () a b)
newPreemptibleActivity preemptible provide =
  flip (newPreemptibleStateActivity preemptible) () $ \s a ->
  do b <- provide a
     return (s, b)

-- | Create a new activity that can provide output @b@ by input @a@
-- starting from state @s@.
newPreemptibleStateActivity :: Bool
                               -- ^ whether the activity can be preempted
                               -> (s -> a -> Process (s, b))
                               -- ^ provide a new state and output by the specified 
                               -- old state and input
                               -> s
                               -- ^ the initial state
                              -> Simulation (Activity s a b)
newPreemptibleStateActivity preemptible provide state =
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
     s4 <- newSignalSource
     return Activity { activityInitState = state,
                       activityStateRef = r0,
                       activityProcess = provide,
                       activityProcessPreemptible = preemptible,
                       activityTotalUtilisationTimeRef = r1,
                       activityTotalIdleTimeRef = r2,
                       activityTotalPreemptionTimeRef = r3,
                       activityUtilisationTimeRef = r4,
                       activityIdleTimeRef = r5,
                       activityPreemptionTimeRef = r6,
                       activityUtilisingSource = s1,
                       activityUtilisedSource = s2,
                       activityPreemptingSource = s3,
                       activityReenteringSource = s4 }

-- | Return a network computation for the specified activity.
--
-- The computation updates the internal state of the activity. The usual case is when 
-- the computation is applied only once in a chain of data processing. Otherwise; 
-- every time the computation is used, the state of the activity changes. Sometimes 
-- it can be indeed useful if you want to aggregate the statistics for different 
-- activities simultaneously, but it would be more preferable to avoid this.
--
-- If you connect different activity computations returned by this function in a chain 
-- with help of '>>>' or other category combinator then this chain will act as one 
-- whole, where the first activity will take a new task only after the last activity 
-- finishes its current task and requests for the next one from the previous activity 
-- in the chain. This is not always that thing you might need.
activityNet :: Activity s a b -> Net a b
activityNet act = Net $ loop (activityInitState act) Nothing
  where
    loop s r a =
      do t0 <- liftDynamics time
         liftEvent $
           do case r of
                Nothing -> return ()
                Just t' ->
                  liftIO $
                  do modifyIORef' (activityTotalIdleTimeRef act) (+ (t0 - t'))
                     modifyIORef' (activityIdleTimeRef act) $
                       addSamplingStats (t0 - t')
              triggerSignal (activityUtilisingSource act) a
         -- utilise the activity
         (s', b, dt) <- if activityProcessPreemptible act
                        then activityProcessPreempting act s a
                        else do (s', b) <- activityProcess act s a
                                return (s', b, 0)
         t1 <- liftDynamics time
         liftEvent $
           do liftIO $
                do writeIORef (activityStateRef act) $! s'
                   modifyIORef' (activityTotalUtilisationTimeRef act) (+ (t1 - t0 - dt))
                   modifyIORef' (activityUtilisationTimeRef act) $
                     addSamplingStats (t1 - t0 - dt)
              triggerSignal (activityUtilisedSource act) (a, b)
         return (b, Net $ loop s' (Just t1))

-- | Process the input with ability to handle a possible preemption.
activityProcessPreempting :: Activity s a b -> s -> a -> Process (s, b, Double)
activityProcessPreempting act s a =
  do pid <- processId
     t0  <- liftDynamics time
     rs  <- liftIO $ newIORef 0
     r0  <- liftIO $ newIORef t0
     h1  <- liftEvent $
            handleSignal (processPreempting pid) $ \() ->
            do t0 <- liftDynamics time
               liftIO $ writeIORef r0 t0
               triggerSignal (activityPreemptingSource act) a
     h2  <- liftEvent $
            handleSignal (processReentering pid) $ \() ->
            do t0 <- liftIO $ readIORef r0
               t1 <- liftDynamics time
               let dt = t1 - t0
               liftIO $
                 do modifyIORef' rs (+ dt)
                    modifyIORef' (activityTotalPreemptionTimeRef act) (+ dt)
                    modifyIORef' (activityPreemptionTimeRef act) $
                      addSamplingStats dt
               triggerSignal (activityReenteringSource act) a 
     let m1 =
           do (s', b) <- activityProcess act s a
              dt <- liftIO $ readIORef rs
              return (s', b, dt)
         m2 =
           liftEvent $
           do disposeEvent h1
              disposeEvent h2
     finallyProcess m1 m2

-- | Return the current state of the activity.
--
-- See also 'activityStateChanged' and 'activityStateChanged_'.
activityState :: Activity s a b -> Event s
activityState act =
  Event $ \p -> readIORef (activityStateRef act)
  
-- | Signal when the 'activityState' property value has changed.
activityStateChanged :: Activity s a b -> Signal s
activityStateChanged act =
  mapSignalM (const $ activityState act) (activityStateChanged_ act)
  
-- | Signal when the 'activityState' property value has changed.
activityStateChanged_ :: Activity s a b -> Signal ()
activityStateChanged_ act =
  mapSignal (const ()) (activityUtilised act)

-- | Return the counted total time when the activity was utilised.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityTotalUtilisationTimeChanged' and 'activityTotalUtilisationTimeChanged_'.
activityTotalUtilisationTime :: Activity s a b -> Event Double
activityTotalUtilisationTime act =
  Event $ \p -> readIORef (activityTotalUtilisationTimeRef act)
  
-- | Signal when the 'activityTotalUtilisationTime' property value has changed.
activityTotalUtilisationTimeChanged :: Activity s a b -> Signal Double
activityTotalUtilisationTimeChanged act =
  mapSignalM (const $ activityTotalUtilisationTime act) (activityTotalUtilisationTimeChanged_ act)
  
-- | Signal when the 'activityTotalUtilisationTime' property value has changed.
activityTotalUtilisationTimeChanged_ :: Activity s a b -> Signal ()
activityTotalUtilisationTimeChanged_ act =
  mapSignal (const ()) (activityUtilised act)

-- | Return the counted total time when the activity was idle.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityTotalIdleTimeChanged' and 'activityTotalIdleTimeChanged_'.
activityTotalIdleTime :: Activity s a b -> Event Double
activityTotalIdleTime act =
  Event $ \p -> readIORef (activityTotalIdleTimeRef act)
  
-- | Signal when the 'activityTotalIdleTime' property value has changed.
activityTotalIdleTimeChanged :: Activity s a b -> Signal Double
activityTotalIdleTimeChanged act =
  mapSignalM (const $ activityTotalIdleTime act) (activityTotalIdleTimeChanged_ act)
  
-- | Signal when the 'activityTotalIdleTime' property value has changed.
activityTotalIdleTimeChanged_ :: Activity s a b -> Signal ()
activityTotalIdleTimeChanged_ act =
  mapSignal (const ()) (activityUtilising act)

-- | Return the counted total time when the activity was preemted waiting for
-- the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityTotalPreemptionTimeChanged' and 'activityTotalPreemptionTimeChanged_'.
activityTotalPreemptionTime :: Activity s a b -> Event Double
activityTotalPreemptionTime act =
  Event $ \p -> readIORef (activityTotalPreemptionTimeRef act)
  
-- | Signal when the 'activityTotalPreemptionTime' property value has changed.
activityTotalPreemptionTimeChanged :: Activity s a b -> Signal Double
activityTotalPreemptionTimeChanged act =
  mapSignalM (const $ activityTotalPreemptionTime act) (activityTotalPreemptionTimeChanged_ act)
  
-- | Signal when the 'activityTotalPreemptionTime' property value has changed.
activityTotalPreemptionTimeChanged_ :: Activity s a b -> Signal ()
activityTotalPreemptionTimeChanged_ act =
  mapSignal (const ()) (activityReentering act)

-- | Return the statistics for the time when the activity was utilised.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityUtilisationTimeChanged' and 'activityUtilisationTimeChanged_'.
activityUtilisationTime :: Activity s a b -> Event (SamplingStats Double)
activityUtilisationTime act =
  Event $ \p -> readIORef (activityUtilisationTimeRef act)
  
-- | Signal when the 'activityUtilisationTime' property value has changed.
activityUtilisationTimeChanged :: Activity s a b -> Signal (SamplingStats Double)
activityUtilisationTimeChanged act =
  mapSignalM (const $ activityUtilisationTime act) (activityUtilisationTimeChanged_ act)
  
-- | Signal when the 'activityUtilisationTime' property value has changed.
activityUtilisationTimeChanged_ :: Activity s a b -> Signal ()
activityUtilisationTimeChanged_ act =
  mapSignal (const ()) (activityUtilised act)

-- | Return the statistics for the time when the activity was idle.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityIdleTimeChanged' and 'activityIdleTimeChanged_'.
activityIdleTime :: Activity s a b -> Event (SamplingStats Double)
activityIdleTime act =
  Event $ \p -> readIORef (activityIdleTimeRef act)
  
-- | Signal when the 'activityIdleTime' property value has changed.
activityIdleTimeChanged :: Activity s a b -> Signal (SamplingStats Double)
activityIdleTimeChanged act =
  mapSignalM (const $ activityIdleTime act) (activityIdleTimeChanged_ act)
  
-- | Signal when the 'activityIdleTime' property value has changed.
activityIdleTimeChanged_ :: Activity s a b -> Signal ()
activityIdleTimeChanged_ act =
  mapSignal (const ()) (activityUtilising act)

-- | Return the statistics for the time when the activity was preempted
-- waiting for the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityPreemptionTimeChanged' and 'activityPreemptionTimeChanged_'.
activityPreemptionTime :: Activity s a b -> Event (SamplingStats Double)
activityPreemptionTime act =
  Event $ \p -> readIORef (activityPreemptionTimeRef act)
  
-- | Signal when the 'activityPreemptionTime' property value has changed.
activityPreemptionTimeChanged :: Activity s a b -> Signal (SamplingStats Double)
activityPreemptionTimeChanged act =
  mapSignalM (const $ activityPreemptionTime act) (activityPreemptionTimeChanged_ act)
  
-- | Signal when the 'activityPreemptionTime' property value has changed.
activityPreemptionTimeChanged_ :: Activity s a b -> Signal ()
activityPreemptionTimeChanged_ act =
  mapSignal (const ()) (activityReentering act)
  
-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was utilised.
--
-- This factor is calculated as
--
-- @
--   totalUtilisationTime \/ (totalUtilisationTime + totalIdleTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'activityUtilisationFactorChanged' and 'activityUtilisationFactorChanged_'.
activityUtilisationFactor :: Activity s a b -> Event Double
activityUtilisationFactor act =
  Event $ \p ->
  do x1 <- readIORef (activityTotalUtilisationTimeRef act)
     x2 <- readIORef (activityTotalIdleTimeRef act)
     x3 <- readIORef (activityTotalPreemptionTimeRef act)
     return (x1 / (x1 + x2 + x3))
  
-- | Signal when the 'activityUtilisationFactor' property value has changed.
activityUtilisationFactorChanged :: Activity s a b -> Signal Double
activityUtilisationFactorChanged act =
  mapSignalM (const $ activityUtilisationFactor act) (activityUtilisationFactorChanged_ act)
  
-- | Signal when the 'activityUtilisationFactor' property value has changed.
activityUtilisationFactorChanged_ :: Activity s a b -> Signal ()
activityUtilisationFactorChanged_ act =
  mapSignal (const ()) (activityUtilising act) <>
  mapSignal (const ()) (activityUtilised act) <>
  mapSignal (const ()) (activityReentering act)
  
-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was idle.
--
-- This factor is calculated as
--
-- @
--   totalIdleTime \/ (totalUtilisationTime + totalIdleTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'activityIdleFactorChanged' and 'activityIdleFactorChanged_'.
activityIdleFactor :: Activity s a b -> Event Double
activityIdleFactor act =
  Event $ \p ->
  do x1 <- readIORef (activityTotalUtilisationTimeRef act)
     x2 <- readIORef (activityTotalIdleTimeRef act)
     x3 <- readIORef (activityTotalPreemptionTimeRef act)
     return (x2 / (x1 + x2 + x3))
  
-- | Signal when the 'activityIdleFactor' property value has changed.
activityIdleFactorChanged :: Activity s a b -> Signal Double
activityIdleFactorChanged act =
  mapSignalM (const $ activityIdleFactor act) (activityIdleFactorChanged_ act)
  
-- | Signal when the 'activityIdleFactor' property value has changed.
activityIdleFactorChanged_ :: Activity s a b -> Signal ()
activityIdleFactorChanged_ act =
  mapSignal (const ()) (activityUtilising act) <>
  mapSignal (const ()) (activityUtilised act) <>
  mapSignal (const ()) (activityReentering act)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was preempted waiting for the further proceeding.
--
-- This factor is calculated as
--
-- @
--   totalUtilisationTime \/ (totalUtilisationTime + totalIdleTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'activityPreemptionFactorChanged' and 'activityPreemptionFactorChanged_'.
activityPreemptionFactor :: Activity s a b -> Event Double
activityPreemptionFactor act =
  Event $ \p ->
  do x1 <- readIORef (activityTotalUtilisationTimeRef act)
     x2 <- readIORef (activityTotalIdleTimeRef act)
     x3 <- readIORef (activityTotalPreemptionTimeRef act)
     return (x3 / (x1 + x2 + x3))
  
-- | Signal when the 'activityPreemptionFactor' property value has changed.
activityPreemptionFactorChanged :: Activity s a b -> Signal Double
activityPreemptionFactorChanged act =
  mapSignalM (const $ activityPreemptionFactor act) (activityPreemptionFactorChanged_ act)
  
-- | Signal when the 'activityPreemptionFactor' property value has changed.
activityPreemptionFactorChanged_ :: Activity s a b -> Signal ()
activityPreemptionFactorChanged_ act =
  mapSignal (const ()) (activityUtilising act) <>
  mapSignal (const ()) (activityUtilised act) <>
  mapSignal (const ()) (activityReentering act)
  
-- | Raised when starting to utilise the activity after a new input task is received.
activityUtilising :: Activity s a b -> Signal a
activityUtilising = publishSignal . activityUtilisingSource

-- | Raised when the activity has been utilised after the current task is processed.
activityUtilised :: Activity s a b -> Signal (a, b)
activityUtilised = publishSignal . activityUtilisedSource

-- | Raised when the task utilisation by the activity was preempted.
activityPreempting :: Activity s a b -> Signal a
activityPreempting = publishSignal . activityPreemptingSource

-- | Raised when the task utilisation by the activity was proceeded after it had been preempted earlier.
activityReentering :: Activity s a b -> Signal a
activityReentering = publishSignal . activityReenteringSource

-- | Signal whenever any property of the activity changes.
activityChanged_ :: Activity s a b -> Signal ()
activityChanged_ act =
  mapSignal (const ()) (activityUtilising act) <>
  mapSignal (const ()) (activityUtilised act) <>
  mapSignal (const ()) (activityReentering act)

-- | Return the summary for the activity with desciption of its
-- properties using the specified indent.
activitySummary :: Activity s a b -> Int -> Event ShowS
activitySummary act indent =
  Event $ \p ->
  do tx1 <- readIORef (activityTotalUtilisationTimeRef act)
     tx2 <- readIORef (activityTotalIdleTimeRef act)
     tx3 <- readIORef (activityTotalPreemptionTimeRef act)
     let xf1 = tx1 / (tx1 + tx2 + tx3)
         xf2 = tx2 / (tx1 + tx2 + tx3)
         xf3 = tx3 / (tx1 + tx2 + tx3)
     xs1 <- readIORef (activityUtilisationTimeRef act)
     xs2 <- readIORef (activityIdleTimeRef act)
     xs3 <- readIORef (activityPreemptionTimeRef act)
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "total utilisation time = " . shows tx1 .
       showString "\n" .
       showString tab .
       showString "total idle time = " . shows tx2 .
       showString "\n" .
       showString tab .
       showString "total preemption time = " . shows tx3 .
       showString "\n" .
       showString tab .
       showString "utilisation factor (from 0 to 1) = " . shows xf1 .
       showString "\n" .
       showString tab .
       showString "idle factor (from 0 to 1) = " . shows xf2 .
       showString "\n" .
       showString tab .
       showString "preemption factor (from 0 to 1) = " . shows xf3 .
       showString "\n" .
       showString tab .
       showString "utilisation time (locked while awaiting the input):\n\n" .
       samplingStatsSummary xs1 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "idle time:\n\n" .
       samplingStatsSummary xs2 (2 + indent) .
       showString tab .
       showString "preemption time:\n\n" .
       samplingStatsSummary xs3 (2 + indent)
