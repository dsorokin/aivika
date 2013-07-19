
-- |
-- Module     : Simulation.Aivika.Dynamics.Signal
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines the signal which we can subscribe handlers to. 
-- These handlers can be disposed. The signal is triggered in the 
-- current time point actuating the corresponded computations from 
-- the handlers. 
--
module Simulation.Aivika.Dynamics.Signal
       (Signal,
        SignalSource,
        newSignalSource,
        newSignalSourceWithUpdate,
        newSignalInTimes,
        newSignalInIntegTimes,
        newSignalInStartTime,
        newSignalInStopTime,
        publishSignal,
        triggerSignal,
        handleSignal,
        handleSignal_,
        updateSignal,
        awaitSignal,
        mapSignal,
        mapSignalM,
        apSignal,
        filterSignal,
        filterSignalM,
        emptySignal,
        merge2Signals,
        merge3Signals,
        merge4Signals,
        merge5Signals,
        SignalHistory,
        signalHistorySignal,
        newSignalHistory,
        newSignalHistoryThrough,
        readSignalHistory) where

import Data.IORef
import Data.Array

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Internal.Signal
import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Cont
import Simulation.Aivika.Dynamics.Internal.Process
import Simulation.Aivika.Dynamics.Base

import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.UVector as UV

-- | Create a new signal source when the state depends on the event queue.
newSignalSource :: EventQueue -> Simulation (SignalSource a)
newSignalSource queue = 
  newSignalSourceWithUpdate $ runQueueSyncBefore queue

-- | Await the signal.
awaitSignal :: Signal a -> Process a
awaitSignal signal =
  Process $ \pid ->
  Cont $ \c ->
  Dynamics $ \p ->
  do r <- newIORef Nothing
     let Dynamics m = 
           handleSignal signal $ 
           \a -> Dynamics $ 
                 \p -> do x <- readIORef r
                          case x of
                            Nothing ->
                              error "The signal was lost: awaitSignal."
                            Just x ->
                              do let Dynamics m = x
                                 m p
                                 let Dynamics m = resumeContByParams c a
                                 m p
     h <- m p
     writeIORef r $ Just h
          
-- | Represents the history of the signal values.
data SignalHistory a =
  SignalHistory { signalHistorySignal :: Signal a,  
                  -- ^ The signal for which the history is created.
                  signalHistoryTimes  :: UV.UVector Double,
                  signalHistoryValues :: V.Vector a }

-- | Create a history of the signal values.
newSignalHistory :: Signal a -> Dynamics (SignalHistory a)
newSignalHistory signal =
  do ts <- liftIO UV.newVector
     xs <- liftIO V.newVector
     handleSignal_ signal $ \a ->
       Dynamics $ \p ->
       do liftIO $ UV.appendVector ts (pointTime p)
          liftIO $ V.appendVector xs a
     return SignalHistory { signalHistorySignal = signal,
                            signalHistoryTimes  = ts,
                            signalHistoryValues = xs }
       
-- | Create a history of the signal values with delay through the event queue.
-- The history will be created at the same simulation time, just the corresponded 
-- handler will be subscribed to the signal after the new event will be processed 
-- by the queue. 
-- 
-- It is very useful if we want the signal won't be triggered at the current 
-- time until we complete some preparation. This is relatated to the fact that
-- the signal is updated at time of subscribing the handler. So, if we subscribe
-- to the signal which must be triggered at the current time then it will be
-- triggered. Using the event queue allows us to complete some preparation logic
-- before the signal will be triggered at the same simulation time point.
newSignalHistoryThrough :: EventQueue -> Signal a -> Dynamics (SignalHistory a)
newSignalHistoryThrough q signal =
  do ts <- liftIO UV.newVector
     xs <- liftIO V.newVector
     enqueueWithCurrentTime q $
       handleSignal_ signal $ \a ->
       Dynamics $ \p ->
       do liftIO $ UV.appendVector ts (pointTime p)
          liftIO $ V.appendVector xs a
     return SignalHistory { signalHistorySignal = signal,
                            signalHistoryTimes  = ts,
                            signalHistoryValues = xs }
       
-- | Read the history of signal values.
readSignalHistory :: SignalHistory a -> Dynamics (Array Int Double, Array Int a)
readSignalHistory history =
  do updateSignal $ signalHistorySignal history
     xs <- liftIO $ UV.freezeVector (signalHistoryTimes history)
     ys <- liftIO $ V.freezeVector (signalHistoryValues history)
     return (xs, ys)     
     
-- | Trigger the signal with the current time.
triggerSignalWithTime :: SignalSource Double -> Dynamics ()
triggerSignalWithTime s =
  Dynamics $ \p ->
  do let Dynamics m = triggerSignal s (pointTime p)
     m p

-- | Return a signal that is triggered in the specified time points.
newSignalInTimes :: EventQueue -> [Double] -> Dynamics (Signal Double)
newSignalInTimes q xs =
  do s <- liftSimulation $ newSignalSource q
     enqueueWithTimes q xs $ triggerSignalWithTime s
     return $ publishSignal s
       
-- | Return a signal that is triggered in the integration time points.
-- It should be called with help of 'runDynamicsInStartTime'.
newSignalInIntegTimes :: EventQueue -> Dynamics (Signal Double)
newSignalInIntegTimes q =
  do s <- liftSimulation $ newSignalSource q
     enqueueWithIntegTimes q $ triggerSignalWithTime s
     return $ publishSignal s
     
-- | Return a signal that is triggered in the start time.
-- It should be called with help of 'runDynamicsInStartTime'.
newSignalInStartTime :: EventQueue -> Dynamics (Signal Double)
newSignalInStartTime q =
  do s <- liftSimulation $ newSignalSource q
     enqueueWithStartTime q $ triggerSignalWithTime s
     return $ publishSignal s

-- | Return a signal that is triggered in the stop time.
newSignalInStopTime :: EventQueue -> Dynamics (Signal Double)
newSignalInStopTime q =
  do s <- liftSimulation $ newSignalSource q
     enqueueWithStopTime q $ triggerSignalWithTime s
     return $ publishSignal s
