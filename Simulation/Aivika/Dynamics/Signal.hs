
-- |
-- Module     : Simulation.Aivika.Dynamics.Signal
-- Copyright  : Copyright (c) 2009-2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
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
        publishSignal,
        triggerSignal,
        handleSignal,
        handleSignal_,
        updateSignal,
        awaitSignal,
        mapSignal,
        composeSignal,
        apSignal,
        filterSignal,
        merge2Signals,
        merge3Signals,
        merge4Signals,
        merge5Signals,
        SignalHistory,
        signalHistorySignal,
        newSignalHistory,
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

import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.UVector as UV

-- | Create a new signal source when the state depends on the event queue.
newSignalSource :: EventQueue -> Simulation (SignalSource a)
newSignalSource queue = 
  newSignalSourceWithUpdate $ queueRun queue

-- | Await the signal.
awaitSignal :: Signal a -> Process a
awaitSignal signal =
  Process $ \pid ->
  Cont $ \c ->
  Dynamics $ \p ->
  do r <- newIORef Nothing
     let Simulation m = 
           handleSignal signal $ 
           \a -> Dynamics $ 
                 \p -> do x <- readIORef r
                          case x of
                            Nothing ->
                              error "The signal was lost: awaitSignal."
                            Just x ->
                              do let Simulation m = x
                                 m $ pointRun p
                                 let Dynamics m = resumeContByParams c a
                                 m p
     h <- m $ pointRun p
     writeIORef r $ Just h
          
-- | Represents the history of the signal values.
data SignalHistory a =
  SignalHistory { signalHistorySignal :: Signal a,  
                  -- ^ The signal for which the history is created.
                  signalHistoryTimes  :: UV.UVector Double,
                  signalHistoryValues :: V.Vector a }

-- | Create a history of the signal values.
newSignalHistory :: Signal a -> Simulation (SignalHistory a)
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
       
-- | Read the history of signal values.
readSignalHistory :: SignalHistory a -> Dynamics (Array Int Double, Array Int a)
readSignalHistory history =
  do updateSignal $ signalHistorySignal history
     xs <- liftIO $ UV.freezeVector (signalHistoryTimes history)
     ys <- liftIO $ V.freezeVector (signalHistoryValues history)
     return (xs, ys)     

-- | Return a signal that is triggered in the specified time points.
newSignalInTimes :: EventQueue -> [Double] -> Dynamics (Signal Double)
newSignalInTimes q xs =
  do s <- liftSimulation $ newSignalSource q
     let loop []       = return ()
         loop (x : xs) = enqueue q x $ 
                         do triggerSignal s x 
                            loop xs
     loop xs
     return $ publishSignal s