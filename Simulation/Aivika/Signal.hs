
-- |
-- Module     : Simulation.Aivika.Signal
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
module Simulation.Aivika.Signal
       (Signal(..),
        SignalSource(..),
        newSignalSource,
        newSignalInTimes,
        newSignalInIntegTimes,
        newSignalInStartTime,
        newSignalInStopTime,
        handleSignal_,
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
        readSignalHistory) where

import Data.IORef
import Data.Array

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Signal
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Cont
import Simulation.Aivika.Internal.Process

import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.UVector as UV

-- | Await the signal.
awaitSignal :: Signal a -> Process a
awaitSignal signal =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do r <- newIORef Nothing
     h <- invokeEvent p $
          handleSignal signal $ 
          \a -> Event $ 
                \p -> do x <- readIORef r
                         case x of
                           Nothing ->
                             error "The signal was lost: awaitSignal."
                           Just x ->
                             do invokeEvent p x
                                invokeEvent p $ resumeCont c a
     writeIORef r $ Just h
          
-- | Represents the history of the signal values.
data SignalHistory a =
  SignalHistory { signalHistorySignal :: Signal a,  
                  -- ^ The signal for which the history is created.
                  signalHistoryTimes  :: UV.UVector Double,
                  signalHistoryValues :: V.Vector a }

-- | Create a history of the signal values.
newSignalHistory :: Signal a -> Event (SignalHistory a)
newSignalHistory signal =
  do ts <- liftIO UV.newVector
     xs <- liftIO V.newVector
     handleSignal_ signal $ \a ->
       Event $ \p ->
       do liftIO $ UV.appendVector ts (pointTime p)
          liftIO $ V.appendVector xs a
     return SignalHistory { signalHistorySignal = signal,
                            signalHistoryTimes  = ts,
                            signalHistoryValues = xs }
       
-- | Read the history of signal values.
readSignalHistory :: SignalHistory a -> Event (Array Int Double, Array Int a)
readSignalHistory history =
  do xs <- liftIO $ UV.freezeVector (signalHistoryTimes history)
     ys <- liftIO $ V.freezeVector (signalHistoryValues history)
     return (xs, ys)     
     
-- | Trigger the signal with the current time.
triggerSignalWithCurrentTime :: SignalSource Double -> Event ()
triggerSignalWithCurrentTime s =
  Event $ \p -> invokeEvent p $ triggerSignal s (pointTime p)

-- | Return a signal that is triggered in the specified time points.
newSignalInTimes :: [Double] -> Event (Signal Double)
newSignalInTimes xs =
  do s <- liftSimulation newSignalSource
     enqueueEventWithTimes xs $ triggerSignalWithCurrentTime s
     return $ publishSignal s
       
-- | Return a signal that is triggered in the integration time points.
-- It should be called with help of 'runEventInStartTime'.
newSignalInIntegTimes :: Event (Signal Double)
newSignalInIntegTimes =
  do s <- liftSimulation newSignalSource
     enqueueEventWithIntegTimes $ triggerSignalWithCurrentTime s
     return $ publishSignal s
     
-- | Return a signal that is triggered in the start time.
-- It should be called with help of 'runEventInStartTime'.
newSignalInStartTime :: Event (Signal Double)
newSignalInStartTime =
  do s <- liftSimulation newSignalSource
     enqueueEventWithStartTime $ triggerSignalWithCurrentTime s
     return $ publishSignal s

-- | Return a signal that is triggered in the stop time.
newSignalInStopTime :: Event (Signal Double)
newSignalInStopTime =
  do s <- liftSimulation newSignalSource
     enqueueEventWithStopTime $ triggerSignalWithCurrentTime s
     return $ publishSignal s