
-- |
-- Module     : Simulation.Aivika.Trans.Signal
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the signal which we can subscribe handlers to. 
-- These handlers can be disposed. The signal is triggered in the 
-- current time point actuating the corresponded computations from 
-- the handlers. 
--
module Simulation.Aivika.Trans.Signal
       (-- * Handling and Triggering Signal
        Signal(..),
        handleSignal_,
        SignalSource,
        newSignalSource,
        publishSignal,
        triggerSignal,
        -- * Useful Combinators
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
        -- * Signal Arriving
        arrivalSignal,
        -- * Creating Signal in Time Points
        newSignalInTimes,
        newSignalInIntegTimes,
        newSignalInStartTime,
        newSignalInStopTime,
        -- * Signal History
        SignalHistory,
        signalHistorySignal,
        newSignalHistory,
        newSignalHistoryStartingWith,
        readSignalHistory,
        -- * Signalable Computations
        Signalable(..),
        signalableChanged,
        emptySignalable,
        appendSignalable) where

import Simulation.Aivika.Trans.Internal.Signal
