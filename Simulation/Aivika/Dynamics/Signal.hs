
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
        publishSignal,
        triggerSignal,
        handleSignal,
        handleSignal_,
        awaitSignal,
        mapSignal,
        filterSignal,
        merge2Signals,
        merge3Signals,
        merge4Signals,
        merge5Signals) where

import Data.IORef

import Simulation.Aivika.Dynamics.Internal.Signal
import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Cont
import Simulation.Aivika.Dynamics.Internal.Process

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
          
