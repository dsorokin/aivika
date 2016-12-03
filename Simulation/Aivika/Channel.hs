
-- |
-- Module     : Simulation.Aivika.Channel
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines a channel that transforms one 'Signal' to another
-- within the 'Composite' computation.
--
module Simulation.Aivika.Channel
       (-- * Channel Computation
        Channel(..),
        -- * Delay Channel
        delayChannel,
        delayChannelM,
        -- * Debugging
        traceChannel) where

import qualified Control.Category as C
import Control.Monad

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Signal
import Simulation.Aivika.Composite

-- | It allows representing a signal transformation.
newtype Channel a b =
  Channel { runChannel :: Signal a -> Composite (Signal b)
            -- ^ Run the channel transform.
          }

instance C.Category Channel where

  id = Channel return
  
  (Channel g) . (Channel f) =
    Channel $ \a -> f a >>= g

-- | Return a delayed signal.
--
-- This is actually the 'delaySignal' function wrapped in the 'Channel' type. 
delayChannel :: Double            -- ^ the delay
                -> Channel a a    -- ^ the delay channel
delayChannel delay =
  Channel $ \a -> return $ delaySignal delay a

-- | Like 'delayChannel', but it re-computes the delay each time.
--
-- This is actually the 'delaySignalM' function wrapped in the 'Channel' type. 
delayChannelM :: Event Double     -- ^ the delay
                 -> Channel a a    -- ^ the delay channel
delayChannelM delay =
  Channel $ \a -> return $ delaySignalM delay a
                                 
-- | Show the debug message with the current simulation time,
-- when emitting the output signal.
traceChannel :: String -> Channel a b -> Channel a b
traceChannel message (Channel f) =
  Channel $ \a ->
  do b <- f a
     return $
       traceSignal message b
