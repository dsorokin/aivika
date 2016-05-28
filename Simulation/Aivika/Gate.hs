
-- |
-- Module     : Simulation.Aivika.Gate
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- The module defines a gate which can be either opened or closed.
--
module Simulation.Aivika.Gate
       (Gate,
        newGate,
        newGateOpened,
        newGateClosed,
        openGate,
        closeGate,
        gateOpened,
        gateClosed,
        awaitGateOpened,
        awaitGateClosed,
        gateChanged_) where

import Control.Monad

import Simulation.Aivika.Simulation
import Simulation.Aivika.Event
import Simulation.Aivika.Process
import Simulation.Aivika.Signal
import Simulation.Aivika.Ref

-- | Represents a gate, which can be either opened or closed.
data Gate = Gate { gateRef :: Ref Bool }

-- | Create a new gate, specifying whether the gate is initially open.
newGate :: Bool -> Simulation Gate
newGate opened =
  do r <- newRef opened
     return Gate { gateRef = r }

-- | Create a new initially open gate.
newGateOpened :: Simulation Gate
newGateOpened = newGate True

-- | Create a new initially close gate.
newGateClosed :: Simulation Gate
newGateClosed = newGate False

-- | Open the gate if it was closed.
openGate :: Gate -> Event ()
openGate gate =
  writeRef (gateRef gate) True

-- | Close the gate if it was open.
closeGate :: Gate -> Event ()
closeGate gate =
  writeRef (gateRef gate) False

-- | Test whether the gate is open.
gateOpened :: Gate -> Event Bool
gateOpened gate =
  readRef (gateRef gate)

-- | Test whether the gate is closed.
gateClosed :: Gate -> Event Bool
gateClosed gate =
  fmap not $ readRef (gateRef gate)

-- | Await the gate to be opened if required. If the gate is already open
-- then the computation returns immediately.
awaitGateOpened :: Gate -> Process ()
awaitGateOpened gate =
  do f <- liftEvent $ readRef (gateRef gate)
     unless f $
       do processAwait $ refChanged_ (gateRef gate)
          awaitGateOpened gate

-- | Await the gate to be closed if required. If the gate is already closed
-- then the computation returns immediately.
awaitGateClosed :: Gate -> Process ()
awaitGateClosed gate =
  do f <- liftEvent $ readRef (gateRef gate)
     when f $
       do processAwait $ refChanged_ (gateRef gate)
          awaitGateClosed gate

-- | Signal triggered when the state of the gate changes.
gateChanged_ :: Gate -> Signal ()
gateChanged_ gate =
  refChanged_ (gateRef gate)
