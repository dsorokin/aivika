
-- |
-- Module     : Simulation.Aivika.Gate
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- The module defines a gate which can be either opened or closed.
--
module Simulation.Aivika.Gate
       (Gate,
        createGate,
        createGateOpened,
        createGateClosed,
        openGate,
        closeGate,
        gateOpened,
        gateClosed,
        awaitGateOpened,
        awaitGateClosed) where

import Control.Monad

import Simulation.Aivika.Simulation
import Simulation.Aivika.Event
import Simulation.Aivika.Process
import Simulation.Aivika.Ref

-- | Represents a gate, which can be either opened or closed.
data Gate = Gate { gateRef :: Ref Bool }

-- | Create a new gate, specifying whether the gate is initially open.
createGate :: Bool -> Simulation Gate
createGate opened =
  do r <- newRef opened
     return Gate { gateRef = r }

-- | Create a new initially open gate.
createGateOpened :: Simulation Gate
createGateOpened = createGate True

-- | Create a new initially close gate.
createGateClosed :: Simulation Gate
createGateClosed = createGate False

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

