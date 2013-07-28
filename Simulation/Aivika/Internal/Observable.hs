
-- |
-- Module     : Simulation.Aivika.Internal.Observable
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- It defines an observable entity that has a value and signal at
-- which this value changes.
module Simulation.Aivika.Internal.Observable
       (Observable(..)) where

import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Signal

-- | Describes some observable entity that has a value and signal at
-- which this value changes.
data Observable a =
  Observable { readObservable :: Event a,
               -- ^ Return the value of the observable entity.
               observableSignal :: Signal a
               -- ^ Return the signal at which the observable entity changes.
             }
