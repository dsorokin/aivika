
-- |
-- Module     : Simulation.Aivika.Internal.Observable
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- It defines an observable entity that has a value and signal at
-- which this value changes.
module Simulation.Aivika.Internal.Observable
       (Observable(..),
        observableChanged) where

import Data.Functor

import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Signal

-- | Describes some observable entity that has a value and signal at
-- which this value changes.
data Observable a =
  Observable { readObservable :: Event a,
               -- ^ Return the value of the observable entity.
               observableChanged_ :: Signal ()
               -- ^ Return the signal at which the observable entity changes
               -- but without the information about the changed value.
             }

-- | Return the signal at which the observable entity changes.
observableChanged :: Observable a -> Signal a
observableChanged x = mapSignalM (const $ readObservable x) $ observableChanged_ x

instance Functor Observable where
  fmap f x = x { readObservable = fmap f (readObservable x) }
                          
