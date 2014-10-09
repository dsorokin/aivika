
-- |
-- Module     : Simulation.Aivika.Trans.MonadSim
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a type class of monads based on which the simulation monads can be built.
--
module Simulation.Aivika.Trans.MonadSim
       (MonadSim(..)) where

import Simulation.Aivika.Trans.Internal.MonadSim
