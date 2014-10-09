
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.MonadSim
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a type class of monads based on which the simulation monads can be built.
--
module Simulation.Aivika.Trans.Internal.MonadSim
       (MonadSim(..)) where

import Control.Monad

import Simulation.Aivika.Trans.Internal.Exception
import Simulation.Aivika.Trans.Internal.Session
import Simulation.Aivika.Trans.Internal.ProtoRef
import Simulation.Aivika.Trans.Internal.ProtoArray
import Simulation.Aivika.Trans.Internal.Generator
import Simulation.Aivika.Trans.Internal.Specs

-- | A type class of monads based on which the simulation monads can be built. 
class (ExceptionHandling m,
       Monad m,
       Sessionning m,
       ProtoReferring m,
       ProtoArraying m,
       ProtoUArraying m Double,
       Generating m,
       EventQueueing m) => MonadSim m

instance MonadSim IO
