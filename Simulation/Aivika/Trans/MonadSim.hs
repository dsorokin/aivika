
{-# LANGUAGE FlexibleContexts #-}

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

import Control.Monad

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.ProtoArray
import Simulation.Aivika.Trans.ProtoVector
import Simulation.Aivika.Trans.Unboxed
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Internal.Specs

-- | A type class of monads based on which the simulation monads can be built. 
class (ExceptionHandling m,
       Monad m,
       Sessionning m,
       ProtoReferring m,
       ProtoArraying m,
       ProtoVectoring m,
       Unboxed m Double,
       Generating m,
       EventQueueable m) => MonadSim m

instance MonadSim IO
