
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Comp
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a type class of monads based on which the simulation monads can be built.
--
module Simulation.Aivika.Trans.Comp
       (ProtoComp(..),
        Comp(..),
        CompTrans(..)) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.ProtoArray
import Simulation.Aivika.Trans.Unboxed
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Internal.Specs

-- | A prototype of the type class of monads based on which the simulation monads can be built. 
class (Monad m,
       ExceptionHandling m,
       Sessionning m,
       ProtoReferring m,
       ProtoArraying m,
       Unboxed m Double,
       Generating m) => ProtoComp m

-- | Such a simulation monad that allows enqueueing events.
class (ProtoComp m, EventQueueing m) => Comp m

-- | A variant of the standard 'MonadTrans' type class with one difference:
-- the computation that will be lifted into another must be 'Comp' instead of
-- more general and less restricted 'Monad'.
class CompTrans t where

  -- | Lift the underlying computation into another within simulation.
  liftComp :: Comp m => m a -> t m a

instance ProtoComp IO
