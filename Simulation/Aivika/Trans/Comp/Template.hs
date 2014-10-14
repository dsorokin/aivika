
-- |
-- Module     : Simulation.Aivika.Trans.Comp.Template
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'TemplateComp' monad template based on which we can
-- construct automatically simulation computations with extended functionality.
--
module Simulation.Aivika.Trans.Comp.Template
       (TemplateComp(..)) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Session.Template
import Simulation.Aivika.Trans.ProtoRef.Template
import Simulation.Aivika.Trans.ProtoArray.Template
import Simulation.Aivika.Trans.Unboxed.Template
import Simulation.Aivika.Trans.Generator.Template
import Simulation.Aivika.Trans.Internal.Template

instance CompTrans TemplateComp where

  {-# INLINE liftComp #-}
  liftComp = TemplateComp

instance ProtoComp m => ProtoComp (TemplateComp m)
