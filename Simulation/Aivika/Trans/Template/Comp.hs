
-- |
-- Module     : Simulation.Aivika.Trans.Template.Comp
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'TemplateComp' monad template based on which we can
-- construct automatically simulation computations with extended functionality.
--
module Simulation.Aivika.Trans.Template.Comp
       (TemplateComp(..)) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Template

instance CompTrans TemplateComp where

  {-# INLINE liftComp #-}
  liftComp = TemplateComp
