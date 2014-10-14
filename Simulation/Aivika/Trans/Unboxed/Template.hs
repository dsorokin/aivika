
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Trans.Unboxed.Template
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It allows specifying unboxed data types using template.
--

module Simulation.Aivika.Trans.Unboxed.Template where

import Simulation.Aivika.Trans.Unboxed
import Simulation.Aivika.Trans.ProtoArray.Unboxed.Template
import Simulation.Aivika.Trans.Internal.Template

instance Unboxed m a => Unboxed (TemplateComp m) a
