
{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.ProtoRef.Template
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a prototype of mutable references.
--
module Simulation.Aivika.Trans.ProtoRef.Template
       (ProtoReferring(..),
        ProtoRef(..)) where

import Control.Monad.Trans

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.Session.Template
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Template

instance ProtoReferring m => ProtoReferring (TemplateComp m) where

  newtype ProtoRef (TemplateComp m) a = TemplateProtoRef (ProtoRef m a)

  newProtoRef (TemplateSession session) a = lift $ fmap TemplateProtoRef (newProtoRef session a)

  readProtoRef (TemplateProtoRef x) = lift $ TemplateProtoRef (readProtoRef x)

  writeProtoRef (TemplateProtoRef x) = lift $ TemplateProtoRef (writeProtoRef x)

  modifyProtoRef (TemplateProtoRef x) = lift $ TemplateProtoRef (modifyProtoRef x)
