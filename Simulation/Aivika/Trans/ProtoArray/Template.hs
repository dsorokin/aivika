
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.ProtoArray.Template
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It allows creating mutable arrays using template.
--
module Simulation.Aivika.Trans.ProtoArray.Template
       (ProtoArraying(..)) where

import Data.Array

import Control.Monad.Trans

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.Session.Template
import Simulation.Aivika.Trans.ProtoRef.Template
import Simulation.Aivika.Trans.ProtoArray
import Simulation.Aivika.Trans.Internal.Template

instance ProtoArraying m => ProtoArraying (TemplateComp m) where

  newtype ProtoArray (TemplateComp m) a = TemplateProtoArray (ProtoArray m a)

  protoArrayCount (TemplateProtoArray x) = lift $ protoArrayCount x

  newProtoArray (TemplateSession s) n a = lift $ fmap TemplateProtoArray (newProtoArray s n a)

  newProtoArray_ (TemplateSession s) n = lift $ fmap TemplateProtoArray (newProtoArray_ s n)

  readProtoArray (TemplateProtoArray x) i = lift $ readProtoArray x i

  writeProtoArray (TemplateProtoArray x) i a = lift $ writeProtoArray x i a

  protoArrayToList (TemplateProtoArray x) = lift $ protoArrayToList x

  protoArrayFromList xs = lift $ fmap TemplateProtoArray (protoArrayFromList xs)

  freezeProtoArray (TemplateProtoArray x) = lift $ freezeProtoArray x
