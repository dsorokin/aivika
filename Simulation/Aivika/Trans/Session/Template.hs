
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.Session.Template
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It allows creating new simulation sessions using template.
--
module Simulation.Aivika.Trans.Session.Template
       (Sessionning(..),
        Session(..)) where

import Control.Monad.Trans

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.Internal.Template

instance Sessionning m => Sessionning (TemplateComp m) where

  newtype Session (TemplateComp m) = TemplateSession (Session m)

  newtype SessionMarker (TemplateComp m) = TemplateSessionMarker (SessionMarker m)

  newSession = lift $ fmap TemplateSession newSession

  newSessionMarker (TemplateSession x) = lift $ fmap TemplateSessionMarker (newSessionMarker x)

  equalSessionMarker (TemplateSessionMarker x) (TemplateSessionMarker y) = equalSessionMarker x y
