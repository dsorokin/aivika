
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.Session
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It identfies a current simulation session usually associated with the current simulation run.
--
module Simulation.Aivika.Trans.Session
       (Sessionning(..),
        Session(..)) where

import Data.IORef

-- | A monad within which computation we can create and work with a simulation session.
class (Functor m, Monad m) => Sessionning m where
  
  -- | A simulation session.
  data Session m :: *

  -- | A marker that exists with the session and which can be compared for equality.
  data SessionMarker m :: *

  -- | Create a new session.
  newSession :: m (Session m)

  -- | Create a new marker within the current session.
  newSessionMarker :: Session m -> m (SessionMarker m)

  -- | Compare two markers for equality.
  equalSessionMarker :: SessionMarker m -> SessionMarker m -> Bool

instance Sessionning IO where

  data Session IO = Session

  newtype SessionMarker IO = SessionMarker (IORef ())

  {-# SPECIALISE INLINE newSession :: IO (Session IO) #-}
  newSession = return Session

  {-# SPECIALISE INLINE newSessionMarker :: Session IO -> IO (SessionMarker IO) #-}
  newSessionMarker session = fmap SessionMarker $ newIORef ()

  {-# SPECIALISE INLINE equalSessionMarker :: SessionMarker IO -> SessionMarker IO -> Bool #-}
  equalSessionMarker (SessionMarker x) (SessionMarker y) = x == y

instance Sessionning m => Eq (SessionMarker m) where

  {-# INLINE (==) #-}
  (==) = equalSessionMarker
