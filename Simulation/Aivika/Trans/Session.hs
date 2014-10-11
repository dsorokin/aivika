
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
class Sessionning m where
  
  -- | A simulation session.
  data SessionT m :: *

  -- | A marker that exists with the session and which can be compared for equality.
  data SessionTMarker m :: *

  -- | Create a new session.
  newSession :: m (SessionT m)

  -- | Create a new marker within the current session.
  newSessionMarker :: SessionT m -> m (SessionTMarker m)

  -- | Compare two markers for equality.
  equalSessionMarker :: SessionTMarker m -> SessionTMarker m -> Bool

instance Sessionning IO where

  data SessionT IO = Session

  newtype SessionTMarker IO = SessionMarker (IORef ())

  {-# SPECIALISE INLINE newSession :: IO Session #-}
  newSession = return Session

  {-# SPECIALISE INLINE newSessionMarker :: Session -> IO SessionMarker #-}
  newSessionMarker session = fmap SessionMarker $ newIORef ()

  {-# SPECIALISE INLINE equalSessionMarker :: SessionMarker -> SessionMarker -> Bool #-}
  equalSessionMarker (SessionMarker x) (SessionMarker y) = x == y

-- | A convenient type synonym.
type Session = SessionT IO

-- | A convenient type synonym.
type SessionMarker = SessionTMarker IO

instance Sessionning m => Eq (SessionTMarker m) where

  {-# INLINE (==) #-}
  (==) = equalSessionMarker
