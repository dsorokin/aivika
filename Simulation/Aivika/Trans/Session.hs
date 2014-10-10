
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

-- | A monad within which computation we can create and work with a simulation session.
class Sessionning m where
  
  -- | A simulation session.
  data SessionT m :: *

  -- | Create a new session.
  newSession :: m (SessionT m)

instance Sessionning IO where

  data SessionT IO = Session

  newSession = return Session
  {-# SPECIALIZE INLINE newSession :: IO Session #-}

-- | A convenient type synonym.
type Session = SessionT IO
