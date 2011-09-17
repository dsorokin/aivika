
-- |
-- Module     : Simulation.Aivika.Dynamics.Cont
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- The 'Cont' monad looks somewhere like the standard ContT monad transformer 
-- parameterized by the 'Dynamics' monad, although this analogy is not strong. 
-- The main idea is to represent the continuation as a dynamic process varying 
-- in time.
--
module Simulation.Aivika.Dynamics.Cont
       (Cont(..),
        runCont) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Lift

-- | The 'Cont' type is similar to the standard Cont monad but only
-- the continuation is represented as a dynamic process varying in time.
newtype Cont a = Cont (Dynamics (a -> IO ()) -> Dynamics ())

instance Monad Cont where
  return  = returnC
  m >>= k = bindC m k

instance Lift Cont where
  liftD = liftC

instance Functor Cont where
  fmap = liftM

instance MonadIO Cont where
  liftIO = liftIOC 

returnC :: a -> Cont a
{-# INLINE returnC #-}
returnC a = 
  Cont $ \(Dynamics c) -> 
  Dynamics $ \p -> 
  do cont' <- c p
     cont' a
                          
bindC :: Cont a -> (a -> Cont b) -> Cont b
{-# INLINE bindC #-}
bindC (Cont m) k =
  Cont $ \c ->
  m $ Dynamics $ \p -> 
  let cont' a = let (Cont m') = k a
                    (Dynamics u) = m' c
                in u p
  in return cont'

-- | Run the 'Cont' computation.
runCont :: Cont a -> IO (a -> IO ()) -> Dynamics ()
{-# INLINE runCont #-}
runCont (Cont m) f = m $ Dynamics $ const f

-- | Lift the 'Dynamics' computation.
liftC :: Dynamics a -> Cont a
{-# INLINE liftC #-}
liftC (Dynamics m) =
  Cont $ \(Dynamics c) ->
  Dynamics $ \p ->
  do cont' <- c p
     a <- m p
     cont' a
     
-- | Lift the IO computation.
liftIOC :: IO a -> Cont a
{-# INLINE liftIOC #-}
liftIOC m =
  Cont $ \(Dynamics c) ->
  Dynamics $ \p ->
  do cont' <- c p
     a <- m
     cont' a
  