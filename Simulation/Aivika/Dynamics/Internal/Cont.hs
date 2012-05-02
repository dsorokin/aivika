
-- |
-- Module     : Simulation.Aivika.Dynamics.Internal.Cont
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- The 'Cont' monad is a variation of the standard Cont monad, where
-- the result of applying the continuation is a dynamic process.
--
module Simulation.Aivika.Dynamics.Internal.Cont
       (Cont(..),
        runCont) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics

-- | The 'Cont' type is similar to the standard Cont monad but only
-- the continuation uses a dynamic process as a result.
newtype Cont a = Cont ((a -> Dynamics ()) -> Dynamics ())

instance Monad Cont where
  return  = returnC
  m >>= k = bindC m k

instance SimulationLift Cont where
  liftSimulation = liftSC

instance DynamicsLift Cont where
  liftDynamics = liftDC

instance Functor Cont where
  fmap = liftM

instance MonadIO Cont where
  liftIO = liftIOC 

returnC :: a -> Cont a
{-# INLINE returnC #-}
returnC a = Cont $ \c -> c a
                          
bindC :: Cont a -> (a -> Cont b) -> Cont b
{-# INLINE bindC #-}
bindC (Cont m) k = Cont $ \c -> m (\a -> let Cont m' = k a in m' c)

-- | Run the 'Cont' computation.
runCont :: Cont a -> (a -> Dynamics ()) -> Dynamics ()
{-# INLINE runCont #-}
runCont (Cont m) = m

-- | Lift the 'Simulation' computation.
liftSC :: Simulation a -> Cont a
{-# INLINE liftSC #-}
liftSC (Simulation m) = 
  Cont $ \c ->
  Dynamics $ \p ->
  do a <- m $ pointRun p
     let Dynamics m' = c a
     m' p
     
-- | Lift the 'Dynamics' computation.
liftDC :: Dynamics a -> Cont a
{-# INLINE liftDC #-}
liftDC (Dynamics m) =
  Cont $ \c ->
  Dynamics $ \p ->
  do a <- m p
     let Dynamics m' = c a
     m' p
     
-- | Lift the IO computation.
liftIOC :: IO a -> Cont a
{-# INLINE liftIOC #-}
liftIOC m =
  Cont $ \c ->
  Dynamics $ \p ->
  do a <- m
     let Dynamics m' = c a
     m' p
  