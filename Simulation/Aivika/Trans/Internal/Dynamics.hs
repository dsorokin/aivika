
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Dynamics
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'Dynamics' monad transformer representing a time varying polymorphic function. 
--
module Simulation.Aivika.Trans.Internal.Dynamics
       (-- * Dynamics
        DynamicsLift(..),
        runDynamicsInStartTime,
        runDynamicsInStopTime,
        runDynamicsInIntegTimes,
        runDynamicsInTime,
        runDynamicsInTimes,
        -- * Error Handling
        catchDynamics,
        finallyDynamics,
        throwDynamics,
        -- * Simulation Time
        time,
        isTimeInteg,
        integIteration,
        integPhase) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation

instance Monad m => Monad (Dynamics m) where

  {-# INLINE return #-}
  return a = Dynamics $ \p -> return a

  {-# INLINE (>>=) #-}
  (Dynamics m) >>= k =
    Dynamics $ \p -> 
    do a <- m p
       let Dynamics m' = k a
       m' p

-- | Run the 'Dynamics' computation in the initial time point.
runDynamicsInStartTime :: Dynamics m a -> Simulation m a
{-# INLINE runDynamicsInStartTime #-}
runDynamicsInStartTime (Dynamics m) =
  Simulation $ m . integStartPoint

-- | Run the 'Dynamics' computation in the final time point.
runDynamicsInStopTime :: Dynamics m a -> Simulation m a
{-# INLINE runDynamicsInStopTime #-}
runDynamicsInStopTime (Dynamics m) =
  Simulation $ m . integStopPoint

-- | Run the 'Dynamics' computation in all integration time points.
runDynamicsInIntegTimes :: Monad m => Dynamics m a -> Simulation m [m a]
{-# INLINE runDynamicsInIntegTimes #-}
runDynamicsInIntegTimes (Dynamics m) =
  Simulation $ return . map m . integPoints

-- | Run the 'Dynamics' computation in the specified time point.
runDynamicsInTime :: Double -> Dynamics m a -> Simulation m a
{-# INLINE runDynamicsInTime #-}
runDynamicsInTime t (Dynamics m) =
  Simulation $ \r -> m $ pointAt r t

-- | Run the 'Dynamics' computation in the specified time points.
runDynamicsInTimes :: Monad m => [Double] -> Dynamics m a -> Simulation m [m a]
{-# INLINE runDynamicsInTimes #-}
runDynamicsInTimes ts (Dynamics m) =
  Simulation $ \r -> return $ map (m . pointAt r) ts 

instance Functor m => Functor (Dynamics m) where
  
  {-# INLINE fmap #-}
  fmap f (Dynamics x) = Dynamics $ \p -> fmap f $ x p

instance Applicative m => Applicative (Dynamics m) where
  
  {-# INLINE pure #-}
  pure = Dynamics . const . pure
  
  {-# INLINE (<*>) #-}
  (Dynamics x) <*> (Dynamics y) = Dynamics $ \p -> x p <*> y p

liftMD :: Monad m => (a -> b) -> Dynamics m a -> Dynamics m b
{-# INLINE liftMD #-}
liftMD f (Dynamics x) =
  Dynamics $ \p -> do { a <- x p; return $ f a }

liftM2D :: Monad m => (a -> b -> c) -> Dynamics m a -> Dynamics m b -> Dynamics m c
{-# INLINE liftM2D #-}
liftM2D f (Dynamics x) (Dynamics y) =
  Dynamics $ \p -> do { a <- x p; b <- y p; return $ f a b }

instance (Num a, Monad m) => Num (Dynamics m a) where
  x + y = liftM2D (+) x y
  x - y = liftM2D (-) x y
  x * y = liftM2D (*) x y
  negate = liftMD negate
  abs = liftMD abs
  signum = liftMD signum
  fromInteger i = return $ fromInteger i

instance (Fractional a, Monad m) => Fractional (Dynamics m a) where
  x / y = liftM2D (/) x y
  recip = liftMD recip
  fromRational t = return $ fromRational t

instance (Floating a, Monad m) => Floating (Dynamics m a) where
  pi = return pi
  exp = liftMD exp
  log = liftMD log
  sqrt = liftMD sqrt
  x ** y = liftM2D (**) x y
  sin = liftMD sin
  cos = liftMD cos
  tan = liftMD tan
  asin = liftMD asin
  acos = liftMD acos
  atan = liftMD atan
  sinh = liftMD sinh
  cosh = liftMD cosh
  tanh = liftMD tanh
  asinh = liftMD asinh
  acosh = liftMD acosh
  atanh = liftMD atanh

instance MonadTrans Dynamics where

  {-# INLINE lift #-}
  lift = Dynamics . const

instance MonadIO m => MonadIO (Dynamics m) where
  
  {-# INLINE liftIO #-}
  liftIO = Dynamics . const . liftIO

instance CompTrans Dynamics where

  {-# INLINE liftComp #-}
  liftComp = Dynamics . const

-- | A type class to lift the 'Dynamics' computations into other computations.
class DynamicsLift t where
  
  -- | Lift the specified 'Dynamics' computation into another computation.
  liftDynamics :: Comp m => Dynamics m a -> t m a

instance DynamicsLift Dynamics where
  
  {-# INLINE liftDynamics #-}
  liftDynamics = id

instance SimulationLift Dynamics where

  {-# INLINE liftSimulation #-}
  liftSimulation (Simulation x) = Dynamics $ x . pointRun 

instance ParameterLift Dynamics where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter x) = Dynamics $ x . pointRun
  
-- | Exception handling within 'Dynamics' computations.
catchDynamics :: (Comp m, Exception e) => Dynamics m a -> (e -> Dynamics m a) -> Dynamics m a
{-# INLINABLE catchDynamics #-}
catchDynamics (Dynamics m) h =
  Dynamics $ \p -> 
  catchComp (m p) $ \e ->
  let Dynamics m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyDynamics :: Comp m => Dynamics m a -> Dynamics m b -> Dynamics m a
{-# INLINABLE finallyDynamics #-}
finallyDynamics (Dynamics m) (Dynamics m') =
  Dynamics $ \p ->
  finallyComp (m p) (m' p)

-- | Like the standard 'throw' function.
throwDynamics :: (Comp m, Exception e) => e -> Dynamics m a
{-# INLINABLE throwDynamics #-}
throwDynamics = throw

instance MonadFix m => MonadFix (Dynamics m) where

  {-# INLINE mfix #-}
  mfix f = 
    Dynamics $ \p ->
    do { rec { a <- invokeDynamics p (f a) }; return a }

-- | Computation that returns the current simulation time.
time :: Monad m => Dynamics m Double
{-# INLINE time #-}
time = Dynamics $ return . pointTime 

-- | Whether the current time is an integration time.
isTimeInteg :: Monad m => Dynamics m Bool
{-# INLINE isTimeInteg #-}
isTimeInteg = Dynamics $ \p -> return $ pointPhase p >= 0

-- | Return the integration iteration closest to the current simulation time.
integIteration :: Monad m => Dynamics m Int
{-# INLINE integIteration #-}
integIteration = Dynamics $ return . pointIteration

-- | Return the integration phase for the current simulation time.
-- It is @(-1)@ for non-integration time points.
integPhase :: Monad m => Dynamics m Int
{-# INLINE integPhase #-}
integPhase = Dynamics $ return . pointPhase
