
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Dynamics
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'DynamicsT' monad transformer representing a time varying polymorphic function. 
--
module Simulation.Aivika.Trans.Internal.Dynamics
       (-- * Dynamics
        DynamicsT(..),
        Dynamics(..),
        DynamicsLift(..),
        invokeDynamics,
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

import qualified Control.Exception as C
import Control.Exception (IOException, throw, finally)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.MonadSim
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation

-- | A value in the 'DynamicsT' monad represents a polymorphic time varying function
-- defined in the whole spectrum of time values as a single entity. It is ideal for
-- numerical approximating integrals.
newtype DynamicsT m a = Dynamics (PointT m -> m a)

-- | A convenient type synonym.
type Dynamics = DynamicsT IO

instance Monad m => Monad (DynamicsT m) where

  {-# INLINE return #-}
  return a = Dynamics $ \p -> return a

  {-# INLINE (>>=) #-}
  (Dynamics m) >>= k =
    Dynamics $ \p -> 
    do a <- m p
       let Dynamics m' = k a
       m' p

-- | Run the 'DynamicsT' computation in the initial time point.
runDynamicsInStartTime :: DynamicsT m a -> SimulationT m a
{-# INLINE runDynamicsInStartTime #-}
runDynamicsInStartTime (Dynamics m) =
  Simulation $ m . integStartPoint

-- | Run the 'DynamicsT' computation in the final time point.
runDynamicsInStopTime :: DynamicsT m a -> SimulationT m a
{-# INLINE runDynamicsInStopTime #-}
runDynamicsInStopTime (Dynamics m) =
  Simulation $ m . integStopPoint

-- | Run the 'DynamicsT' computation in all integration time points.
runDynamicsInIntegTimes :: Monad m => DynamicsT m a -> SimulationT m [m a]
{-# INLINE runDynamicsInIntegTimes #-}
runDynamicsInIntegTimes (Dynamics m) =
  Simulation $ return . map m . integPoints

-- | Run the 'DynamicsT' computation in the specified time point.
runDynamicsInTime :: Double -> DynamicsT m a -> SimulationT m a
{-# INLINE runDynamicsInTime #-}
runDynamicsInTime t (Dynamics m) =
  Simulation $ \r -> m $ pointAt r t

-- | Run the 'DynamicsT' computation in the specified time points.
runDynamicsInTimes :: Monad m => [Double] -> DynamicsT m a -> SimulationT m [m a]
{-# INLINE runDynamicsInTimes #-}
runDynamicsInTimes ts (Dynamics m) =
  Simulation $ \r -> return $ map (m . pointAt r) ts 

instance Functor m => Functor (DynamicsT m) where
  
  {-# INLINE fmap #-}
  fmap f (Dynamics x) = Dynamics $ \p -> fmap f $ x p

instance Applicative m => Applicative (DynamicsT m) where
  
  {-# INLINE pure #-}
  pure = Dynamics . const . pure
  
  {-# INLINE (<*>) #-}
  (Dynamics x) <*> (Dynamics y) = Dynamics $ \p -> x p <*> y p

liftMD :: Monad m => (a -> b) -> DynamicsT m a -> DynamicsT m b
{-# INLINE liftMD #-}
liftMD f (Dynamics x) =
  Dynamics $ \p -> do { a <- x p; return $ f a }

liftM2D :: Monad m => (a -> b -> c) -> DynamicsT m a -> DynamicsT m b -> DynamicsT m c
{-# INLINE liftM2D #-}
liftM2D f (Dynamics x) (Dynamics y) =
  Dynamics $ \p -> do { a <- x p; b <- y p; return $ f a b }

instance (Num a, Monad m) => Num (DynamicsT m a) where
  x + y = liftM2D (+) x y
  x - y = liftM2D (-) x y
  x * y = liftM2D (*) x y
  negate = liftMD negate
  abs = liftMD abs
  signum = liftMD signum
  fromInteger i = return $ fromInteger i

instance (Fractional a, Monad m) => Fractional (DynamicsT m a) where
  x / y = liftM2D (/) x y
  recip = liftMD recip
  fromRational t = return $ fromRational t

instance (Floating a, Monad m) => Floating (DynamicsT m a) where
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

instance MonadTrans DynamicsT where

  {-# INLINE lift #-}
  lift = Dynamics . const

instance MonadIO m => MonadIO (DynamicsT m) where
  
  {-# INLINE liftIO #-}
  liftIO = Dynamics . const . liftIO

-- | A type class to lift the 'DynamicsT' computations into other computations.
class DynamicsLift t where
  
  -- | Lift the specified 'DynamicsT' computation into another computation.
  liftDynamics :: Monad m => DynamicsT m a -> t m a

instance DynamicsLift DynamicsT where
  
  {-# INLINE liftDynamics #-}
  liftDynamics = id

instance SimulationLift DynamicsT where

  {-# INLINE liftSimulation #-}
  liftSimulation (Simulation x) = Dynamics $ x . pointRun 

instance ParameterLift DynamicsT where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter x) = Dynamics $ x . pointRun
  
-- | Exception handling within 'DynamicsT' computations.
catchDynamics :: MonadSim m => DynamicsT m a -> (IOException -> DynamicsT m a) -> DynamicsT m a
{-# INLINABLE catchDynamics #-}
catchDynamics (Dynamics m) h =
  Dynamics $ \p -> 
  catchComputation (m p) $ \e ->
  let Dynamics m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyDynamics :: MonadSim m => DynamicsT m a -> DynamicsT m b -> DynamicsT m a
{-# INLINABLE finallyDynamics #-}
finallyDynamics (Dynamics m) (Dynamics m') =
  Dynamics $ \p ->
  finallyComputation (m p) (m' p)

-- | Like the standard 'throw' function.
throwDynamics :: MonadSim m => IOException -> DynamicsT m a
{-# INLINABLE throwDynamics #-}
throwDynamics = throw

-- | Invoke the 'DynamicsT' computation.
invokeDynamics :: PointT m -> DynamicsT m a -> m a
{-# INLINE invokeDynamics #-}
invokeDynamics p (Dynamics m) = m p

instance MonadFix m => MonadFix (DynamicsT m) where

  {-# INLINE mfix #-}
  mfix f = 
    Dynamics $ \p ->
    do { rec { a <- invokeDynamics p (f a) }; return a }

-- | Computation that returns the current simulation time.
time :: Monad m => DynamicsT m Double
{-# INLINE time #-}
time = Dynamics $ return . pointTime 

-- | Whether the current time is an integration time.
isTimeInteg :: Monad m => DynamicsT m Bool
{-# INLINE isTimeInteg #-}
isTimeInteg = Dynamics $ \p -> return $ pointPhase p >= 0

-- | Return the integration iteration closest to the current simulation time.
integIteration :: Monad m => DynamicsT m Int
{-# INLINE integIteration #-}
integIteration = Dynamics $ return . pointIteration

-- | Return the integration phase for the current simulation time.
-- It is @(-1)@ for non-integration time points.
integPhase :: Monad m => DynamicsT m Int
{-# INLINE integPhase #-}
integPhase = Dynamics $ return . pointPhase
