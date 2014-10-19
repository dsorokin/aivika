
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

  {-# SPECIALISE INLINE return :: a -> Dynamics IO a #-}
  return a = Dynamics $ \p -> return a

  {-# SPECIALISE INLINE (>>=) :: Dynamics IO a -> (a -> Dynamics IO b) -> Dynamics IO b #-}
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
{-# INLINABLE runDynamicsInIntegTimes #-}
{-# SPECIALISE runDynamicsInIntegTimes :: Dynamics IO a -> Simulation IO [IO a] #-}
runDynamicsInIntegTimes (Dynamics m) =
  Simulation $ return . map m . integPoints

-- | Run the 'Dynamics' computation in the specified time point.
runDynamicsInTime :: Double -> Dynamics m a -> Simulation m a
{-# INLINE runDynamicsInTime #-}
runDynamicsInTime t (Dynamics m) =
  Simulation $ \r -> m $ pointAt r t

-- | Run the 'Dynamics' computation in the specified time points.
runDynamicsInTimes :: Monad m => [Double] -> Dynamics m a -> Simulation m [m a]
{-# INLINABLE runDynamicsInTimes #-}
{-# SPECIALISE runDynamicsInTimes :: [Double] -> Dynamics IO a -> Simulation IO [IO a] #-}
runDynamicsInTimes ts (Dynamics m) =
  Simulation $ \r -> return $ map (m . pointAt r) ts 

instance Functor m => Functor (Dynamics m) where
  
  {-# SPECIALISE INLINE fmap :: (a -> b) -> Dynamics IO a -> Dynamics IO b #-}
  fmap f (Dynamics x) = Dynamics $ \p -> fmap f $ x p

instance Applicative m => Applicative (Dynamics m) where
  
  {-# SPECIALISE INLINE pure :: a -> Dynamics IO a #-}
  pure = Dynamics . const . pure
  
  {-# SPECIALISE INLINE (<*>) :: Dynamics IO (a -> b) -> Dynamics IO a -> Dynamics IO b #-}
  (Dynamics x) <*> (Dynamics y) = Dynamics $ \p -> x p <*> y p

liftMD :: Monad m => (a -> b) -> Dynamics m a -> Dynamics m b
{-# INLINABLE liftMD #-}
{-# SPECIALISE liftMD :: (a -> b) -> Dynamics IO a -> Dynamics IO b #-}
liftMD f (Dynamics x) =
  Dynamics $ \p -> do { a <- x p; return $ f a }

liftM2D :: Monad m => (a -> b -> c) -> Dynamics m a -> Dynamics m b -> Dynamics m c
{-# INLINABLE liftM2D #-}
{-# SPECIALISE liftM2D :: (a -> b -> c) -> Dynamics IO a -> Dynamics IO b -> Dynamics IO c #-}
liftM2D f (Dynamics x) (Dynamics y) =
  Dynamics $ \p -> do { a <- x p; b <- y p; return $ f a b }

instance (Num a, Monad m) => Num (Dynamics m a) where

  {-# INLINE (+) #-}
  x + y = liftM2D (+) x y

  {-# INLINE (-) #-}
  x - y = liftM2D (-) x y

  {-# INLINE (*) #-}
  x * y = liftM2D (*) x y

  {-# INLINE negate #-}
  negate = liftMD negate

  {-# INLINE abs #-}
  abs = liftMD abs

  {-# INLINE signum #-}
  signum = liftMD signum

  {-# INLINE fromInteger #-}
  fromInteger i = return $ fromInteger i

instance (Fractional a, Monad m) => Fractional (Dynamics m a) where

  {-# INLINE (/) #-}
  x / y = liftM2D (/) x y

  {-# INLINE recip #-}
  recip = liftMD recip

  {-# INLINE fromRational #-}
  fromRational t = return $ fromRational t

instance (Floating a, Monad m) => Floating (Dynamics m a) where

  {-# INLINE pi #-}
  pi = return pi

  {-# INLINE exp #-}
  exp = liftMD exp

  {-# INLINE log #-}
  log = liftMD log

  {-# INLINE sqrt #-}
  sqrt = liftMD sqrt

  {-# INLINE (**) #-}
  x ** y = liftM2D (**) x y

  {-# INLINE sin #-}
  sin = liftMD sin

  {-# INLINE cos #-}
  cos = liftMD cos

  {-# INLINE tan #-}
  tan = liftMD tan

  {-# INLINE asin #-}
  asin = liftMD asin

  {-# INLINE acos #-}
  acos = liftMD acos

  {-# INLINE atan #-}
  atan = liftMD atan

  {-# INLINE sinh #-}
  sinh = liftMD sinh

  {-# INLINE cosh #-}
  cosh = liftMD cosh

  {-# INLINE tanh #-}
  tanh = liftMD tanh

  {-# INLINE asinh #-}
  asinh = liftMD asinh

  {-# INLINE acosh #-}
  acosh = liftMD acosh

  {-# INLINE atanh #-}
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
{-# SPECIALISE catchDynamics :: Exception e => Dynamics IO a -> (e -> Dynamics IO a) -> Dynamics IO a #-}
catchDynamics (Dynamics m) h =
  Dynamics $ \p -> 
  catchComp (m p) $ \e ->
  let Dynamics m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyDynamics :: Comp m => Dynamics m a -> Dynamics m b -> Dynamics m a
{-# INLINABLE finallyDynamics #-}
{-# SPECIALISE finallyDynamics :: Dynamics IO a -> Dynamics IO b -> Dynamics IO a #-}
finallyDynamics (Dynamics m) (Dynamics m') =
  Dynamics $ \p ->
  finallyComp (m p) (m' p)

-- | Like the standard 'throw' function.
throwDynamics :: (Comp m, Exception e) => e -> Dynamics m a
{-# INLINABLE throwDynamics #-}
{-# SPECIALISE throwDynamics :: Exception e => e -> Dynamics IO a #-}
throwDynamics = throw

instance MonadFix m => MonadFix (Dynamics m) where

  {-# SPECIALISE INLINE mfix :: (a -> Dynamics IO a) -> Dynamics IO a #-}
  mfix f = 
    Dynamics $ \p ->
    do { rec { a <- invokeDynamics p (f a) }; return a }

-- | Computation that returns the current simulation time.
time :: Monad m => Dynamics m Double
{-# INLINABLE time #-}
{-# SPECIALISE time :: Dynamics IO Double #-}
time = Dynamics $ return . pointTime 

-- | Whether the current time is an integration time.
isTimeInteg :: Monad m => Dynamics m Bool
{-# INLINABLE isTimeInteg #-}
{-# SPECIALISE isTimeInteg :: Dynamics IO Bool #-}
isTimeInteg = Dynamics $ \p -> return $ pointPhase p >= 0

-- | Return the integration iteration closest to the current simulation time.
integIteration :: Monad m => Dynamics m Int
{-# INLINABLE integIteration #-}
{-# SPECIALISE integIteration :: Dynamics IO Int #-}
integIteration = Dynamics $ return . pointIteration

-- | Return the integration phase for the current simulation time.
-- It is @(-1)@ for non-integration time points.
integPhase :: Monad m => Dynamics m Int
{-# INLINABLE integPhase #-}
{-# SPECIALISE integPhase :: Dynamics IO Int #-}
integPhase = Dynamics $ return . pointPhase
