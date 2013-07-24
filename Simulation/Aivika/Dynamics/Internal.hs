
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Dynamics.Internal
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines the 'Dynamics' monad representing a time varying polymorphic function. 
--
module Simulation.Aivika.Dynamics.Internal
       (-- * Dynamics
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
        -- * Time parameters
        starttime,
        stoptime,
        dt,
        time,
        isTimeInteg,
        integIteration,
        integPhase) where

import qualified Control.Exception as C
import Control.Exception (IOException, throw, finally)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix

import Simulation.Aivika.Specs.Internal
import Simulation.Aivika.Simulation.Internal

-- | A value in the 'Dynamics' monad represents a polymorphic time varying function.
newtype Dynamics a = Dynamics (Point -> IO a)

instance Monad Dynamics where
  return  = returnD
  m >>= k = bindD m k

returnD :: a -> Dynamics a
{-# INLINE returnD #-}
returnD a = Dynamics (\p -> return a)

bindD :: Dynamics a -> (a -> Dynamics b) -> Dynamics b
{-# INLINE bindD #-}
bindD (Dynamics m) k = 
  Dynamics $ \p -> 
  do a <- m p
     let Dynamics m' = k a
     m' p

-- | Run the 'Dynamics' computation in the initial time point.
runDynamicsInStartTime :: Dynamics a -> Simulation a
runDynamicsInStartTime (Dynamics m) =
  Simulation $ m . integStartPoint

-- | Run the 'Dynamics' computation in the final time point.
runDynamicsInStopTime :: Dynamics a -> Simulation a
runDynamicsInStopTime (Dynamics m) =
  Simulation $ m . integStopPoint

-- | Run the 'Dynamics' computation in all integration time points.
runDynamicsInIntegTimes :: Dynamics a -> Simulation [IO a]
runDynamicsInIntegTimes (Dynamics m) =
  Simulation $ return . map m . integPoints

-- | Run the 'Dynamics' computation in the specified time point.
runDynamicsInTime :: Double -> Dynamics a -> Simulation a
runDynamicsInTime t (Dynamics m) =
  Simulation $ \r -> m $ pointAt r t

-- | Run the 'Dynamics' computation in the specified time points.
runDynamicsInTimes :: [Double] -> Dynamics a -> Simulation [IO a]
runDynamicsInTimes ts (Dynamics m) =
  Simulation $ \r -> return $ map (m . pointAt r) ts 

instance Functor Dynamics where
  fmap = liftMD

instance Eq (Dynamics a) where
  x == y = error "Can't compare dynamics." 

instance Show (Dynamics a) where
  showsPrec _ x = showString "<< Dynamics >>"

liftMD :: (a -> b) -> Dynamics a -> Dynamics b
{-# INLINE liftMD #-}
liftMD f (Dynamics x) =
  Dynamics $ \p -> do { a <- x p; return $ f a }

liftM2D :: (a -> b -> c) -> Dynamics a -> Dynamics b -> Dynamics c
{-# INLINE liftM2D #-}
liftM2D f (Dynamics x) (Dynamics y) =
  Dynamics $ \p -> do { a <- x p; b <- y p; return $ f a b }

instance (Num a) => Num (Dynamics a) where
  x + y = liftM2D (+) x y
  x - y = liftM2D (-) x y
  x * y = liftM2D (*) x y
  negate = liftMD negate
  abs = liftMD abs
  signum = liftMD signum
  fromInteger i = return $ fromInteger i

instance (Fractional a) => Fractional (Dynamics a) where
  x / y = liftM2D (/) x y
  recip = liftMD recip
  fromRational t = return $ fromRational t

instance (Floating a) => Floating (Dynamics a) where
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

instance MonadIO Dynamics where
  liftIO m = Dynamics $ const m

instance SimulationLift Dynamics where
  liftSimulation = liftDS
    
liftDS :: Simulation a -> Dynamics a
{-# INLINE liftDS #-}
liftDS (Simulation m) =
  Dynamics $ \p -> m $ pointRun p

-- | A type class to lift the 'Dynamics' computations in other monads.
class Monad m => DynamicsLift m where
  
  -- | Lift the specified 'Dynamics' computation in another monad.
  liftDynamics :: Dynamics a -> m a
  
-- | Exception handling within 'Dynamics' computations.
catchDynamics :: Dynamics a -> (IOException -> Dynamics a) -> Dynamics a
catchDynamics (Dynamics m) h =
  Dynamics $ \p -> 
  C.catch (m p) $ \e ->
  let Dynamics m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyDynamics :: Dynamics a -> Dynamics b -> Dynamics a
finallyDynamics (Dynamics m) (Dynamics m') =
  Dynamics $ \p ->
  C.finally (m p) (m' p)

-- | Like the standard 'throw' function.
throwDynamics :: IOException -> Dynamics a
throwDynamics = throw

-- | Invoke the 'Dynamics' computation.
invokeDynamics :: Dynamics a -> Point -> IO a
{-# INLINE invokeDynamics #-}
invokeDynamics (Dynamics m) p = m p

instance MonadFix Dynamics where
  mfix f = 
    Dynamics $ \p ->
    do { rec { a <- invokeDynamics (f a) p }; return a }

-- | Return the start simulation time.
starttime :: Dynamics Double
starttime = Dynamics $ return . spcStartTime . pointSpecs

-- | Return the stop simulation time.
stoptime :: Dynamics Double
stoptime = Dynamics $ return . spcStopTime . pointSpecs

-- | Return the integration time step.
dt :: Dynamics Double
dt = Dynamics $ return . spcDT . pointSpecs

-- | Return the current simulation time.
time :: Dynamics Double
time = Dynamics $ return . pointTime 

-- | Whether the current time is an integration time.
isTimeInteg :: Dynamics Bool
isTimeInteg = Dynamics $ \p -> return $ pointPhase p >= 0

-- | Return the integration iteration closest to the current simulation time.
integIteration :: Dynamics Int
integIteration = Dynamics $ return . pointIteration

-- | Return the integration phase for the current simulation time.
-- It is @(-1)@ for non-integration time points.
integPhase :: Dynamics Int
integPhase = Dynamics $ return . pointPhase