
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Internal.Dynamics
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'Dynamics' monad representing a time varying polymorphic function. 
--
module Simulation.Aivika.Internal.Dynamics
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
        -- * Simulation Time
        time,
        isTimeInteg,
        integIteration,
        integPhase,
        -- * Debugging
        traceDynamics) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Debug.Trace

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Internal.Simulation

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

instance Applicative Dynamics where
  pure = return
  (<*>) = ap

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

instance ParameterLift Dynamics where
  liftParameter = liftDP

instance SimulationLift Dynamics where
  liftSimulation = liftDS
    
liftDP :: Parameter a -> Dynamics a
{-# INLINE liftDP #-}
liftDP (Parameter m) =
  Dynamics $ \p -> m $ pointRun p
    
liftDS :: Simulation a -> Dynamics a
{-# INLINE liftDS #-}
liftDS (Simulation m) =
  Dynamics $ \p -> m $ pointRun p

-- | A type class to lift the 'Dynamics' computations to other computations.
class DynamicsLift m where
  
  -- | Lift the specified 'Dynamics' computation to another computation.
  liftDynamics :: Dynamics a -> m a

instance DynamicsLift Dynamics where
  liftDynamics = id
  
-- | Exception handling within 'Dynamics' computations.
catchDynamics :: Exception e => Dynamics a -> (e -> Dynamics a) -> Dynamics a
catchDynamics (Dynamics m) h =
  Dynamics $ \p -> 
  catch (m p) $ \e ->
  let Dynamics m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyDynamics :: Dynamics a -> Dynamics b -> Dynamics a
finallyDynamics (Dynamics m) (Dynamics m') =
  Dynamics $ \p ->
  finally (m p) (m' p)

-- | Like the standard 'throw' function.
throwDynamics :: Exception e => e -> Dynamics a
throwDynamics = throw

-- | Invoke the 'Dynamics' computation.
invokeDynamics :: Point -> Dynamics a -> IO a
{-# INLINE invokeDynamics #-}
invokeDynamics p (Dynamics m) = m p

instance MonadFix Dynamics where
  mfix f = 
    Dynamics $ \p ->
    do { rec { a <- invokeDynamics p (f a) }; return a }

-- | Computation that returns the current simulation time.
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

-- | Show the debug message with the current simulation time.
traceDynamics :: String -> Dynamics a -> Dynamics a
traceDynamics message m =
  Dynamics $ \p ->
  trace ("t = " ++ show (pointTime p) ++ ": " ++ message) $
  invokeDynamics p m
