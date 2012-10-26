
-- |
-- Module     : Simulation.Aivika.Dynamics.Internal.Dynamics
-- Copyright  : Copyright (c) 2009-2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines the 'Dynamics' monad representing an abstract dynamic 
-- process, i.e. a time varying polymorphic function. 
--
module Simulation.Aivika.Dynamics.Internal.Dynamics
       (-- * Dynamics
        Dynamics(..),
        DynamicsLift(..),
        Point(..),
        runDynamicsInStartTime,
        runDynamicsInStopTime,
        runDynamicsInIntegTimes,
        runDynamicsInTime,
        runDynamicsInTimes,
        -- * Error Handling
        catchDynamics,
        finallyDynamics,
        -- * Utilities
        basicTime,
        iterationBnds,
        iterationHiBnd,
        iterationLoBnd,
        phaseBnds,
        phaseHiBnd,
        phaseLoBnd) where

import qualified Control.Exception as C
import Control.Exception (IOException)

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.Internal.Simulation

--
-- The Dynamics Monad
--
-- A value of the Dynamics monad represents an abstract dynamic 
-- process, i.e. a time varying polymorphic function. This is 
-- a key point of the Aivika simulation library.
--

-- | A value in the 'Dynamics' monad represents a dynamic process, i.e.
-- a polymorphic time varying function.
newtype Dynamics a = Dynamics (Point -> IO a)

-- | It defines the simulation point appended with the additional information.
data Point = Point { pointSpecs :: Specs,    -- ^ the simulation specs
                     pointRun :: Run,        -- ^ the simulation run
                     pointTime :: Double,    -- ^ the current time
                     pointIteration :: Int,  -- ^ the current iteration
                     pointPhase :: Int       -- ^ the current phase
                   } deriving (Eq, Ord, Show)
           
-- | Returns the iterations starting from zero.
iterations :: Specs -> [Int]
iterations sc = [i1 .. i2] where
  i1 = 0
  i2 = round ((spcStopTime sc - 
               spcStartTime sc) / spcDT sc)

-- | Returns the first and last iterations.
iterationBnds :: Specs -> (Int, Int)
iterationBnds sc = (0, round ((spcStopTime sc - 
                               spcStartTime sc) / spcDT sc))

-- | Returns the first iteration, i.e. zero.
iterationLoBnd :: Specs -> Int
iterationLoBnd sc = 0

-- | Returns the last iteration.
iterationHiBnd :: Specs -> Int
iterationHiBnd sc = round ((spcStopTime sc - 
                            spcStartTime sc) / spcDT sc)

-- | Returns the phases for the specified simulation specs starting from zero.
phases :: Specs -> [Int]
phases sc = 
  case spcMethod sc of
    Euler -> [0]
    RungeKutta2 -> [0, 1]
    RungeKutta4 -> [0, 1, 2, 3]

-- | Returns the first and last phases.
phaseBnds :: Specs -> (Int, Int)
phaseBnds sc = 
  case spcMethod sc of
    Euler -> (0, 0)
    RungeKutta2 -> (0, 1)
    RungeKutta4 -> (0, 3)

-- | Returns the first phase, i.e. zero.
phaseLoBnd :: Specs -> Int
phaseLoBnd sc = 0
                  
-- | Returns the last phase, 1 for Euler's method, 2 for RK2 and 4 for RK4.
phaseHiBnd :: Specs -> Int
phaseHiBnd sc = 
  case spcMethod sc of
    Euler -> 0
    RungeKutta2 -> 1
    RungeKutta4 -> 3

-- | Returns a simulation time for the integration point specified by 
-- the specs, iteration and phase.
basicTime :: Specs -> Int -> Int -> Double
basicTime sc n ph =
  if ph < 0 then 
    error "Incorrect phase: basicTime"
  else
    spcStartTime sc + n' * spcDT sc + delta (spcMethod sc) ph 
      where n' = fromInteger (toInteger n)
            delta Euler       0 = 0
            delta RungeKutta2 0 = 0
            delta RungeKutta2 1 = spcDT sc
            delta RungeKutta4 0 = 0
            delta RungeKutta4 1 = spcDT sc / 2
            delta RungeKutta4 2 = spcDT sc / 2
            delta RungeKutta4 3 = spcDT sc

instance Monad Dynamics where
  return  = returnD
  m >>= k = bindD m k

returnD :: a -> Dynamics a
returnD a = Dynamics (\p -> return a)

bindD :: Dynamics a -> (a -> Dynamics b) -> Dynamics b
bindD (Dynamics m) k = 
  Dynamics $ \p -> 
  do a <- m p
     let Dynamics m' = k a
     m' p

-- | Run the dynamic process in the initial simulation point.
runDynamicsInStartTime :: Dynamics a -> Simulation a
runDynamicsInStartTime (Dynamics m) =
  Simulation $ \r ->
  do let sc = runSpecs r 
         n  = 0
         t  = spcStartTime sc
     m Point { pointSpecs = sc,
               pointRun = r,
               pointTime = t,
               pointIteration = n,
               pointPhase = 0 }

-- | Run the dynamic process in the final simulation point.
runDynamicsInStopTime :: Dynamics a -> Simulation a
runDynamicsInStopTime (Dynamics m) =
  Simulation $ \r ->
  do let sc = runSpecs r 
         n  = iterationHiBnd sc
         t  = basicTime sc n 0
     m Point { pointSpecs = sc,
               pointRun = r,
               pointTime = t,
               pointIteration = n,
               pointPhase = 0 }

-- | Run the dynamic process in all integration time points
runDynamicsInIntegTimes :: Dynamics a -> Simulation [IO a]
runDynamicsInIntegTimes (Dynamics m) =
  Simulation $ \r ->
  do let sc = runSpecs r
         (nl, nu) = iterationBnds sc
         point n = Point { pointSpecs = sc,
                           pointRun = r,
                           pointTime = basicTime sc n 0,
                           pointIteration = n,
                           pointPhase = 0 }
     return $ map (m . point) [nl .. nu]

-- | Run the dynamic process in the specified time point.
runDynamicsInTime :: Double -> Dynamics a -> Simulation a
runDynamicsInTime t (Dynamics m) =
  Simulation $ \r ->
  do let sc = runSpecs r
         t0 = spcStartTime sc
         dt = spcDT sc
         n  = fromInteger $ toInteger $ floor ((t - t0) / dt)
     m Point { pointSpecs = sc,
               pointRun = r,
               pointTime = t,
               pointIteration = n,
               pointPhase = -1 }

-- | Run the dynamic process in the specified time points.
runDynamicsInTimes :: [Double] -> Dynamics a -> Simulation [IO a]
runDynamicsInTimes ts (Dynamics m) =
  Simulation $ \r ->
  do let sc = runSpecs r
         t0 = spcStartTime sc
         dt = spcDT sc
         point t =
           let n = fromInteger $ toInteger $ floor ((t - t0) / dt)
           in Point { pointSpecs = sc,
                      pointRun = r,
                      pointTime = t,
                      pointIteration = n,
                      pointPhase = -1 }
     return $ map (m . point) ts

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
                           
-- | A computation with finalization part.
finallyDynamics :: Dynamics a -> Dynamics b -> Dynamics a
finallyDynamics (Dynamics m) (Dynamics m') =
  Dynamics $ \p ->
  C.finally (m p) (m' p)
