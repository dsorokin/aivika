
-- |
-- Module     : Simulation.Aivika.Dynamics.Internal.Dynamics
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- The module defines the 'Dynamics' monad representing an abstract dynamic 
-- process, i.e. a time varying polymorphic function. 
-- 
-- This is a key point of the Aivika simulation library. With help of this monad 
-- we can simulate the system of ordinary differential equations (ODEs) of 
-- System Dynamics, define the tasks of Discrete Event Simulation (DES) supporting 
-- different paradigms. Also we can use the Agent-based Modeling. Thus, 
-- we can create hybrid simulation models.
--
module Simulation.Aivika.Dynamics.Internal.Dynamics
       (-- * Dynamics
        Dynamics(..),
        Point(..),
        Specs(..),
        Method(..),
        Run(..),
        runDynamics1,
        runDynamics1_,
        runDynamics,
        runDynamics_,
        runDynamicsIO,
        runDynamicsSeries1,
        runDynamicsSeries1_,
        runDynamicsSeries,
        runDynamicsSeries_,
        printDynamics1,
        printDynamics,
        -- * Utilities
        basicTime,
        iterationBnds,
        iterationHiBnd,
        iterationLoBnd,
        phaseBnds,
        phaseHiBnd,
        phaseLoBnd) where

import Control.Monad
import Control.Monad.Trans

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

-- | It defines the simulation specs.
data Specs = Specs { spcStartTime :: Double,    -- ^ the start time
                     spcStopTime :: Double,     -- ^ the stop time
                     spcDT :: Double,           -- ^ the integration time step
                     spcMethod :: Method        -- ^ the integration method
                   } deriving (Eq, Ord, Show)

-- | It defines the integration method.
data Method = Euler          -- ^ Euler's method
            | RungeKutta2    -- ^ the 2nd order Runge-Kutta method
            | RungeKutta4    -- ^ the 4th order Runge-Kutta method
            deriving (Eq, Ord, Show)

-- | It defined the simulation run as part of some experiment.
data Run = Run { runIndex :: Int,    -- ^ the current simulation run
                 runCount :: Int     -- ^ the total number of runs in this experiment
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

subrunDynamics1 :: Dynamics a -> Specs -> Run -> IO a
subrunDynamics1 (Dynamics m) sc r =
  do let n = iterationHiBnd sc
         t = basicTime sc n 0
     m Point { pointSpecs = sc,
               pointRun = r,
               pointTime = t,
               pointIteration = n,
               pointPhase = 0 }

subrunDynamics1_ :: Dynamics a -> Specs -> Run -> IO ()
subrunDynamics1_ (Dynamics m) sc r =
  do let n = iterationHiBnd sc
         t = basicTime sc n 0
     m Point { pointSpecs = sc,
               pointRun = r,
               pointTime = t,
               pointIteration = n,
               pointPhase = 0 }
     return ()

subrunDynamics :: Dynamics a -> Specs -> Run -> [IO a]
subrunDynamics (Dynamics m) sc r =
  do let (nl, nu) = iterationBnds sc
         point n = Point { pointSpecs = sc,
                           pointRun = r,
                           pointTime = basicTime sc n 0,
                           pointIteration = n,
                           pointPhase = 0 }
     map (m . point) [nl .. nu]

subrunDynamics_ :: Dynamics a -> Specs -> Run -> IO ()
subrunDynamics_ (Dynamics m) sc r =
  do let (nl, nu) = iterationBnds sc
         point n = Point { pointSpecs = sc,
                           pointRun = r,
                           pointTime = basicTime sc n 0,
                           pointIteration = n,
                           pointPhase = 0 }
     mapM_ (m . point) [nl .. nu]

-- | Run the simulation and return the result in the last 
-- time point using the specified simulation specs.
runDynamics1 :: Dynamics (Dynamics a) -> Specs -> IO a
runDynamics1 (Dynamics m) sc = 
  do let r = Run { runIndex = 1, runCount = 1 }
     d <- m Point { pointSpecs = sc,
                    pointRun = r,
                    pointTime = spcStartTime sc,
                    pointIteration = 0,
                    pointPhase = 0 }
     subrunDynamics1 d sc r

-- | Run the simulation and return the result in the last 
-- time point using the specified simulation specs.
runDynamics1_ :: Dynamics (Dynamics a) -> Specs -> IO ()
runDynamics1_ (Dynamics m) sc = 
  do let r = Run { runIndex = 1, runCount = 1 }
     d <- m Point { pointSpecs = sc,
                    pointRun = r,
                    pointTime = spcStartTime sc,
                    pointIteration = 0,
                    pointPhase = 0 }
     subrunDynamics1_ d sc r

-- | Run the simulation and return the results in all 
-- integration time points using the specified simulation specs.
runDynamics :: Dynamics (Dynamics a) -> Specs -> IO [a]
runDynamics (Dynamics m) sc = 
  do let r = Run { runIndex = 1, runCount = 1 }
     d <- m Point { pointSpecs = sc,
                    pointRun = r,
                    pointTime = spcStartTime sc,
                    pointIteration = 0,
                    pointPhase = 0 }
     sequence $ subrunDynamics d sc r

-- | Run the simulation and return the results in all 
-- integration time points using the specified simulation specs.
runDynamics_ :: Dynamics (Dynamics a) -> Specs -> IO ()
runDynamics_ (Dynamics m) sc = 
  do let r = Run { runIndex = 1, runCount = 1 }
     d <- m Point { pointSpecs = sc,
                    pointRun = r,
                    pointTime = spcStartTime sc,
                    pointIteration = 0,
                    pointPhase = 0 }
     sequence_ $ subrunDynamics d sc r

-- | Run the simulation and return the results in all 
-- integration time points using the specified simulation specs.
runDynamicsIO :: Dynamics (Dynamics a) -> Specs -> IO [IO a]
runDynamicsIO (Dynamics m) sc =
  do let r = Run { runIndex = 1, runCount = 1 }
     d <- m Point { pointSpecs = sc,
                    pointRun = r,
                    pointTime = spcStartTime sc,
                    pointIteration = 0,
                    pointPhase = 0 }
     return $ subrunDynamics d sc r

-- | Run an experiment consisting of the given number of simulations, where each 
-- model is created and then requested in the last integration time point using 
-- the specified specs.
runDynamicsSeries1_ :: Dynamics (Dynamics a) -> Specs -> Int -> [IO ()]
runDynamicsSeries1_ (Dynamics m) sc runs = map f [1 .. runs]
  where f i =
          do let r = Run { runIndex = i, runCount = runs }
             d <- m Point { pointSpecs = sc,
                            pointRun = r,
                            pointTime = spcStartTime sc,
                            pointIteration = 0,
                            pointPhase = 0 }
             subrunDynamics1_ d sc r

-- | Run an experiment consisting of the given number of simulations, where each 
-- model is created and then requested sequentially in all integration time points 
-- using the specified specs.
runDynamicsSeries_ :: Dynamics (Dynamics a) -> Specs -> Int -> [IO ()]
runDynamicsSeries_ (Dynamics m) sc runs = map f [1 .. runs]
  where f i =
          do let r = Run { runIndex = i, runCount = runs }
             d <- m Point { pointSpecs = sc,
                            pointRun = r,
                            pointTime = spcStartTime sc,
                            pointIteration = 0,
                            pointPhase = 0 }
             subrunDynamics_ d sc r

-- | Run an experiment consisting of the given number of simulations, where each 
-- model is created and then requested in the last integration time point using 
-- the specified specs.
runDynamicsSeries1 :: Dynamics (Dynamics a) -> Specs -> Int -> [IO a]
runDynamicsSeries1 (Dynamics m) sc runs = map f [1 .. runs]
  where f i =
          do let r = Run { runIndex = i, runCount = runs }
             d <- m Point { pointSpecs = sc,
                            pointRun = r,
                            pointTime = spcStartTime sc,
                            pointIteration = 0,
                            pointPhase = 0 }
             subrunDynamics1 d sc r

-- | Run an experiment consisting of the given number of simulations, where each 
-- model is created and then requested sequentially in all integration time points 
-- using the specified specs.
runDynamicsSeries :: Dynamics (Dynamics a) -> Specs -> Int -> [IO [a]]
runDynamicsSeries (Dynamics m) sc runs = map f [1 .. runs]
  where f i =
          do let r = Run { runIndex = i, runCount = runs }
             d <- m Point { pointSpecs = sc,
                            pointRun = r,
                            pointTime = spcStartTime sc,
                            pointIteration = 0,
                            pointPhase = 0 }
             sequence $ subrunDynamics d sc r

-- | Run the simulation and print the result in the last 
-- time point using the specified simulation specs.
printDynamics1 :: (Show a) => Dynamics (Dynamics a) -> Specs -> IO ()
printDynamics1 m sc = runDynamics1 m sc >>= print

-- | Run the simulation and print lazily the results in all
-- integration time points using the specified simulation specs.
printDynamics :: (Show a) => Dynamics (Dynamics a) -> Specs -> IO ()
printDynamics m sc = runDynamicsIO m sc >>= loop
  where loop [] = return ()
        loop (x : xs) = do { a <- x; print a; loop xs }

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
