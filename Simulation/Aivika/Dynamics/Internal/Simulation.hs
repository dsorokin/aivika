
-- |
-- Module     : Simulation.Aivika.Dynamics.Internal.Simulation
-- Copyright  : Copyright (c) 2009-2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- The module defines the 'Simulation' monad that represents a simulation run.
-- 
module Simulation.Aivika.Dynamics.Internal.Simulation
       (-- * Simulation
        Simulation(..),
        SimulationLift(..),
        Specs(..),
        Method(..),
        Run(..),
        runSimulation,
        runSimulations,
        -- * Utilities
        simulationIndex,
        simulationCount,
        simulationSpecs) where

import Control.Monad
import Control.Monad.Trans

--
-- The Simulation Monad
--
-- A value of the Simulation monad represents a simulation run.
--

-- | A value in the 'Simulation' monad represents a simulation run.
newtype Simulation a = Simulation (Run -> IO a)

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

-- | It indentifies the simulation run.
data Run = Run { runSpecs :: Specs,  -- ^ the simulation specs
                 runIndex :: Int,    -- ^ the current simulation run index
                 runCount :: Int     -- ^ the total number of runs in this experiment
               } deriving (Eq, Ord, Show)

instance Monad Simulation where
  return  = returnS
  m >>= k = bindS m k

returnS :: a -> Simulation a
returnS a = Simulation (\r -> return a)

bindS :: Simulation a -> (a -> Simulation b) -> Simulation b
bindS (Simulation m) k = 
  Simulation $ \r -> 
  do a <- m r
     let Simulation m' = k a
     m' r

-- | Run the simulation using the specified specs.
runSimulation :: Simulation a -> Specs -> IO a
runSimulation (Simulation m) sc =
  m Run { runSpecs = sc,
          runIndex = 1,
          runCount = 1 }

-- | Run the given number of simulations using the specified specs, 
--   where each simulation is distinguished by its index 'simulationIndex'.
runSimulations :: Simulation a -> Specs -> Int -> [IO a]
runSimulations (Simulation m) sc runs = map f [1 .. runs]
  where f i = m Run { runSpecs = sc,
                      runIndex = i,
                      runCount = runs }

-- | Return the run index for the current simulation.
simulationIndex :: Simulation Int
simulationIndex = Simulation $ return . runIndex

-- | Return the number of simulations currently run.
simulationCount :: Simulation Int
simulationCount = Simulation $ return . runCount

-- | Return the simulation specs
simulationSpecs :: Simulation Specs
simulationSpecs = Simulation $ return . runSpecs

instance Functor Simulation where
  fmap = liftMS

liftMS :: (a -> b) -> Simulation a -> Simulation b
{-# INLINE liftMS #-}
liftMS f (Simulation x) =
  Simulation $ \r -> do { a <- x r; return $ f a }

instance MonadIO Simulation where
  liftIO m = Simulation $ const m

-- | A type class to lift the simulation computations in other monads.
class Monad m => SimulationLift m where
  
  -- | Lift the specified 'Simulation' computation in another monad.
  liftSimulation :: Simulation a -> m a