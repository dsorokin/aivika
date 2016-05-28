
{-# LANGUAGE RecursiveDo, ExistentialQuantification, DeriveDataTypeable #-}

-- |
-- Module     : Simulation.Aivika.Internal.Simulation
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This is an internal implementation module that should never be used directly.
--
-- The module defines the 'Simulation' monad that represents a computation within
-- the simulation run.
-- 
module Simulation.Aivika.Internal.Simulation
       (-- * Simulation
        Simulation(..),
        SimulationLift(..),
        invokeSimulation,
        runSimulation,
        runSimulations,
        runSimulationByIndex,
        -- * Error Handling
        catchSimulation,
        finallySimulation,
        throwSimulation,
        -- * Utilities
        simulationEventQueue,
        -- * Memoization
        memoSimulation,
        -- * Exceptions
        SimulationException(..),
        SimulationAbort(..),
        SimulationRetry(..)) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Data.IORef
import Data.Typeable

import Simulation.Aivika.Generator
import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter

-- | A value in the 'Simulation' monad represents a computation
-- within the simulation run.
newtype Simulation a = Simulation (Run -> IO a)

instance Monad Simulation where
  return  = returnS
  m >>= k = bindS m k

returnS :: a -> Simulation a
{-# INLINE returnS #-}
returnS a = Simulation (\r -> return a)

bindS :: Simulation a -> (a -> Simulation b) -> Simulation b
{-# INLINE bindS #-}
bindS (Simulation m) k = 
  Simulation $ \r -> 
  do a <- m r
     let Simulation m' = k a
     m' r

-- | Run the simulation using the specified specs.
runSimulation :: Simulation a -> Specs -> IO a
runSimulation (Simulation m) sc =
  do q <- newEventQueue sc
     g <- newGenerator $ spcGeneratorType sc
     m Run { runSpecs = sc,
             runIndex = 1,
             runCount = 1,
             runEventQueue = q,
             runGenerator = g }

-- | Run the simulation by the specified specs and run index in series.
runSimulationByIndex :: Simulation a
                        -- ^ the simulation model
                        -> Specs
                        -- ^ the simulation specs
                        -> Int
                        -- ^ the number of runs in series
                        -> Int
                        -- ^ the index of the current run (started from 1)
                        -> IO a
runSimulationByIndex (Simulation m) sc runs index =
  do q <- newEventQueue sc
     g <- newGenerator $ spcGeneratorType sc
     m Run { runSpecs = sc,
             runIndex = index,
             runCount = runs,
             runEventQueue = q,
             runGenerator = g }

-- | Run the given number of simulations using the specified specs, 
--   where each simulation is distinguished by its index 'simulationIndex'.
runSimulations :: Simulation a -> Specs -> Int -> [IO a]
runSimulations (Simulation m) sc runs = map f [1 .. runs]
  where f i = do q <- newEventQueue sc
                 g <- newGenerator $ spcGeneratorType sc
                 m Run { runSpecs = sc,
                         runIndex = i,
                         runCount = runs,
                         runEventQueue = q,
                         runGenerator = g }

-- | Return the event queue.
simulationEventQueue :: Simulation EventQueue
simulationEventQueue = Simulation $ return . runEventQueue

instance Functor Simulation where
  fmap = liftMS

instance Applicative Simulation where
  pure = return
  (<*>) = ap

liftMS :: (a -> b) -> Simulation a -> Simulation b
{-# INLINE liftMS #-}
liftMS f (Simulation x) =
  Simulation $ \r -> do { a <- x r; return $ f a }

instance MonadIO Simulation where
  liftIO m = Simulation $ const m

-- | A type class to lift the simulation computations to other computations.
class SimulationLift m where
  
  -- | Lift the specified 'Simulation' computation to another computation.
  liftSimulation :: Simulation a -> m a

instance SimulationLift Simulation where
  liftSimulation = id

instance ParameterLift Simulation where
  liftParameter = liftPS

liftPS :: Parameter a -> Simulation a
{-# INLINE liftPS #-}
liftPS (Parameter x) =
  Simulation x
    
-- | Exception handling within 'Simulation' computations.
catchSimulation :: Exception e => Simulation a -> (e -> Simulation a) -> Simulation a
catchSimulation (Simulation m) h =
  Simulation $ \r -> 
  catch (m r) $ \e ->
  let Simulation m' = h e in m' r
                           
-- | A computation with finalization part like the 'finally' function.
finallySimulation :: Simulation a -> Simulation b -> Simulation a
finallySimulation (Simulation m) (Simulation m') =
  Simulation $ \r ->
  finally (m r) (m' r)

-- | Like the standard 'throw' function.
throwSimulation :: Exception e => e -> Simulation a
throwSimulation = throw

-- | Invoke the 'Simulation' computation.
invokeSimulation :: Run -> Simulation a -> IO a
{-# INLINE invokeSimulation #-}
invokeSimulation r (Simulation m) = m r

instance MonadFix Simulation where
  mfix f = 
    Simulation $ \r ->
    do { rec { a <- invokeSimulation r (f a) }; return a }  

-- | Memoize the 'Simulation' computation, always returning the same value
-- within a simulation run.
memoSimulation :: Simulation a -> Simulation (Simulation a)
memoSimulation m =
  do ref <- liftIO $ newIORef Nothing
     return $ Simulation $ \r ->
       do x <- readIORef ref
          case x of
            Just v -> return v
            Nothing ->
              do v <- invokeSimulation r m
                 writeIORef ref (Just v)
                 return v

-- | The root of simulation exceptions.
data SimulationException = forall e . Exception e => SimulationException e
                           -- ^ A particular simulation exception.
                         deriving Typeable

instance Show SimulationException where
  show (SimulationException e) = show e

instance Exception SimulationException

-- | An exception that signals of aborting the simulation.
data SimulationAbort = SimulationAbort String
                       -- ^ The exception to abort the simulation.
                     deriving (Show, Typeable)

-- | An exception that signals that the current computation should be retried
-- as possible, which feature may be supported by the simulation engine or not.
data SimulationRetry = SimulationRetry String
                       -- ^ The exception to retry the computation.
                     deriving (Show, Typeable)

instance Exception SimulationAbort where
  
  toException = toException . SimulationException
  fromException x = do { SimulationException a <- fromException x; cast a }

instance Exception SimulationRetry where
  
  toException = toException . SimulationException
  fromException x = do { SimulationException a <- fromException x; cast a }
