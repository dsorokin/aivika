
{-# LANGUAGE RecursiveDo, TypeSynonymInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Simulation
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'SimulationT' monad transformer that represents a computation
-- within the simulation run.
-- 
module Simulation.Aivika.Trans.Internal.Simulation
       (-- * Simulation
        SimulationT(..),
        Simulation(..),
        SimulationLift(..),
        invokeSimulation,
        runSimulation,
        runSimulations,
        -- * Error Handling
        catchSimulation,
        finallySimulation,
        throwSimulation,
        -- * Internal Parameters
        simulationSession,
        simulationEventQueue,
        -- * Memoization
        memoSimulation) where

import qualified Control.Exception as C
import Control.Exception (IOException, throw, finally)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Data.IORef

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.MonadSim
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter

-- | A value in the 'SimulationT' monad represents a computation
-- within the simulation run.
newtype SimulationT m a = Simulation (RunT m -> m a)

-- | A convenient type synonym.
type Simulation a = SimulationT IO a

instance Monad m => Monad (SimulationT m) where

  {-# INLINE return #-}
  return a = Simulation $ \r -> return a

  {-# INLINE (>>=) #-}
  (Simulation m) >>= k =
    Simulation $ \r -> 
    do a <- m r
       let Simulation m' = k a
       m' r

-- | Run the simulation using the specified specs.
runSimulation :: MonadSim m => SimulationT m a -> SpecsT m -> m a
{-# INLINABLE runSimulation #-}
runSimulation (Simulation m) sc =
  do s <- newSession
     q <- newEventQueue s sc
     g <- newGenerator s $ spcGeneratorType sc
     m Run { runSpecs = sc,
             runSession = s,
             runIndex = 1,
             runCount = 1,
             runEventQueue = q,
             runGenerator = g }

-- | Run the given number of simulations using the specified specs, 
--   where each simulation is distinguished by its index 'simulationIndex'.
runSimulations :: MonadSim m => SimulationT m a -> SpecsT m -> Int -> [m a]
{-# INLINABLE runSimulations #-}
runSimulations (Simulation m) sc runs = map f [1 .. runs]
  where f i = do s <- newSession
                 q <- newEventQueue s sc
                 g <- newGenerator s $ spcGeneratorType sc
                 m Run { runSpecs = sc,
                         runSession = s,
                         runIndex = i,
                         runCount = runs,
                         runEventQueue = q,
                         runGenerator = g }

-- | Return the event queue.
simulationEventQueue :: Monad m => SimulationT m (EventQueueT m)
{-# INLINE simulationEventQueue #-}
simulationEventQueue = Simulation $ return . runEventQueue

-- | Return the simulation session.
simulationSession :: Monad m => SimulationT m (SessionT m)
{-# INLINE simulationSession #-}
simulationSession = Simulation $ return . runSession

instance Functor m => Functor (SimulationT m) where
  
  {-# INLINE fmap #-}
  fmap f (Simulation x) = Simulation $ \r -> fmap f $ x r

instance Applicative m => Applicative (SimulationT m) where
  
  {-# INLINE pure #-}
  pure = Simulation . const . pure
  
  {-# INLINE (<*>) #-}
  (Simulation x) <*> (Simulation y) = Simulation $ \r -> x r <*> y r

liftMS :: Monad m => (a -> b) -> SimulationT m a -> SimulationT m b
{-# INLINE liftMS #-}
liftMS f (Simulation x) =
  Simulation $ \r -> do { a <- x r; return $ f a }

instance MonadTrans SimulationT where

  {-# INLINE lift #-}
  lift = Simulation . const

instance MonadIO m => MonadIO (SimulationT m) where
  
  {-# INLINE liftIO #-}
  liftIO = Simulation . const . liftIO

-- | A type class to lift the simulation computations into other computations.
class SimulationLift t where
  
  -- | Lift the specified 'SimulationT' computation into another computation.
  liftSimulation :: Monad m => SimulationT m a -> t m a

instance SimulationLift SimulationT where
  
  {-# INLINE liftSimulation #-}
  liftSimulation = id

instance ParameterLift SimulationT where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter x) = Simulation x
    
-- | Exception handling within 'SimulationT' computations.
catchSimulation :: MonadSim m => SimulationT m a -> (IOException -> SimulationT m a) -> SimulationT m a
{-# INLINABLE catchSimulation #-}
catchSimulation (Simulation m) h =
  Simulation $ \r -> 
  catchComputation (m r) $ \e ->
  let Simulation m' = h e in m' r
                           
-- | A computation with finalization part like the 'finally' function.
finallySimulation :: MonadSim m => SimulationT m a -> SimulationT m b -> SimulationT m a
{-# INLINABLE finallySimulation #-}
finallySimulation (Simulation m) (Simulation m') =
  Simulation $ \r ->
  finallyComputation (m r) (m' r)

-- | Like the standard 'throw' function.
throwSimulation :: MonadSim m => IOException -> SimulationT m a
{-# INLINABLE throwSimulation #-}
throwSimulation = throw

-- | Invoke the 'SimulationT' computation.
invokeSimulation :: RunT m -> SimulationT m a -> m a
{-# INLINE invokeSimulation #-}
invokeSimulation r (Simulation m) = m r

instance MonadFix m => MonadFix (SimulationT m) where

  {-# INLINE mfix #-}
  mfix f = 
    Simulation $ \r ->
    do { rec { a <- invokeSimulation r (f a) }; return a }

-- | Memoize the 'SimulationT' computation, always returning the same value
-- within a simulation run.
memoSimulation :: MonadSim m => SimulationT m a -> SimulationT m (SimulationT m a)
{-# INLINABLE memoSimulation #-}
memoSimulation m =
  Simulation $ \r ->
  do let s = runSession r
     ref <- newProtoRef s Nothing
     return $ Simulation $ \r ->
       do x <- readProtoRef ref
          case x of
            Just v -> return v
            Nothing ->
              do v <- invokeSimulation r m
                 writeProtoRef ref (Just v)
                 return v
