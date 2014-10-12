
{-# LANGUAGE RecursiveDo, TypeSynonymInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Simulation
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'Simulation' monad transformer that represents a computation
-- within the simulation run.
-- 
module Simulation.Aivika.Trans.Internal.Simulation
       (-- * Simulation
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
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter

-- | A value in the 'Simulation' monad represents a computation
-- within the simulation run.
newtype Simulation m a = Simulation (Run m -> m a)

instance Monad m => Monad (Simulation m) where

  {-# INLINE return #-}
  return a = Simulation $ \r -> return a

  {-# INLINE (>>=) #-}
  (Simulation m) >>= k =
    Simulation $ \r -> 
    do a <- m r
       let Simulation m' = k a
       m' r

-- | Run the simulation using the specified specs.
runSimulation :: Comp m => Simulation m a -> Specs m -> m a
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
runSimulations :: Comp m => Simulation m a -> Specs m -> Int -> [m a]
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
simulationEventQueue :: Monad m => Simulation m (EventQueue m)
{-# INLINE simulationEventQueue #-}
simulationEventQueue = Simulation $ return . runEventQueue

-- | Return the simulation session.
simulationSession :: Monad m => Simulation m (Session m)
{-# INLINE simulationSession #-}
simulationSession = Simulation $ return . runSession

instance Functor m => Functor (Simulation m) where
  
  {-# INLINE fmap #-}
  fmap f (Simulation x) = Simulation $ \r -> fmap f $ x r

instance Applicative m => Applicative (Simulation m) where
  
  {-# INLINE pure #-}
  pure = Simulation . const . pure
  
  {-# INLINE (<*>) #-}
  (Simulation x) <*> (Simulation y) = Simulation $ \r -> x r <*> y r

liftMS :: Monad m => (a -> b) -> Simulation m a -> Simulation m b
{-# INLINE liftMS #-}
liftMS f (Simulation x) =
  Simulation $ \r -> do { a <- x r; return $ f a }

instance MonadTrans Simulation where

  {-# INLINE lift #-}
  lift = Simulation . const

instance MonadIO m => MonadIO (Simulation m) where
  
  {-# INLINE liftIO #-}
  liftIO = Simulation . const . liftIO

-- | A type class to lift the simulation computations into other computations.
class SimulationLift t where
  
  -- | Lift the specified 'Simulation' computation into another computation.
  liftSimulation :: Comp m => Simulation m a -> t m a

instance SimulationLift Simulation where
  
  {-# INLINE liftSimulation #-}
  liftSimulation = id

instance ParameterLift Simulation where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter x) = Simulation x
    
-- | Exception handling within 'Simulation' computations.
catchSimulation :: Comp m => Simulation m a -> (IOException -> Simulation m a) -> Simulation m a
{-# INLINABLE catchSimulation #-}
catchSimulation (Simulation m) h =
  Simulation $ \r -> 
  catchComp (m r) $ \e ->
  let Simulation m' = h e in m' r
                           
-- | A computation with finalization part like the 'finally' function.
finallySimulation :: Comp m => Simulation m a -> Simulation m b -> Simulation m a
{-# INLINABLE finallySimulation #-}
finallySimulation (Simulation m) (Simulation m') =
  Simulation $ \r ->
  finallyComp (m r) (m' r)

-- | Like the standard 'throw' function.
throwSimulation :: Comp m => IOException -> Simulation m a
{-# INLINABLE throwSimulation #-}
throwSimulation = throw

-- | Invoke the 'Simulation' computation.
invokeSimulation :: Run m -> Simulation m a -> m a
{-# INLINE invokeSimulation #-}
invokeSimulation r (Simulation m) = m r

instance MonadFix m => MonadFix (Simulation m) where

  {-# INLINE mfix #-}
  mfix f = 
    Simulation $ \r ->
    do { rec { a <- invokeSimulation r (f a) }; return a }

-- | Memoize the 'Simulation' computation, always returning the same value
-- within a simulation run.
memoSimulation :: Comp m => Simulation m a -> Simulation m (Simulation m a)
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
