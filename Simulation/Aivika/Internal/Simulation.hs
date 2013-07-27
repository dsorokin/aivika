
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Internal.Simulation
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines the 'Simulation' monad that represents a simulation run.
-- 
module Simulation.Aivika.Internal.Simulation
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
        -- * Utilities
        simulationIndex,
        simulationCount,
        simulationSpecs,
        simulationEventQueue) where

import qualified Control.Exception as C
import Control.Exception (IOException, throw, finally)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix

import Simulation.Aivika.Internal.Specs

-- | A value in the 'Simulation' monad represents something that
-- doesn't change within the simulation run but may change for
-- other runs.
--
-- This monad is ideal for representing the external
-- parameters for the model, when the Monte-Carlo simulation
-- is used. Also this monad is useful for defining some
-- actions that should occur only once within the simulation run,
-- for example, setting of the integral with help of recursive
-- equations.
--
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
     m Run { runSpecs = sc,
             runIndex = 1,
             runCount = 1,
             runEventQueue = q }

-- | Run the given number of simulations using the specified specs, 
--   where each simulation is distinguished by its index 'simulationIndex'.
runSimulations :: Simulation a -> Specs -> Int -> [IO a]
runSimulations (Simulation m) sc runs = map f [1 .. runs]
  where f i = do q <- newEventQueue sc
                 m Run { runSpecs = sc,
                         runIndex = i,
                         runCount = runs,
                         runEventQueue = q }

-- | Return the run index for the current simulation.
simulationIndex :: Simulation Int
simulationIndex = Simulation $ return . runIndex

-- | Return the number of simulations currently run.
simulationCount :: Simulation Int
simulationCount = Simulation $ return . runCount

-- | Return the simulation specs.
simulationSpecs :: Simulation Specs
simulationSpecs = Simulation $ return . runSpecs

-- | Return the event queue.
simulationEventQueue :: Simulation EventQueue
simulationEventQueue = Simulation $ return . runEventQueue

instance Functor Simulation where
  fmap = liftMS

instance Eq (Simulation a) where
  x == y = error "Can't compare simulation runs." 

instance Show (Simulation a) where
  showsPrec _ x = showString "<< Simulation >>"

liftMS :: (a -> b) -> Simulation a -> Simulation b
{-# INLINE liftMS #-}
liftMS f (Simulation x) =
  Simulation $ \r -> do { a <- x r; return $ f a }

liftM2S :: (a -> b -> c) -> Simulation a -> Simulation b -> Simulation c
{-# INLINE liftM2S #-}
liftM2S f (Simulation x) (Simulation y) =
  Simulation $ \r -> do { a <- x r; b <- y r; return $ f a b }

instance (Num a) => Num (Simulation a) where
  x + y = liftM2S (+) x y
  x - y = liftM2S (-) x y
  x * y = liftM2S (*) x y
  negate = liftMS negate
  abs = liftMS abs
  signum = liftMS signum
  fromInteger i = return $ fromInteger i

instance (Fractional a) => Fractional (Simulation a) where
  x / y = liftM2S (/) x y
  recip = liftMS recip
  fromRational t = return $ fromRational t

instance (Floating a) => Floating (Simulation a) where
  pi = return pi
  exp = liftMS exp
  log = liftMS log
  sqrt = liftMS sqrt
  x ** y = liftM2S (**) x y
  sin = liftMS sin
  cos = liftMS cos
  tan = liftMS tan
  asin = liftMS asin
  acos = liftMS acos
  atan = liftMS atan
  sinh = liftMS sinh
  cosh = liftMS cosh
  tanh = liftMS tanh
  asinh = liftMS asinh
  acosh = liftMS acosh
  atanh = liftMS atanh

instance MonadIO Simulation where
  liftIO m = Simulation $ const m

-- | A type class to lift the simulation computations to other monads.
class Monad m => SimulationLift m where
  
  -- | Lift the specified 'Simulation' computation to another monad.
  liftSimulation :: Simulation a -> m a

instance SimulationLift Simulation where
  liftSimulation = id
    
-- | Exception handling within 'Simulation' computations.
catchSimulation :: Simulation a -> (IOException -> Simulation a) -> Simulation a
catchSimulation (Simulation m) h =
  Simulation $ \r -> 
  C.catch (m r) $ \e ->
  let Simulation m' = h e in m' r
                           
-- | A computation with finalization part like the 'finally' function.
finallySimulation :: Simulation a -> Simulation b -> Simulation a
finallySimulation (Simulation m) (Simulation m') =
  Simulation $ \r ->
  C.finally (m r) (m' r)

-- | Like the standard 'throw' function.
throwSimulation :: IOException -> Simulation a
throwSimulation = throw

-- | Invoke the 'Simulation' computation.
invokeSimulation :: Run -> Simulation a -> IO a
{-# INLINE invokeSimulation #-}
invokeSimulation r (Simulation m) = m r

instance MonadFix Simulation where
  mfix f = 
    Simulation $ \r ->
    do { rec { a <- invokeSimulation r (f a) }; return a }  
