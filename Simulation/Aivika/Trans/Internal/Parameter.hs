
{-# LANGUAGE RecursiveDo, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Parameter
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'Parameter' monad transformer that allows representing the model
-- parameters. For example, they can be used when running the Monte-Carlo simulation.
-- 
-- In general, this monad is very useful for representing a computation which is external
-- relative to the model itself.
--
module Simulation.Aivika.Trans.Internal.Parameter
       (-- * Parameter
        Parameter(..),
        ParameterLift(..),
        invokeParameter,
        runParameter,
        runParameters,
        -- * Error Handling
        catchParameter,
        finallyParameter,
        throwParameter,
        -- * Predefined Parameters
        simulationIndex,
        simulationCount,
        simulationSpecs,
        simulationSession,
        simulationEventQueue,
        starttime,
        stoptime,
        dt,
        generatorParameter,
        -- * Memoization
        memoParameter,
        -- * Utilities
        tableParameter) where

import Control.Exception (IOException, throw, finally)
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Data.IORef
import qualified Data.IntMap as M
import Data.Array

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs

-- | The 'Parameter' monad that allows specifying the model parameters.
-- For example, they can be used when running the Monte-Carlo simulation.
-- 
-- In general, this monad is very useful for representing a computation which is external
-- relative to the model itself.
newtype Parameter m a = Parameter (Run m -> m a)

instance Monad m => Monad (Parameter m) where

  {-# INLINE return #-}
  return a = Parameter $ \r -> return a

  {-# INLINE (>>=) #-}
  (Parameter m) >>= k =
    Parameter $ \r -> 
    do a <- m r
       let Parameter m' = k a
       m' r

-- | Run the parameter using the specified specs.
runParameter :: Comp m => Parameter m a -> Specs m -> m a
{-# INLINABLE runParameter #-}
runParameter (Parameter m) sc =
  do s <- newSession
     q <- newEventQueue s sc
     g <- newGenerator s $ spcGeneratorType sc
     m Run { runSpecs = sc,
             runSession = s,
             runIndex = 1,
             runCount = 1,
             runEventQueue = q,
             runGenerator = g }

-- | Run the given number of parameters using the specified specs, 
--   where each parameter is distinguished by its index 'parameterIndex'.
runParameters :: Comp m => Parameter m a -> Specs m -> Int -> [m a]
{-# INLINABLE runParameters #-}
runParameters (Parameter m) sc runs = map f [1 .. runs]
  where f i = do s <- newSession
                 q <- newEventQueue s sc
                 g <- newGenerator s $ spcGeneratorType sc
                 m Run { runSpecs = sc,
                         runSession = s,
                         runIndex = i,
                         runCount = runs,
                         runEventQueue = q,
                         runGenerator = g }

-- | Return the run index for the current simulation.
simulationIndex :: Monad m => Parameter m Int
{-# INLINE simulationIndex #-}
simulationIndex = Parameter $ return . runIndex

-- | Return the number of simulations currently run.
simulationCount :: Monad m => Parameter m Int
{-# INLINE simulationCount #-}
simulationCount = Parameter $ return . runCount

-- | Return the simulation specs.
simulationSpecs :: Monad m => Parameter m (Specs m)
{-# INLINE simulationSpecs #-}
simulationSpecs = Parameter $ return . runSpecs

-- | Return the random number generator for the simulation run.
generatorParameter :: Monad m => Parameter m (Generator m)
{-# INLINE generatorParameter #-}
generatorParameter = Parameter $ return . runGenerator

instance Functor m => Functor (Parameter m) where
  
  {-# INLINE fmap #-}
  fmap f (Parameter x) = Parameter $ \r -> fmap f $ x r

instance Applicative m => Applicative (Parameter m) where
  
  {-# INLINE pure #-}
  pure = Parameter . const . pure
  
  {-# INLINE (<*>) #-}
  (Parameter x) <*> (Parameter y) = Parameter $ \r -> x r <*> y r

liftMP :: Monad m => (a -> b) -> Parameter m a -> Parameter m b
{-# INLINE liftMP #-}
liftMP f (Parameter x) =
  Parameter $ \r -> do { a <- x r; return $ f a }

liftM2P :: Monad m => (a -> b -> c) -> Parameter m a -> Parameter m b -> Parameter m c
{-# INLINE liftM2P #-}
liftM2P f (Parameter x) (Parameter y) =
  Parameter $ \r -> do { a <- x r; b <- y r; return $ f a b }

instance (Num a, Monad m) => Num (Parameter m a) where
  x + y = liftM2P (+) x y
  x - y = liftM2P (-) x y
  x * y = liftM2P (*) x y
  negate = liftMP negate
  abs = liftMP abs
  signum = liftMP signum
  fromInteger i = return $ fromInteger i

instance (Fractional a, Monad m) => Fractional (Parameter m a) where
  x / y = liftM2P (/) x y
  recip = liftMP recip
  fromRational t = return $ fromRational t

instance (Floating a, Monad m) => Floating (Parameter m a) where
  pi = return pi
  exp = liftMP exp
  log = liftMP log
  sqrt = liftMP sqrt
  x ** y = liftM2P (**) x y
  sin = liftMP sin
  cos = liftMP cos
  tan = liftMP tan
  asin = liftMP asin
  acos = liftMP acos
  atan = liftMP atan
  sinh = liftMP sinh
  cosh = liftMP cosh
  tanh = liftMP tanh
  asinh = liftMP asinh
  acosh = liftMP acosh
  atanh = liftMP atanh

instance MonadTrans Parameter where

  {-# INLINE lift #-}
  lift = Parameter . const

instance MonadIO m => MonadIO (Parameter m) where
  
  {-# INLINE liftIO #-}
  liftIO = Parameter . const . liftIO

instance CompTrans Parameter where

  {-# INLINE liftComp #-}
  liftComp = Parameter . const

-- | A type class to lift the parameters into other computations.
class ParameterLift t where
  
  -- | Lift the specified 'Parameter' computation into another computation.
  liftParameter :: Comp m => Parameter m a -> t m a

instance ParameterLift Parameter where
  
  {-# INLINE liftParameter #-}
  liftParameter = id
    
-- | Exception handling within 'Parameter' computations.
catchParameter :: Comp m => Parameter m a -> (IOException -> Parameter m a) -> Parameter m a
{-# INLINABLE catchParameter #-}
catchParameter (Parameter m) h =
  Parameter $ \r -> 
  catchComp (m r) $ \e ->
  let Parameter m' = h e in m' r
                           
-- | A computation with finalization part like the 'finally' function.
finallyParameter :: Comp m => Parameter m a -> Parameter m b -> Parameter m a
{-# INLINABLE finallyParameter #-}
finallyParameter (Parameter m) (Parameter m') =
  Parameter $ \r ->
  finallyComp (m r) (m' r)

-- | Like the standard 'throw' function.
throwParameter :: Comp m => IOException -> Parameter m a
{-# INLINABLE throwParameter #-}
throwParameter = throw

-- | Invoke the 'Parameter' computation.
invokeParameter :: Run m -> Parameter m a -> m a
{-# INLINE invokeParameter #-}
invokeParameter r (Parameter m) = m r

instance MonadFix m => MonadFix (Parameter m) where

  {-# INLINE mfix #-}
  mfix f = 
    Parameter $ \r ->
    do { rec { a <- invokeParameter r (f a) }; return a }

-- | Memoize the 'Parameter' computation, always returning the same value
-- within a simulation run. However, the value will be recalculated for other
-- simulation runs. Also it is thread-safe when different simulation runs
-- are executed in parallel on physically different operating system threads.
memoParameter :: Parameter IO a -> IO (Parameter IO a)
memoParameter x = 
  do lock <- newMVar ()
     dict <- newIORef M.empty
     return $ Parameter $ \r ->
       do let i = runIndex r
          m <- readIORef dict
          if M.member i m
            then do let Just v = M.lookup i m
                    return v
            else withMVar lock $ 
                 \() -> do { m <- readIORef dict;
                             if M.member i m
                             then do let Just v = M.lookup i m
                                     return v
                             else do v <- invokeParameter r x
                                     writeIORef dict $ M.insert i v m
                                     return v }

-- | Return a parameter which value is taken consequently from the specified table
-- based on the run index of the current simulation starting from zero. After all
-- values from the table are used, it takes again the first value of the table,
-- then the second one and so on.
tableParameter :: Monad m => Array Int a -> Parameter m a
{-# INLINABLE tableParameter #-}
tableParameter t =
  do i <- simulationIndex
     return $ t ! (((i - i1) `mod` n) + i1)
  where (i1, i2) = bounds t
        n = i2 - i1 + 1

-- | Computation that returns the start simulation time.
starttime :: Monad m => Parameter m Double
{-# INLINE starttime #-}
starttime =
  Parameter $ return . spcStartTime . runSpecs

-- | Computation that returns the final simulation time.
stoptime :: Monad m => Parameter m Double
{-# INLINE stoptime #-}
stoptime =
  Parameter $ return . spcStopTime . runSpecs

-- | Computation that returns the integration time step.
dt :: Monad m => Parameter m Double
{-# INLINE dt #-}
dt =
  Parameter $ return . spcDT . runSpecs

-- | Return the event queue.
simulationEventQueue :: Monad m => Parameter m (EventQueue m)
{-# INLINE simulationEventQueue #-}
simulationEventQueue =
  Parameter $ return . runEventQueue

-- | Return the simulation session.
simulationSession :: Monad m => Parameter m (Session m)
{-# INLINE simulationSession #-}
simulationSession =
  Parameter $ return . runSession
