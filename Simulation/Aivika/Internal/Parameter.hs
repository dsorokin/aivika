
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Internal.Parameter
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines the 'Parameter' monad that allows representing the model
-- parameters. For example, they can be used when running the Monte-Carlo simulation.
-- 
module Simulation.Aivika.Internal.Parameter
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
        parameterIndex,
        parameterCount,
        parameterSpecs,
        -- * Memoization
        memoParameter,
        -- * Utilities
        newTableParameter,
        newIndexedParameter) where

import qualified Control.Exception as C
import Control.Exception (IOException, throw, finally)
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix

import Data.IORef
import qualified Data.Map as M
import Data.Array

import Simulation.Aivika.Internal.Specs

-- | The 'Parameter' monad that allows specifying the model parameters.
--   For example, they can be used when running the Monte-Carlo simulation.
newtype Parameter a = Parameter (Run -> IO a)

instance Monad Parameter where
  return  = returnP
  m >>= k = bindP m k

returnP :: a -> Parameter a
{-# INLINE returnP #-}
returnP a = Parameter (\r -> return a)

bindP :: Parameter a -> (a -> Parameter b) -> Parameter b
{-# INLINE bindP #-}
bindP (Parameter m) k = 
  Parameter $ \r -> 
  do a <- m r
     let Parameter m' = k a
     m' r

-- | Run the parameter using the specified specs.
runParameter :: Parameter a -> Specs -> IO a
runParameter (Parameter m) sc =
  do q <- newEventQueue sc
     m Run { runSpecs = sc,
             runIndex = 1,
             runCount = 1,
             runEventQueue = q }

-- | Run the given number of parameters using the specified specs, 
--   where each parameter is distinguished by its index 'parameterIndex'.
runParameters :: Parameter a -> Specs -> Int -> [IO a]
runParameters (Parameter m) sc runs = map f [1 .. runs]
  where f i = do q <- newEventQueue sc
                 m Run { runSpecs = sc,
                         runIndex = i,
                         runCount = runs,
                         runEventQueue = q }

-- | Return the run index for the current simulation.
parameterIndex :: Parameter Int
parameterIndex = Parameter $ return . runIndex

-- | Return the number of simulations currently run.
parameterCount :: Parameter Int
parameterCount = Parameter $ return . runCount

-- | Return the simulation specs.
parameterSpecs :: Parameter Specs
parameterSpecs = Parameter $ return . runSpecs

instance Functor Parameter where
  fmap = liftMP

instance Eq (Parameter a) where
  x == y = error "Can't compare parameters." 

instance Show (Parameter a) where
  showsPrec _ x = showString "<< Parameter >>"

liftMP :: (a -> b) -> Parameter a -> Parameter b
{-# INLINE liftMP #-}
liftMP f (Parameter x) =
  Parameter $ \r -> do { a <- x r; return $ f a }

liftM2P :: (a -> b -> c) -> Parameter a -> Parameter b -> Parameter c
{-# INLINE liftM2P #-}
liftM2P f (Parameter x) (Parameter y) =
  Parameter $ \r -> do { a <- x r; b <- y r; return $ f a b }

instance (Num a) => Num (Parameter a) where
  x + y = liftM2P (+) x y
  x - y = liftM2P (-) x y
  x * y = liftM2P (*) x y
  negate = liftMP negate
  abs = liftMP abs
  signum = liftMP signum
  fromInteger i = return $ fromInteger i

instance (Fractional a) => Fractional (Parameter a) where
  x / y = liftM2P (/) x y
  recip = liftMP recip
  fromRational t = return $ fromRational t

instance (Floating a) => Floating (Parameter a) where
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

instance MonadIO Parameter where
  liftIO m = Parameter $ const m

-- | A type class to lift the parameters to other computations.
class ParameterLift m where
  
  -- | Lift the specified 'Parameter' computation to another computation.
  liftParameter :: Parameter a -> m a

instance ParameterLift Parameter where
  liftParameter = id
    
-- | Exception handling within 'Parameter' computations.
catchParameter :: Parameter a -> (IOException -> Parameter a) -> Parameter a
catchParameter (Parameter m) h =
  Parameter $ \r -> 
  C.catch (m r) $ \e ->
  let Parameter m' = h e in m' r
                           
-- | A computation with finalization part like the 'finally' function.
finallyParameter :: Parameter a -> Parameter b -> Parameter a
finallyParameter (Parameter m) (Parameter m') =
  Parameter $ \r ->
  C.finally (m r) (m' r)

-- | Like the standard 'throw' function.
throwParameter :: IOException -> Parameter a
throwParameter = throw

-- | Invoke the 'Parameter' computation.
invokeParameter :: Run -> Parameter a -> IO a
{-# INLINE invokeParameter #-}
invokeParameter r (Parameter m) = m r

instance MonadFix Parameter where
  mfix f = 
    Parameter $ \r ->
    do { rec { a <- invokeParameter r (f a) }; return a }  

-- | Memoize the 'Parameter' computation, always returning the same value
-- within a simulation run. However, the value will be recalculated for other
-- simulation runs. Also it is thread-safe when different simulation runs
-- are executed in parallel on physically different operating system threads.
memoParameter :: Parameter a -> IO (Parameter a)
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

-- | Create a thread-safe parameter that returns always the same value within the simulation run,
-- where the value is taken consequently from the specified table based on the number of the 
-- current run starting from zero. After all values from the table are used, it takes the first 
-- value of the table, then the second one and so on.
--
-- It uses the 'memoParameter' function.
newTableParameter :: Array Int a -> IO (Parameter a)
newTableParameter t = newIndexedParameter (\i -> return $ t ! (((i - i1) `mod` n) + i1))
  where (i1, i2) = bounds t
        n = i2 - i1 + 1

-- | Create a thread-safe parameter that returns always the same value within the simulation run, 
-- where the value depends on the number of this run starting from zero.
--
-- It uses the 'memoParameter' function.
newIndexedParameter :: (Int -> Parameter a) -> IO (Parameter a)
newIndexedParameter f =
  memoParameter $ parameterIndex >>= f
