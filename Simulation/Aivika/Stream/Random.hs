
-- |
-- Module     : Simulation.Aivika.Stream.Random
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines random streams of data, which are useful
-- for describing the input of the model.
--

module Simulation.Aivika.Stream.Random
       (uniformStream,
        normalStream,
        exponentialStream,
        poissonStream,
        binomialStream,
        delayStream) where

import System.Random

import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Process
import Simulation.Aivika.Random
import Simulation.Aivika.Stream

-- | Create a new stream with delays distributed uniformly.
uniformStream :: Dynamics Double     -- ^ the minimum delay
                 -> Dynamics Double  -- ^ the maximum delay
                 -> Stream Double    -- ^ the stream of delays
uniformStream min max = Cons z where
  z = do x <- liftIO $ getStdRandom random
         min' <- liftDynamics min
         max' <- liftDynamics max
         let delay = min' + x * (max' - min')
         holdProcess delay
         return (delay, uniformStream min max)

-- | Create a new stream with delays distributed normally.
normalStream :: Dynamics Double     -- ^ the mean delay
                -> Dynamics Double  -- ^ the delay variance
                -> Stream Double    -- ^ the stream of delays
normalStream mu nu = Cons z where
  z = do g <- liftIO $ newNormalGen
         let stream =
               do x <- liftIO g
                  mu' <- liftDynamics mu
                  nu' <- liftDynamics nu
                  let delay = mu' + x * nu'
                  holdProcess delay
                  return (delay, Cons stream)
         stream

-- | Return a new stream with delays disibuted exponentially with the specified mean.
exponentialStream :: Dynamics Double   -- ^ the mean delay
                     -> Stream Double  -- ^ the stream of delays
exponentialStream mu = Cons z where
  z = do mu' <- liftDynamics mu
         delay <- liftIO $ exponentialGen mu'
         holdProcess delay
         return (delay, exponentialStream mu)

-- | Return a new stream with delays having the Poisson distribution with the specified mean.
poissonStream :: Dynamics Double  -- ^ the mean delay
                 -> Stream Int    -- ^ the stream of delays
poissonStream mu = Cons z where
  z = do mu' <- liftDynamics mu
         delay <- liftIO $ poissonGen mu'
         holdProcess $ fromIntegral delay
         return (delay, poissonStream mu)

-- | Return a new stream with delays having the binomial distribution with the specified
-- probability and trials.
binomialStream :: Dynamics Double  -- ^ the probability
                  -> Dynamics Int  -- ^ the number of trials
                  -> Stream Int    -- ^ the stream of delays
binomialStream prob trials = Cons z where
  z = do prob' <- liftDynamics prob
         trials' <- liftDynamics trials
         delay <- liftIO $ binomialGen prob' trials'
         holdProcess $ fromIntegral delay
         return (delay, binomialStream prob trials)

-- | Create a new stream with the specified delays. This function is useful
-- if you want to create a random stream, which is not defined in this module.
-- So, you can lift any 'IO' computation to the 'Dynamics' computation, for example,
-- such a generation of the random number, which will be repeated again and again.
delayStream :: Dynamics (a, Double)  -- ^ the value and its delay
               -> Stream a           -- ^ the stream of values delayed in time
delayStream x = Cons z where
  z = do (a, delay) <- liftDynamics x
         holdProcess delay
         return (a, delayStream x)
