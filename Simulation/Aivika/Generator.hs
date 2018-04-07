
{-# LANGUAGE RankNTypes #-}

-- |
-- Module     : Simulation.Aivika.Generator
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- Below is defined a type class of the random number generator.
--
module Simulation.Aivika.Generator 
       (Generator(..),
        GeneratorType(..),
        DiscretePDF(..),
        newGenerator,
        newRandomGenerator,
        newRandomGenerator01) where

import System.Random
import qualified System.Random.MWC as MWC
import Data.IORef
import Data.Word
import Data.Vector
import Data.Functor

-- | A discrete probability density function.
type DiscretePDF a = [(a, Double)]

-- | Defines a random number generator.
data Generator =
  Generator { generateUniform :: Double -> Double -> IO Double,
              -- ^ Generate an uniform random number
              -- with the specified minimum and maximum.
              generateUniformInt :: Int -> Int -> IO Int,
              -- ^ Generate an uniform integer random number
              -- with the specified minimum and maximum.
              generateTriangular :: Double -> Double -> Double -> IO Double,
              -- ^ Generate a triangular random number
              -- by the specified minimum, median and maximum.
              generateNormal :: Double -> Double -> IO Double,
              -- ^ Generate the normal random number
              -- with the specified mean and deviation.
              generateLogNormal :: Double -> Double -> IO Double,
              -- ^ Generate a random number from the lognormal distribution derived
              -- from a normal distribution with the specified mean and deviation.
              generateExponential :: Double -> IO Double,
              -- ^ Generate the random number distributed exponentially
              -- with the specified mean (the reciprocal of the rate).
              generateErlang :: Double -> Int -> IO Double,
              -- ^ Generate the Erlang random number
              -- with the specified scale (the reciprocal of the rate) and integer shape.
              generatePoisson :: Double -> IO Int,
              -- ^ Generate the Poisson random number
              -- with the specified mean.
              generateBinomial :: Double -> Int -> IO Int,
              -- ^ Generate the binomial random number
              -- with the specified probability and number of trials.
              generateGamma :: Double -> Double -> IO Double,
              -- ^ Generate a random number from the Gamma distribution with
              -- the specified shape (kappa) and scale (theta, a reciprocal of the rate).
              --
              -- The probability density for the Gamma distribution is
              --
              -- @f x = x ** (kappa - 1) * exp (- x \/ theta) \/ theta ** kappa * Gamma kappa@
              generateBeta :: Double -> Double -> IO Double,
              -- ^ Generate a random number from the Beta distribution by
              -- the specified shape parameters (alpha and beta).
              --
              -- The probability density for the Beta distribution is
              --
              -- @f x = x ** (alpha - 1) * (1 - x) ** (beta - 1) \/ B alpha beta@
              generateWeibull :: Double -> Double -> IO Double,
              -- ^ Generate a random number from the Weibull distribution by
              -- the specified shape and scale.
              generateDiscrete :: forall a. DiscretePDF a -> IO a,
              -- ^ Generate a random value from the specified discrete distribution.
              generateSequenceNo :: IO Int
              -- ^ Generate a sequence number which can be considered quite unique.
            }

-- | Generate the uniform random number with the specified minimum and maximum.
generateUniform01 :: IO Double
                     -- ^ the generator
                     -> Double
                     -- ^ minimum
                     -> Double
                     -- ^ maximum
                     -> IO Double
generateUniform01 g min max =
  do x <- g
     return $ min + x * (max - min)

-- | Generate the uniform random number with the specified minimum and maximum.
generateUniformInt01 :: IO Double
                        -- ^ the generator
                        -> Int
                        -- ^ minimum
                        -> Int
                        -- ^ maximum
                        -> IO Int
generateUniformInt01 g min max =
  do x <- g
     let min' = fromIntegral min - 0.5
         max' = fromIntegral max + 0.5
         z    = round (min' + x * (max' - min'))
         z'   = if z < min
                then min
                else if z > max
                     then max
                     else z
     return z'

-- | Generate the triangular random number by the specified minimum, median and maximum.
generateTriangular01 :: IO Double
                        -- ^ the generator
                        -> Double
                        -- ^ minimum
                        -> Double
                        -- ^ median
                        -> Double
                        -- ^ maximum
                        -> IO Double
generateTriangular01 g min median max =
  do x <- g
     if x <= (median - min) / (max - min)
       then return $ min + sqrt ((median - min) * (max - min) * x)
       else return $ max - sqrt ((max - median) * (max - min) * (1 - x))

-- | Create a normal random number generator with mean 0 and variance 1
-- by the specified generator of uniform random numbers from 0 to 1.
newNormalGenerator01 :: IO Double
                        -- ^ the generator
                        -> IO (IO Double)
newNormalGenerator01 g =
  do nextRef <- newIORef 0.0
     flagRef <- newIORef False
     xi1Ref  <- newIORef 0.0
     xi2Ref  <- newIORef 0.0
     psiRef  <- newIORef 0.0
     let loop =
           do psi <- readIORef psiRef
              if (psi >= 1.0) || (psi == 0.0)
                then do g1 <- g
                        g2 <- g
                        let xi1 = 2.0 * g1 - 1.0
                            xi2 = 2.0 * g2 - 1.0
                            psi = xi1 * xi1 + xi2 * xi2
                        writeIORef xi1Ref xi1
                        writeIORef xi2Ref xi2
                        writeIORef psiRef psi
                        loop
                else writeIORef psiRef $ sqrt (- 2.0 * log psi / psi)
     return $
       do flag <- readIORef flagRef
          if flag
            then do writeIORef flagRef False
                    readIORef nextRef
            else do writeIORef xi1Ref 0.0
                    writeIORef xi2Ref 0.0
                    writeIORef psiRef 0.0
                    loop
                    xi1 <- readIORef xi1Ref
                    xi2 <- readIORef xi2Ref
                    psi <- readIORef psiRef
                    writeIORef flagRef True
                    writeIORef nextRef $ xi2 * psi
                    return $ xi1 * psi

-- | Return the exponential random number with the specified mean.
generateExponential01 :: IO Double
                         -- ^ the generator
                         -> Double
                         -- ^ the mean
                         -> IO Double
generateExponential01 g mu =
  do x <- g
     return (- log x * mu)

-- | Return the Erlang random number.
generateErlang01 :: IO Double
                    -- ^ the generator
                    -> Double
                    -- ^ the scale
                    -> Int
                    -- ^ the shape
                    -> IO Double
generateErlang01 g beta m =
  do x <- loop m 1
     return (- log x * beta)
       where loop m acc
               | m < 0     = error "Negative shape: generateErlang."
               | m == 0    = return acc
               | otherwise = do x <- g
                                loop (m - 1) (x * acc)

-- | Generate the Poisson random number with the specified mean.
generatePoisson01 :: IO Double
                     -- ^ the generator
                     -> Double
                     -- ^ the mean
                     -> IO Int
generatePoisson01 g mu =
  do prob0 <- g
     let loop prob prod acc
           | prob <= prod = return acc
           | otherwise    = loop
                            (prob - prod)
                            (prod * mu / fromIntegral (acc + 1))
                            (acc + 1)
     loop prob0 (exp (- mu)) 0

-- | Generate a binomial random number with the specified probability and number of trials. 
generateBinomial01 :: IO Double
                      -- ^ the generator
                      -> Double 
                      -- ^ the probability
                      -> Int
                      -- ^ the number of trials
                      -> IO Int
generateBinomial01 g prob trials = loop trials 0 where
  loop n acc
    | n < 0     = error "Negative number of trials: generateBinomial."
    | n == 0    = return acc
    | otherwise = do x <- g
                     if x <= prob
                       then loop (n - 1) (acc + 1)
                       else loop (n - 1) acc

-- | Generate a random number from the Gamma distribution using Marsaglia and Tsang method.
generateGamma01 :: IO Double
                   -- ^ a normal random number ~ N (0,1)
                   -> IO Double
                   -- ^ an uniform random number ~ U (0, 1)
                   -> Double
                   -- ^ the shape parameter (kappa) 
                   -> Double
                   -- ^ the scale parameter (theta)
                   -> IO Double
generateGamma01 gn gu kappa theta
  | kappa <= 0 = error "The shape parameter (kappa) must be positive: generateGamma01"
  | kappa > 1  =
    let d = kappa - 1 / 3
        c = 1 / sqrt (9 * d)
        loop =
          do z <- gn
             if z <= - (1 / c)
               then loop
               else do let v = (1 + c * z) ** 3
                       u <- gu
                       if log u > 0.5 * z * z + d - d * v + d * log v
                         then loop
                         else return $ d * v * theta
    in loop
  | otherwise  =
    do x <- generateGamma01 gn gu (1 + kappa) theta
       u <- gu
       return $ x * u ** (1 / kappa)

-- | Generate a random number from the Beta distribution.
generateBeta01 :: IO Double
                  -- ^ a normal random number ~ N (0, 1)
                  -> IO Double
                  -- ^ an uniform random number ~ U (0, 1)
                  -> Double
                  -- ^ the shape parameter alpha
                  -> Double
                  -- ^ the shape parameter beta
                  -> IO Double
generateBeta01 gn gu alpha beta =
  do g1 <- generateGamma01 gn gu alpha 1
     g2 <- generateGamma01 gn gu beta 1
     return $ g1 / (g1 + g2)

-- | Generate a random number from the Weibull distribution.
generateWeibull01 :: IO Double
                     -- ^ an uniform random number ~ U (0, 1)
                     -> Double
                     -- ^ shape
                     -> Double
                     -- ^ scale
                     -> IO Double
generateWeibull01 g alpha beta =
  do x <- g
     return $ beta * (- log x) ** (1 / alpha)

-- | Generate a random value from the specified discrete distribution.
generateDiscrete01 :: IO Double
                      -- ^ an uniform random number ~ U (0, 1)
                      -> DiscretePDF a
                      -- ^ a discrete probability density function
                      -> IO a
generateDiscrete01 g []   = error "Empty PDF: generateDiscrete01"
generateDiscrete01 g dpdf =
  do x <- g
     let loop acc [(a, p)] = a
         loop acc ((a, p) : dpdf) =
           if x <= acc + p
           then a
           else loop (acc + p) dpdf
     return $ loop 0 dpdf

-- | Defines a type of the random number generator.
data GeneratorType = SimpleGenerator
                     -- ^ The simple random number generator.
                   | SimpleGeneratorWithSeed Word32
                     -- ^ The simple random number generator with the specified seed.
                   | CustomGenerator (IO Generator)
                     -- ^ The custom random number generator.
                   | CustomGenerator01 (IO Double)
                     -- ^ The custom random number generator by the specified uniform
                     -- generator of numbers from 0 to 1.

-- | Create a new random number generator by the specified type.
newGenerator :: GeneratorType -> IO Generator
newGenerator tp =
  case tp of
    SimpleGenerator ->
      MWC.uniform <$> MWC.withSystemRandom (return :: MWC.GenIO -> IO MWC.GenIO) >>= newRandomGenerator01
    SimpleGeneratorWithSeed x ->
      MWC.uniform <$> MWC.initialize (singleton x) >>= newRandomGenerator01
    CustomGenerator g ->
      g
    CustomGenerator01 g ->
      newRandomGenerator01 g

-- | Create a new random generator by the specified standard generator.
newRandomGenerator :: RandomGen g => g -> IO Generator
newRandomGenerator g =
  do r <- newIORef g
     let g1 = do g <- readIORef r
                 let (x, g') = random g
                 writeIORef r g'
                 return x
     newRandomGenerator01 g1

-- | Create a new random generator by the specified uniform generator of numbers from 0 to 1.
newRandomGenerator01 :: IO Double -> IO Generator
newRandomGenerator01 g =
  do let g1 = g
     g2 <- newNormalGenerator01 g1
     let g3 mu nu = do { x <- g2; return $ mu + nu * x }
         g4 mu nu = do { x <- g2; return $ exp (mu + nu * x) }
     seqNoRef <- newIORef 0
     let seqNo = do { x <- readIORef seqNoRef; modifyIORef' seqNoRef (+1); return x }
     return Generator { generateUniform = generateUniform01 g1,
                        generateUniformInt = generateUniformInt01 g1,
                        generateTriangular = generateTriangular01 g1,
                        generateNormal = g3,
                        generateLogNormal = g4,
                        generateExponential = generateExponential01 g1,
                        generateErlang = generateErlang01 g1,
                        generatePoisson = generatePoisson01 g1,
                        generateBinomial = generateBinomial01 g1,
                        generateGamma = generateGamma01 g2 g1,
                        generateBeta = generateBeta01 g2 g1,
                        generateWeibull = generateWeibull01 g1,
                        generateDiscrete = generateDiscrete01 g1,
                        generateSequenceNo = seqNo }
