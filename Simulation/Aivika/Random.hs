
-- |
-- Module     : Simulation.Aivika.Random
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- Below are defined some random functions.
--
module Simulation.Aivika.Random 
       (uniformGen,
        newNormalGen,
        exponentialGen,
        poissonGen,
        binomialGen) where

import System.Random
import Data.IORef

-- | Generate the uniform random number with the specified minimum and maximum.
uniformGen :: Double
              -- ^ minimum
              -> Double
              -- ^ maximum
              -> IO Double
uniformGen min max =
  do x <- getStdRandom random
     return $ min + x * (max - min)

-- | Create a normal random number generator with mean 0 and variance 1.
newNormalGen :: IO (IO Double)
newNormalGen =
  do nextRef <- newIORef 0.0
     flagRef <- newIORef False
     xi1Ref  <- newIORef 0.0
     xi2Ref  <- newIORef 0.0
     psiRef  <- newIORef 0.0
     let loop =
           do psi <- readIORef psiRef
              if (psi >= 1.0) || (psi == 0.0)
                then do g1 <- getStdRandom random
                        g2 <- getStdRandom random
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
exponentialGen :: Double -> IO Double
exponentialGen mu =
  do x <- getStdRandom random
     return (- log x * mu)

-- | Generate the Poisson random number with the specified mean.
poissonGen :: Double -> IO Int
poissonGen mu =
  do prob0 <- getStdRandom random
     let loop prob prod acc
           | prob <= prod = return acc
           | otherwise    = loop
                            (prob - prod)
                            (prod * mu / fromIntegral (acc + 1))
                            (acc + 1)
     loop prob0 (exp (- mu)) 0

-- | Generate a binomial random number with the specified probability and number of trials. 
binomialGen :: Double 
               -- ^ the probability
               -> Int
               -- ^ the number of trials
               -> IO Int
binomialGen prob trials = loop trials 0 where
  loop n acc
    | n == 0    = return acc
    | otherwise = do x <- getStdRandom random
                     if x <= prob
                       then loop (n - 1) (acc + 1)
                       else loop (n - 1) acc
