
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Transform
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines something which is most close to the notion of
-- analogous circuit as an opposite to the digital one.
--
module Simulation.Aivika.Transform
       (-- * Transform Arrow
        Transform(..),
        -- * Differential and Difference Equations
        integTransform,
        sumTransform,
        timeTransform) where

import qualified Control.Category as C
import Control.Arrow
import Control.Monad

import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Memo
import Simulation.Aivika.Unboxed
import Simulation.Aivika.SystemDynamics

-- | It allows representing an analogous circuit as an opposite to
-- the digital one.
--
-- This is a transform of one time varying function to another usually
-- specified in the integration time points and then interpolated in
-- other time points with help of one of the memoization functions
-- like 'memo0Dynamics'.
--
newtype Transform a b =
  Transform { runTransform :: Dynamics a -> Simulation (Dynamics b)
              -- ^ Run the transform.
            }

instance C.Category Transform where

  id = Transform return
  
  (Transform g) . (Transform f) =
    Transform $ \a -> f a >>= g

instance Arrow Transform where

  arr f = Transform $ return . fmap f

  first (Transform f) =
    Transform $ \bd ->
    do (b, d) <- memo0UnzipDynamics bd
       c <- f b
       return $ liftM2 (,) c d 

  second (Transform f) =
    Transform $ \db ->
    do (d, b) <- memo0UnzipDynamics db
       c <- f b
       return $ liftM2 (,) d c

  (Transform f) *** (Transform g) =
    Transform $ \bb' ->
    do (b, b') <- memo0UnzipDynamics bb'
       c  <- f b
       c' <- g b'
       return $ liftM2 (,) c c'

  (Transform f) &&& (Transform g) =
    Transform $ \b ->
    do c  <- f b
       c' <- g b
       return $ liftM2 (,) c c'

instance ArrowLoop Transform where

  loop (Transform f) =
    Transform $ \b ->
    mdo let bd = liftM2 (,) b d
        cd <- f bd
        (c, d) <- memo0UnzipDynamics cd
        return c

-- | A transform that returns the current modeling time.
timeTransform :: Transform a Double
timeTransform = Transform $ const $ return time

-- | Return a transform that maps the derivative to an integral
-- by the specified initial value.
integTransform :: Dynamics Double
                  -- ^ the initial value
                  -> Transform Double Double
                  -- ^ map the derivative to an integral
integTransform = Transform . integ

-- | Return a transform that maps the difference to a sum
-- by the specified initial value.
sumTransform :: (Num a, Unboxed a) =>
                Dynamics a
                -- ^ the initial value
                -> Transform a a
                -- ^ map the difference to a sum
sumTransform = Transform . diffsum
