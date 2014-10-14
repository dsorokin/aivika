
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Trans.Template
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'TemplateComp' monad template based on which we can
-- construct automatically simulation computations with extended functionality.
--
module Simulation.Aivika.Trans.Template
       (TemplateComp(..)) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Session

-- | The 'TemplateComp' monad template based on which we can construct
-- automatically simulation computations with extended functionality.
--
-- We need a structurally different type to be able for extending
-- the source simulation computation with new functions.
newtype TemplateComp m a =
  TemplateComp { runComp :: m a
                 -- ^ Return the source computation.
               }

instance Monad m => Monad (TemplateComp m) where

  {-# INLINE return #-}
  return = TemplateComp . return

  {-# INLINE (>>=) #-}
  (TemplateComp m) >>= k = TemplateComp (m >>= k')
    where k' a = let TemplateComp m' = k a in m'

instance Functor m => Functor (TemplateComp m) where
  
  {-# INLINE fmap #-}
  fmap f (TemplateComp a) = TemplateComp (fmap f a)

instance Applicative m => Applicative (TemplateComp m) where
  
  {-# INLINE pure #-}
  pure = TemplateComp . pure
  
  {-# INLINE (<*>) #-}
  (TemplateComp x) <*> (TemplateComp y) = TemplateComp (x <*> y)

liftMT :: Monad m => (a -> b) -> TemplateComp m a -> TemplateComp m b
{-# INLINE liftMT #-}
liftMT f (TemplateComp m) =
  TemplateComp $ do { a <- m; return $ f a }

liftM2T :: Monad m => (a -> b -> c) -> TemplateComp m a -> TemplateComp m b -> TemplateComp m c
{-# INLINE liftM2T #-}
liftM2T f (TemplateComp x) (TemplateComp y) =
  TemplateComp $ do { a <- x; b <- y; return $ f a b }

instance (Num a, Monad m) => Num (TemplateComp m a) where
  x + y = liftM2T (+) x y
  x - y = liftM2T (-) x y
  x * y = liftM2T (*) x y
  negate = liftMT negate
  abs = liftMT abs
  signum = liftMT signum
  fromInteger i = return $ fromInteger i

instance (Fractional a, Monad m) => Fractional (TemplateComp m a) where
  x / y = liftM2T (/) x y
  recip = liftMT recip
  fromRational t = return $ fromRational t

instance (Floating a, Monad m) => Floating (TemplateComp m a) where
  pi = return pi
  exp = liftMT exp
  log = liftMT log
  sqrt = liftMT sqrt
  x ** y = liftM2T (**) x y
  sin = liftMT sin
  cos = liftMT cos
  tan = liftMT tan
  asin = liftMT asin
  acos = liftMT acos
  atan = liftMT atan
  sinh = liftMT sinh
  cosh = liftMT cosh
  tanh = liftMT tanh
  asinh = liftMT asinh
  acosh = liftMT acosh
  atanh = liftMT atanh

instance MonadTrans TemplateComp where

  {-# INLINE lift #-}
  lift = TemplateComp

instance MonadIO m => MonadIO (TemplateComp m) where
  
  {-# INLINE liftIO #-}
  liftIO = TemplateComp . liftIO

instance ExceptionHandling m => ExceptionHandling (TemplateComp m) where

  {-# INLINABLE catchComp #-}
  catchComp (TemplateComp m) h =
    TemplateComp $ catchComp m (\e -> let TemplateComp m' = h e in m')
    
  {-# INLINABLE finallyComp #-}
  finallyComp (TemplateComp x) (TemplateComp y) =
    TemplateComp $ finallyComp x y

  {-# INLINABLE throwComp #-}
  throwComp = TemplateComp . throwComp

instance MonadFix m => MonadFix (TemplateComp m) where

  {-# INLINE mfix #-}
  mfix f = TemplateComp $ mfix f'
    where f' a = let ~(TemplateComp m) = f a in m 
