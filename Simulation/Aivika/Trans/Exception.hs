
-- |
-- Module     : Simulation.Aivika.Trans.Exception
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a type class of monads with 'IO' exception handling capabilities.
--
module Simulation.Aivika.Trans.Exception
       (ExceptionThrowing(..),
        ExceptionHandling(..)) where

import Control.Monad.Trans
import Control.Exception

-- | A computation within which we can throw an exception.
class ExceptionThrowing m where

  -- | Throw an exception.
  throwComp :: Exception e => e -> m a

-- | A computation within which we can handle 'IO' exceptions
-- as well as define finalisation blocks.
class (ExceptionThrowing m, MonadIO m) => ExceptionHandling m where

  -- | Catch an 'IO' exception within the computation.
  catchComp :: (Exception e, MonadIO m) => m a -> (e -> m a) -> m a

  -- | Introduce a finalisation block.
  finallyComp :: MonadIO m => m a -> m b -> m a

instance ExceptionThrowing IO where

  {-# INLINE throwComp #-}
  throwComp = throw

instance ExceptionHandling IO where

  {-# INLINE catchComp #-}
  catchComp = catch

  {-# INLINE finallyComp #-}
  finallyComp = finally
