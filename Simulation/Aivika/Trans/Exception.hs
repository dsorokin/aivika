
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
       (ExceptionHandling(..)) where

import Control.Exception

-- | A computation within which we can handle 'IO' exceptions
-- as well as define finalisation blocks.
class ExceptionHandling m where

  -- | Catch an 'IO' exception within the computation.
  catchComp :: m a -> (IOException -> m a) -> m a

  -- | Introduce a finalisation block.
  finallyComp :: m a -> m b -> m a

  -- | Throw an exception.
  throwComp :: IOException -> m a

instance ExceptionHandling IO where

  {-# INLINE catchComp #-}
  catchComp = catch

  {-# INLINE finallyComp #-}
  finallyComp = finally

  {-# INLINE throwComp #-}
  throwComp = throw
