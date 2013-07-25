
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Unboxed
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The 'Unboxed' class allows creating unboxed arrays in monad 'IO'.
--

module Simulation.Aivika.Unboxed
       (Unboxed(..)) where

import Data.Array
import Data.Array.IO.Safe

-- | The type which values can be contained in an unboxed array.
class Unboxed e where

  -- | Create an unboxed array with default values.
  newUnboxedArray_ :: Ix i => (i, i) -> IO (IOUArray i e)

instance MArray IOUArray e IO => Unboxed e where

  newUnboxedArray_ = newArray_
