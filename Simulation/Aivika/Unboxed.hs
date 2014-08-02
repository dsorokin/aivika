
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Unboxed
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The 'Unboxed' class allows creating unboxed arrays in monad 'IO'.
--

module Simulation.Aivika.Unboxed
       (Unboxed(..)) where

import Data.Array
import Data.Array.IO.Safe
import Data.Int	 
import Data.Word	 

-- | The type which values can be contained in an unboxed array.
class MArray IOUArray e IO => Unboxed e where

  -- | Create an unboxed array with default values.
  newUnboxedArray_ :: Ix i => (i, i) -> IO (IOUArray i e)
  newUnboxedArray_ = newArray_

instance Unboxed Bool	 
instance Unboxed Char	 
instance Unboxed Double	 
instance Unboxed Float	 
instance Unboxed Int	 
instance Unboxed Int8	 
instance Unboxed Int16	 
instance Unboxed Int32	 
instance Unboxed Int64	 
instance Unboxed Word	 
instance Unboxed Word8	 
instance Unboxed Word16	 
instance Unboxed Word32	 
instance Unboxed Word64
