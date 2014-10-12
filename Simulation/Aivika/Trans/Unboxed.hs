
{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Trans.Unboxed
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The 'Unboxed' class allows creating unboxed arrays in monad 'IO'.
--

module Simulation.Aivika.Trans.Unboxed
       (Unboxed(..)) where

import Simulation.Aivika.Trans.ProtoArray.Unboxed

import Data.Array
import Data.Int
import Data.Word

-- | The type which values can be contained in an unboxed array.
class ProtoArraying m e => Unboxed m e

instance Unboxed IO Bool
instance Unboxed IO Char
instance Unboxed IO Double
instance Unboxed IO Float
instance Unboxed IO Int
instance Unboxed IO Int8
instance Unboxed IO Int16
instance Unboxed IO Int32
instance Unboxed IO Word
instance Unboxed IO Word8
instance Unboxed IO Word16
instance Unboxed IO Word32

#ifndef __HASTE__

instance Unboxed IO Int64
instance Unboxed IO Word64

#endif
