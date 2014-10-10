
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

import Simulation.Aivika.Trans.Internal.Unboxed
