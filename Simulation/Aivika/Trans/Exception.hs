
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

import Simulation.Aivika.Trans.Internal.Exception
