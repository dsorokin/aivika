
-- |
-- Module     : Simulation.Aivika.Stream
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The infinite stream of data in time.
--
module Simulation.Aivika.Stream
       (Stream(..),
        zipStream,
        unzipStream) where

import Simulation.Aivika.Internal.Process

-- | Represents an infinite stream of data in time.
data Stream a = Cons (Process (a, Stream a))

-- | Zip two streams.
zipStream :: Stream a -> Stream b -> Stream (a, b)
zipStream = undefined

-- | Unzip the stream.
unzipStream :: Stream (a, b) -> Process (Stream a, Stream b)
unzipStream = undefined