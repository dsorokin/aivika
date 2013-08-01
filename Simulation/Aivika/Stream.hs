
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

import Data.IORef
import Data.Maybe

import Simulation.Aivika.Simulation
import Simulation.Aivika.Process

-- | Represents an infinite stream of data in time,
-- some kind of the cons cell.
data Stream a = Cons (Process (a, Stream a))

instance Functor Stream where
  
  fmap f (Cons s) = Cons y where
    y = do (x, xs) <- s
           return (f x, fmap f xs)

-- | Zip two streams trying to get data as soon as possible,
-- launching the sub-processes in parallel.
zipStream :: Stream a -> Stream b -> Stream (a, b)
zipStream (Cons sa) (Cons sb) = Cons y where
  y = do ((x, xs), (y, ys)) <- zipProcess sa sb
         return ((x, y), zipStream xs ys)

-- | Unzip the stream.
unzipStream :: Stream (a, b) -> Process (Stream a, Stream b)
unzipStream (Cons s) =
  do ((x, y), xys) <- s
     xys' <- liftSimulation $ memoProcess (unzipStream xys)
     let xs = xys' >>= \(Cons xs, _) -> xs
         ys = xys' >>= \(_, Cons ys) -> ys
     return (Cons $ return (x, Cons xs),
             Cons $ return (y, Cons ys))
