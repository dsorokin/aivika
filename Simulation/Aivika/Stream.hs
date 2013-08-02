
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
        memoStream,
        zipStreamSeq,
        zipStreamParallel,
        unzipStream,
        mapStream,
        mapStreamM,
        apStreamDataFirst,
        apStreamDataLater,
        apStreamParallel,
        filterStream,
        filterStreamM) where

import Data.IORef
import Data.Maybe

import Simulation.Aivika.Simulation
import Simulation.Aivika.Process

-- | Represents an infinite stream of data in time,
-- some kind of the cons cell.
data Stream a = Cons (Process (a, Stream a))

instance Functor Stream where
  
  fmap f (Cons s) = Cons y where
    y = do ~(x, xs) <- s
           return (f x, fmap f xs)

-- | Memoize the stream.
memoStream :: Stream a -> Simulation (Stream a)
memoStream (Cons s) =
  do p <- memoProcess $
          do ~(x, xs) <- s
             xs' <- liftSimulation $ memoStream xs
             return (x, xs')
     return (Cons p)

-- | Zip two streams trying to get data sequentially.
zipStreamSeq :: Stream a -> Stream b -> Stream (a, b)
zipStreamSeq (Cons sa) (Cons sb) = Cons y where
  y = do ~(x, xs) <- sa
         ~(y, ys) <- sb
         return ((x, y), zipStreamSeq xs ys)

-- | Zip two streams trying to get data as soon as possible,
-- launching the sub-processes in parallel.
zipStreamParallel :: Stream a -> Stream b -> Stream (a, b)
zipStreamParallel (Cons sa) (Cons sb) = Cons y where
  y = do ~((x, xs), (y, ys)) <- zipProcessParallel sa sb
         return ((x, y), zipStreamParallel xs ys)

-- | Unzip the stream.
unzipStream :: Stream (a, b) -> Simulation (Stream a, Stream b)
unzipStream s =
  do s' <- memoStream s
     let sa = mapStream fst s'
         sb = mapStream snd s'
     return (sa, sb)

-- | Map the stream according the specified function.
mapStream :: (a -> b) -> Stream a -> Stream b
mapStream = fmap

-- | Compose the stream.
mapStreamM :: (a -> Process b) -> Stream a -> Stream b
mapStreamM f (Cons x) = Cons y where
  y = do (a, xs) <- x
         b <- f a
         return (b, mapStreamM f xs)

-- | Transform the stream getting the transformation function after data have come.
apStreamDataFirst :: Process (a -> b) -> Stream a -> Stream b
apStreamDataFirst f (Cons x) = Cons y where
  y = do ~(a, xs) <- x
         g <- f
         return (g a, apStreamDataFirst f xs)

-- | Transform the stream getting the transformation function before requesting for data.
apStreamDataLater :: Process (a -> b) -> Stream a -> Stream b
apStreamDataLater f (Cons x) = Cons y where
  y = do g <- f
         ~(a, xs) <- x
         return (g a, apStreamDataLater f xs)

-- | Transform the stream trying to get the transformation function as soon as possible
-- at the same time when requesting for the next portion of data.
apStreamParallel :: Process (a -> b) -> Stream a -> Stream b
apStreamParallel f (Cons x) = Cons y where
  y = do ~(g, (a, xs)) <- zipProcessParallel f x
         return (g a, apStreamParallel f xs)

-- | Filter only those data values that satisfy to the specified predicate.
filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream p (Cons x) = Cons y where
  y = do (a, xs) <- x
         if p a
           then return (a, filterStream p xs)
           else let Cons z = filterStream p xs in z

-- | Filter only those data values that satisfy to the specified predicate.
filterStreamM :: (a -> Process Bool) -> Stream a -> Stream a
filterStreamM p (Cons x) = Cons y where
  y = do (a, xs) <- x
         b <- p a
         if b
           then return (a, filterStreamM p xs)
           else let Cons z = filterStreamM p xs in z
