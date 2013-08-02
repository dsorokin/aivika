
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
        filterStreamM,
        leftStream,
        rightStream,
        replaceLeftStream,
        replaceRightStream) where

import Data.IORef
import Data.Maybe

import Simulation.Aivika.Simulation
import Simulation.Aivika.Process

-- | Represents an infinite stream of data in time,
-- some kind of the cons cell.
newtype Stream a = Cons { runStream :: Process (a, Stream a)
                          -- ^ Run the stream.
                        }

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
mapStreamM f (Cons s) = Cons y where
  y = do (a, xs) <- s
         b <- f a
         return (b, mapStreamM f xs)

-- | Transform the stream getting the transformation function after data have come.
apStreamDataFirst :: Process (a -> b) -> Stream a -> Stream b
apStreamDataFirst f (Cons s) = Cons y where
  y = do ~(a, xs) <- s
         g <- f
         return (g a, apStreamDataFirst f xs)

-- | Transform the stream getting the transformation function before requesting for data.
apStreamDataLater :: Process (a -> b) -> Stream a -> Stream b
apStreamDataLater f (Cons s) = Cons y where
  y = do g <- f
         ~(a, xs) <- s
         return (g a, apStreamDataLater f xs)

-- | Transform the stream trying to get the transformation function as soon as possible
-- at the same time when requesting for the next portion of data.
apStreamParallel :: Process (a -> b) -> Stream a -> Stream b
apStreamParallel f (Cons s) = Cons y where
  y = do ~(g, (a, xs)) <- zipProcessParallel f s
         return (g a, apStreamParallel f xs)

-- | Filter only those data values that satisfy to the specified predicate.
filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream p (Cons s) = Cons y where
  y = do (a, xs) <- s
         if p a
           then return (a, filterStream p xs)
           else let Cons z = filterStream p xs in z

-- | Filter only those data values that satisfy to the specified predicate.
filterStreamM :: (a -> Process Bool) -> Stream a -> Stream a
filterStreamM p (Cons s) = Cons y where
  y = do (a, xs) <- s
         b <- p a
         if b
           then return (a, filterStreamM p xs)
           else let Cons z = filterStreamM p xs in z

-- | The stream of 'Left' values.
leftStream :: Stream (Either a b) -> Stream a
leftStream (Cons s) = Cons y where
  y = do (a, xs) <- s
         case a of
           Left a  -> return (a, leftStream xs)
           Right _ -> let Cons z = leftStream xs in z

-- | The stream of 'Right' values.
rightStream :: Stream (Either a b) -> Stream b
rightStream (Cons s) = Cons y where
  y = do (a, xs) <- s
         case a of
           Left _  -> let Cons z = rightStream xs in z
           Right a -> return (a, rightStream xs)

-- | Replace the 'Left' values.
replaceLeftStream :: Stream (Either a b) -> Stream c -> Stream (Either c b)
replaceLeftStream (Cons sab) (ys0 @ (Cons sc)) = Cons z where
  z = do (a, xs) <- sab
         case a of
           Left _ ->
             do (b, ys) <- sc
                return (Left b, replaceLeftStream xs ys)
           Right a ->
             return (Right a, replaceLeftStream xs ys0)

-- | Replace the 'Right' values.
replaceRightStream :: Stream (Either a b) -> Stream c -> Stream (Either a c)
replaceRightStream (Cons sab) (ys0 @ (Cons sc)) = Cons z where
  z = do (a, xs) <- sab
         case a of
           Right _ ->
             do (b, ys) <- sc
                return (Right b, replaceRightStream xs ys)
           Left a ->
             return (Left a, replaceRightStream xs ys0)