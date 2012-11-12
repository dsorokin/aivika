
-- |
-- Module     : Simulation.Aivika.PriorityQueue
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- An imperative heap-based priority queue.
--
module Simulation.Aivika.PriorityQueue 
       (PriorityQueue, 
        queueNull, 
        queueCount,
        newQueue, 
        enqueue, 
        dequeue, 
        queueFront) where 

import Data.Array
import Data.Array.MArray.Safe
import Data.Array.IO.Safe
import Data.IORef
import Control.Monad

-- | The 'PriorityQueue' type represents an imperative heap-based 
-- priority queue.
data PriorityQueue a = 
  PriorityQueue { pqKeys  :: IORef (IOUArray Int Double),
                  pqVals  :: IORef (IOArray Int a),
                  pqSize  :: IORef Int }

increase :: PriorityQueue a -> Int -> IO ()
increase pq capacity = 
  do let keyRef = pqKeys pq
         valRef = pqVals pq
     keys <- readIORef keyRef
     vals <- readIORef valRef
     (il, iu)  <- getBounds keys
     let len = (iu - il) + 1
         capacity' | len < 64  = max capacity ((len + 1) * 2)
                   | otherwise = max capacity ((len `div` 2) * 3)
         il' = il
         iu' = il + capacity' - 1
     keys' <- newArray_ (il', iu')
     vals' <- newArray_ (il', iu')
     mapM_ (\i -> do { k <- readArray keys i; writeArray keys' i k }) [il..iu]
     mapM_ (\i -> do { v <- readArray vals i; writeArray vals' i v }) [il..iu]
     writeIORef keyRef keys'
     writeIORef valRef vals'

siftUp :: IOUArray Int Double 
         -> IOArray Int a
         -> Int -> Double -> a 
         -> IO ()
siftUp keys vals i k v =
  if i == 0 
  then do writeArray keys i k
          writeArray vals i v
  else do let n = (i - 1) `div` 2
          kn <- readArray keys n
          if k >= kn 
            then do writeArray keys i k
                    writeArray vals i v
            else do vn <- readArray vals n
                    writeArray keys i kn
                    writeArray vals i vn
                    siftUp keys vals n k v

siftDown :: IOUArray Int Double 
           -> IOArray Int a -> Int
           -> Int -> Double -> a 
           -> IO ()
siftDown keys vals size i k v =
  if i >= (size `div` 2)
  then do writeArray keys i k
          writeArray vals i v
  else do let n  = 2 * i + 1
              n' = n + 1
          kn  <- readArray keys n
          if n' >= size 
            then if k <= kn
                 then do writeArray keys i k
                         writeArray vals i v
                 else do vn <- readArray vals n
                         writeArray keys i kn
                         writeArray vals i vn
                         siftDown keys vals size n k v
            else do kn' <- readArray keys n'
                    let n''  = if kn > kn' then n' else n
                        kn'' = min kn' kn
                    if k <= kn''
                      then do writeArray keys i k
                              writeArray vals i v
                      else do vn'' <- readArray vals n''
                              writeArray keys i kn''
                              writeArray vals i vn''
                              siftDown keys vals size n'' k v

-- | Test whether the priority queue is empty.
queueNull :: PriorityQueue a -> IO Bool
queueNull pq =
  do size <- readIORef (pqSize pq)
     return $ size == 0

-- | Return the number of elements in the priority queue.
queueCount :: PriorityQueue a -> IO Int
queueCount pq = readIORef (pqSize pq)

-- | Create a new priority queue.
newQueue :: IO (PriorityQueue a)
newQueue =
  do keys <- newArray_ (0, 10)
     vals <- newArray_ (0, 10)
     keyRef  <- newIORef keys
     valRef  <- newIORef vals
     sizeRef <- newIORef 0
     return PriorityQueue { pqKeys = keyRef, 
                            pqVals = valRef, 
                            pqSize = sizeRef }

-- | Enqueue a new element with the specified priority.
enqueue :: PriorityQueue a -> Double -> a -> IO ()
enqueue pq k v =
  do i <- readIORef (pqSize pq)
     keys <- readIORef (pqKeys pq)
     (il, iu) <- getBounds keys
     when (i >= iu - il) $ increase pq (i + 2)  -- plus one element on the end
     writeIORef (pqSize pq) (i + 1)
     keys <- readIORef (pqKeys pq)  -- it can be another! (side-effect)
     vals <- readIORef (pqVals pq)
     siftUp keys vals i k v

-- | Dequeue the element with the minimal priority.
dequeue :: PriorityQueue a -> IO ()
dequeue pq =
  do size <- readIORef (pqSize pq)
     when (size == 0) $ error "Empty priority queue: dequeue"
     let i = size - 1
     writeIORef (pqSize pq) i
     keys <- readIORef (pqKeys pq)
     vals <- readIORef (pqVals pq)
     k  <- readArray keys i
     v  <- readArray vals i
     k0 <- readArray keys size
     v0 <- readArray vals size
     writeArray keys i k0
     writeArray vals i v0
     siftDown keys vals i 0 k v

-- | Return the element with the minimal priority.
queueFront :: PriorityQueue a -> IO (Double, a)
queueFront pq =
  do size <- readIORef (pqSize pq)
     when (size == 0) $ error "Empty priority queue: front"
     keys <- readIORef (pqKeys pq)
     vals <- readIORef (pqVals pq)
     k <- readArray keys 0
     v <- readArray vals 0
     return (k, v)