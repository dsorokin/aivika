
-- Copyright (c) 2009, 2010, 2011 David Sorokin <david.sorokin@gmail.com>
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

-- This is an imperative version of the heap-based priority queue in monad IO.

module Simulation.Aivika.PriorityQueue 
       (PriorityQueue, 
        queueNull, 
        newQueue, 
        enqueue, 
        dequeue, 
        queueFront) where 

import Data.Array
import Data.Array.MArray
import Data.Array.IO
import Data.IORef
import Control.Monad

data PriorityQueue a = 
  PriorityQueue { pqKeys  :: IORef (IOUArray Int Double),
                  pqVals  :: IORef (IOArray Int a),
                  pqSize  :: IORef Int,
                  pqNoVal :: a    -- to release references                 
                }

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

queueNull :: PriorityQueue a -> IO Bool
queueNull pq =
  do size <- readIORef (pqSize pq)
     return $ size == 0

newQueue :: a -> IO (PriorityQueue a)
newQueue defaultValue =
  do keys <- newArray_ (0, 10)
     vals <- newArray_ (0, 10)
     keyRef  <- newIORef keys
     valRef  <- newIORef vals
     sizeRef <- newIORef 0
     return PriorityQueue { pqKeys = keyRef, 
                            pqVals = valRef, 
                            pqSize = sizeRef,
                            pqNoVal = defaultValue }

enqueue :: PriorityQueue a -> Double -> a -> IO ()
enqueue pq k v =
  do i <- readIORef (pqSize pq)
     keys <- readIORef (pqKeys pq)
     (il, iu) <- getBounds keys
     when (i >= iu - il + 1) $ increase pq (i + 1)
     writeIORef (pqSize pq) (i + 1)
     keys <- readIORef (pqKeys pq)  -- it can be another! (side-effect)
     vals <- readIORef (pqVals pq)
     siftUp keys vals i k v

dequeue :: PriorityQueue a -> IO ()
dequeue pq =
  do size <- readIORef (pqSize pq)
     when (size == 0) $ error "Empty priority queue: dequeue"
     let i = size - 1
     writeIORef (pqSize pq) i
     keys <- readIORef (pqKeys pq)
     vals <- readIORef (pqVals pq)
     k <- readArray keys i
     v <- readArray vals i
     writeArray keys i 0.0
     writeArray vals i (pqNoVal pq)    -- to release the reference!
     siftDown keys vals i 0 k v

queueFront :: PriorityQueue a -> IO (Double, a)
queueFront pq =
  do size <- readIORef (pqSize pq)
     when (size == 0) $ error "Empty priority queue: front"
     keys <- readIORef (pqKeys pq)
     vals <- readIORef (pqVals pq)
     k <- readArray keys 0
     v <- readArray vals 0
     return (k, v)