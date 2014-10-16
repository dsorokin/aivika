
-- |
-- Module     : Simulation.Aivika.Trans.PriorityQueue
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- An imperative heap-based priority queue.
--
module Simulation.Aivika.Trans.PriorityQueue 
       (PriorityQueue,
        queueNull, 
        queueCount,
        newQueue, 
        enqueue, 
        dequeue, 
        queueFront) where 

import Control.Monad

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp

import qualified Simulation.Aivika.Trans.ProtoArray as A
import qualified Simulation.Aivika.Trans.ProtoArray.Unboxed as UA

-- | The 'PriorityQueue' type represents an imperative heap-based 
-- priority queue.
data PriorityQueue m a = 
  PriorityQueue { pqSession  :: Session m,
                  pqKeys     :: ProtoRef m (UA.ProtoArray m Double),
                  pqVals     :: ProtoRef m (A.ProtoArray m a),
                  pqSize     :: ProtoRef m Int,
                  pqCapacity :: ProtoRef m Int }

increase :: ProtoComp m => PriorityQueue m a -> m ()
{-# INLINABLE increase #-}
{-# SPECIALISE increase :: PriorityQueue IO Double -> IO () #-}
increase pq = 
  do let s = pqSession pq
         keyRef = pqKeys pq
         valRef = pqVals pq
         capacityRef = pqCapacity pq
     keys <- readProtoRef keyRef
     vals <- readProtoRef valRef
     len  <- readProtoRef capacityRef
     let capacity' | len < 64  = 2 * len
                   | otherwise = (len `div` 2) * 3
     keys' <- UA.newProtoArray_ s capacity'
     vals' <- A.newProtoArray_ s capacity'
     mapM_ (\i -> do { k <- UA.readProtoArray keys i; UA.writeProtoArray keys' i k }) [0 .. len - 1]
     mapM_ (\i -> do { v <- A.readProtoArray vals i; A.writeProtoArray vals' i v }) [0 .. len - 1]
     writeProtoRef keyRef keys'
     writeProtoRef valRef vals'
     writeProtoRef capacityRef capacity'

siftUp :: ProtoComp m 
          => UA.ProtoArray m Double
          -- ^ keys
          -> A.ProtoArray m a
          -- ^ values
          -> Int
          -- ^ index
          -> Double
          -- ^ key
          -> a
          -- ^ value
          -> m ()
{-# INLINABLE siftUp #-}
{-# SPECIALISE siftUp :: UA.ProtoArray IO Double -> A.ProtoArray IO a -> Int -> Double -> a -> IO () #-}
siftUp keys vals i k v =
  if i == 0 
  then do UA.writeProtoArray keys i k
          A.writeProtoArray vals i v
  else do let n = (i - 1) `div` 2
          kn <- UA.readProtoArray keys n
          if k >= kn 
            then do UA.writeProtoArray keys i k
                    A.writeProtoArray vals i v
            else do vn <- A.readProtoArray vals n
                    UA.writeProtoArray keys i kn
                    A.writeProtoArray vals i vn
                    siftUp keys vals n k v

siftDown :: ProtoComp m 
            => UA.ProtoArray m Double
            -- ^ keys
            -> A.ProtoArray m a
            -- ^ values
            -> Int
            -- ^ size
            -> Int
            -- ^ index
            -> Double
            -- ^ key
            -> a
            -- ^ value
            -> m ()
{-# INLINABLE siftDown #-}
{-# SPECIALISE siftDown :: UA.ProtoArray IO Double -> A.ProtoArray IO a -> Int -> Int -> Double -> a -> IO () #-}
siftDown keys vals size i k v =
  if i >= (size `div` 2)
  then do UA.writeProtoArray keys i k
          A.writeProtoArray vals i v
  else do let n  = 2 * i + 1
              n' = n + 1
          kn  <- UA.readProtoArray keys n
          if n' >= size 
            then if k <= kn
                 then do UA.writeProtoArray keys i k
                         A.writeProtoArray vals i v
                 else do vn <- A.readProtoArray vals n
                         UA.writeProtoArray keys i kn
                         A.writeProtoArray vals i vn
                         siftDown keys vals size n k v
            else do kn' <- UA.readProtoArray keys n'
                    let n''  = if kn > kn' then n' else n
                        kn'' = min kn' kn
                    if k <= kn''
                      then do UA.writeProtoArray keys i k
                              A.writeProtoArray vals i v
                      else do vn'' <- A.readProtoArray vals n''
                              UA.writeProtoArray keys i kn''
                              A.writeProtoArray vals i vn''
                              siftDown keys vals size n'' k v

-- | Test whether the priority queue is empty.
queueNull :: ProtoComp m => PriorityQueue m a -> m Bool
{-# INLINABLE queueNull #-}
{-# SPECIALISE queueNull :: PriorityQueue IO a -> IO Bool #-}
queueNull pq =
  do size <- readProtoRef (pqSize pq)
     return $ size == 0

-- | Return the number of elements in the priority queue.
queueCount :: ProtoComp m => PriorityQueue m a -> m Int
{-# INLINABLE queueCount #-}
{-# SPECIALISE queueCount :: PriorityQueue IO a -> IO Int #-}
queueCount pq = readProtoRef (pqSize pq)

-- | Create a new priority queue.
newQueue :: ProtoComp m => Session m -> m (PriorityQueue m a)
{-# INLINABLE newQueue #-}
{-# SPECIALISE newQueue :: Session IO -> IO (PriorityQueue IO a) #-}
newQueue session =
  do keys        <- UA.newProtoArray_ session 11
     vals        <- A.newProtoArray_ session 11
     keyRef      <- newProtoRef session keys
     valRef      <- newProtoRef session vals
     sizeRef     <- newProtoRef session 0
     capacityRef <- newProtoRef session 11
     return PriorityQueue { pqSession = session,
                            pqKeys = keyRef, 
                            pqVals = valRef, 
                            pqSize = sizeRef,
                            pqCapacity = capacityRef }

-- | Enqueue a new element with the specified priority.
enqueue :: ProtoComp m => PriorityQueue m a -> Double -> a -> m ()
{-# INLINABLE enqueue #-}
{-# SPECIALISE enqueue :: PriorityQueue IO a -> Double -> a -> IO () #-}
enqueue pq k v =
  do i <- readProtoRef (pqSize pq)
     n <- readProtoRef (pqCapacity pq)
     when (i >= n - 1) $ increase pq
     writeProtoRef (pqSize pq) (i + 1)
     keys <- readProtoRef (pqKeys pq)
     vals <- readProtoRef (pqVals pq)
     siftUp keys vals i k v

-- | Dequeue the element with the minimal priority.
dequeue :: ProtoComp m => PriorityQueue m a -> m ()
{-# INLINABLE dequeue #-}
{-# SPECIALISE dequeue :: PriorityQueue IO a -> IO () #-}
dequeue pq =
  do size <- readProtoRef (pqSize pq)
     when (size == 0) $ error "Empty priority queue: dequeue"
     let i = size - 1
     writeProtoRef (pqSize pq) i
     keys <- readProtoRef (pqKeys pq)
     vals <- readProtoRef (pqVals pq)
     k  <- UA.readProtoArray keys i
     v  <- A.readProtoArray vals i
     let k0 = 0.0
         v0 = undefined
     UA.writeProtoArray keys i k0
     A.writeProtoArray vals i v0
     siftDown keys vals i 0 k v

-- | Return the element with the minimal priority.
queueFront :: ProtoComp m => PriorityQueue m a -> m (Double, a)
{-# INLINABLE queueFront #-}
{-# SPECIALISE queueFront :: PriorityQueue IO a -> IO (Double, a) #-}
queueFront pq =
  do size <- readProtoRef (pqSize pq)
     when (size == 0) $ error "Empty priority queue: queueFront"
     keys <- readProtoRef (pqKeys pq)
     vals <- readProtoRef (pqVals pq)
     k <- UA.readProtoArray keys 0
     v <- A.readProtoArray vals 0
     return (k, v)
