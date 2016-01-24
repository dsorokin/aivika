
-- |
-- Module     : Simulation.Aivika.PriorityQueue.Pure
-- Copyright  : Copyright (c) 2015-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- An immutable heap-based priority queue based on book
-- Algorithms: A Functional Programming Approach by
-- Fethi Rabhi and Guy Lapalme.
--
module Simulation.Aivika.PriorityQueue.Pure 
       (PriorityQueue, 
        queueNull, 
        queueCount,
        emptyQueue, 
        enqueue, 
        dequeue, 
        queueFront) where 

import Control.Monad

-- | The 'PriorityQueue' type represents an immutable heap-based priority queue.
data PriorityQueue a = EmptyQueue
                     | Queue !Int !Double a !Int (PriorityQueue a) (PriorityQueue a)
                       deriving Show

-- | Test whether the priority queue is empty.
queueNull :: PriorityQueue a -> Bool
queueNull EmptyQueue = True
queueNull _          = False

-- | Return the number of elements in the priority queue.
queueCount :: PriorityQueue a -> Int
queueCount EmptyQueue = 0
queueCount (Queue n k v r a b) = n

-- | An empty priority queue.
emptyQueue :: PriorityQueue a
emptyQueue = EmptyQueue

-- | Enqueue a new element with the specified priority.
enqueue :: PriorityQueue a -> Double -> a -> PriorityQueue a
enqueue pq k v = mergeQueues (Queue 1 k v 1 EmptyQueue EmptyQueue) pq

-- | Dequeue the element with the minimal priority.
dequeue :: PriorityQueue a -> PriorityQueue a
dequeue EmptyQueue = error "The queue is empty: dequeue"
dequeue (Queue n k v r a b) = mergeQueues a b

-- | Return the element with the minimal priority.
queueFront :: PriorityQueue a -> (Double, a)
queueFront EmptyQueue = error "The queue is empty: queueFront"
queueFront (Queue n k v r a b) = (k, v)

-- | Return the rank of the priority queue.
queueRank :: PriorityQueue a -> Int
queueRank EmptyQueue = 0
queueRank (Queue n k v r a b) = r

-- | Construct a new priority queue.
makeQueue :: Double -> a -> PriorityQueue a -> PriorityQueue a -> PriorityQueue a
makeQueue k v a b
  | queueRank a >= queueRank b = n `seq` Queue n k v (queueRank b + 1) a b
  | otherwise                  = n `seq` Queue n k v (queueRank a + 1) b a
  where n = queueCount a + queueCount b + 1

-- | Merge two priority queues.
mergeQueues :: PriorityQueue a -> PriorityQueue a -> PriorityQueue a
mergeQueues h EmptyQueue = h
mergeQueues EmptyQueue h = h
mergeQueues h1@(Queue _ k1 v1 _ a1 b1) h2@(Queue _ k2 v2 _ a2 b2)
  | k1 <= k2  = makeQueue k1 v1 a1 (mergeQueues b1 h2)
  | otherwise = makeQueue k2 v2 a2 (mergeQueues h1 b2)
