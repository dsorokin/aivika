
-- |
-- Module     : Simulation.Aivika.Queue
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- An imperative double-linked queue.
--
module Simulation.Aivika.Queue 
       (Queue, 
        queueNull, 
        queueCount,
        newQueue, 
        enqueue, 
        dequeue, 
        queueFront) where 

import Data.IORef
import Control.Monad

-- | A cell of the double-linked queue.
data QueueItem a = 
  QueueItem { qiVal  :: a,
              qiPrev :: IORef (Maybe (QueueItem a)),
              qiNext :: IORef (Maybe (QueueItem a)) }
  
-- | The 'Queue' type represents an imperative double-linked queue.
data Queue a =  
  Queue { qHead :: IORef (Maybe (QueueItem a)),
          qTail :: IORef (Maybe (QueueItem a)), 
          qSize :: IORef Int }

-- | Test whether the queue is empty.
queueNull :: Queue a -> IO Bool
queueNull q =
  do head <- readIORef (qHead q) 
     case head of
       Nothing -> return True
       Just _  -> return False
    
-- | Return the number of elements in the queue.
queueCount :: Queue a -> IO Int
queueCount q = readIORef (qSize q)

-- | Create a new queue.
newQueue :: IO (Queue a)
newQueue =
  do head <- newIORef Nothing 
     tail <- newIORef Nothing
     size <- newIORef 0
     return Queue { qHead = head, qTail = tail, qSize = size }

-- | Enqueue a new element.
enqueue :: Queue a -> a -> IO ()
enqueue q v =
  do size <- readIORef (qSize q)
     writeIORef (qSize q) (size + 1)
     head <- readIORef (qHead q)
     case head of
       Nothing ->
         do prev <- newIORef Nothing
            next <- newIORef Nothing
            let item = Just QueueItem { qiVal = v, 
                                        qiPrev = prev, 
                                        qiNext = next }
            writeIORef (qHead q) item
            writeIORef (qTail q) item
       Just h ->
         do prev <- newIORef Nothing
            next <- newIORef head
            let item = Just QueueItem { qiVal = v,
                                        qiPrev = prev,
                                        qiNext = next }
            writeIORef (qiPrev h) item
            writeIORef (qHead q) item

-- | Dequeue the first element.
dequeue :: Queue a -> IO ()
dequeue q =
  do tail <- readIORef (qTail q) 
     case tail of
       Nothing ->
         error "Empty queue: dequeue"
       Just t ->
         do size  <- readIORef (qSize q)
            writeIORef (qSize q) (size - 1)
            tail' <- readIORef (qiPrev t)
            case tail' of
              Nothing ->
                do writeIORef (qHead q) Nothing
                   writeIORef (qTail q) Nothing
              Just t' ->
                do writeIORef (qiNext t') Nothing
                   writeIORef (qTail q) tail'

-- | Return the first element.
queueFront :: Queue a -> IO a
queueFront q =
  do tail <- readIORef (qTail q)
     case tail of
       Nothing ->
         error "Empty queue: front"
       Just t ->
         return $ qiVal t
