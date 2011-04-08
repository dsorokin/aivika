
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

-- This is an imperative version of the queue in monad IO.

module Simulation.Aivika.Queue 
       (Queue, 
        queueNull, 
        newQueue, 
        enqueue, 
        dequeue, 
        queueFront) where 

import Data.IORef
import Control.Monad

data QueueItem a = 
  QueueItem { qiVal  :: a,
              qiPrev :: IORef (Maybe (QueueItem a)),
              qiNext :: IORef (Maybe (QueueItem a)) }
  
data Queue a =  
  Queue { qHead :: IORef (Maybe (QueueItem a)),
          qTail :: IORef (Maybe (QueueItem a)) }

queueNull :: Queue a -> IO Bool
queueNull q =
  do head <- readIORef (qHead q) 
     case head of
       Nothing -> return True
       Just _  -> return False
    
newQueue :: IO (Queue a)
newQueue =
  do head <- newIORef Nothing 
     tail <- newIORef Nothing
     return Queue { qHead = head, qTail = tail }

enqueue :: Queue a -> a -> IO ()
enqueue q v =
  do head <- readIORef (qHead q)
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

dequeue :: Queue a -> IO ()
dequeue q =
  do tail <- readIORef (qTail q) 
     case tail of
       Nothing ->
         error "Empty queue: dequeue"
       Just t ->
         do tail' <- readIORef (qiPrev t)
            case tail' of
              Nothing ->
                do writeIORef (qHead q) Nothing
                   writeIORef (qTail q) Nothing
              Just t' ->
                do writeIORef (qiNext t') Nothing
                   writeIORef (qTail q) tail'

queueFront :: Queue a -> IO a
queueFront q =
  do tail <- readIORef (qTail q)
     case tail of
       Nothing ->
         error "Empty queue: front"
       Just t ->
         return $ qiVal t
