
-- |
-- Module     : Simulation.Aivika.Dynamics.Buffer
-- Copyright  : Copyright (c) 2009-2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- This module defines the limited queue similar to 'LIFO' and 'FIFO' but where 
-- the items are not represented. We know only of their number in the buffer and 
-- how many items were lost.
--
module Simulation.Aivika.Dynamics.FIFO
       (Buffer,
        bufferQueue,
        bufferNull,
        bufferFull,
        bufferMaxCount,
        bufferCount,
        bufferLostCount,
        newBuffer,
        dequeueBuffer,
        tryDequeueBuffer,
        enqueueBuffer,
        tryEnqueueBuffer,
        enqueueBufferOrLost) where

import Data.IORef
import Data.Array
import Data.Array.IO

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Process
import Simulation.Aivika.Dynamics.Resource

-- | Represents the limited queue similar to 'LIFO' and 'FIFO' but where the items are not repsented.
-- So, there is no order of items but their number is strictly limited.
data Buffer =
  Buffer { bufferQueue :: EventQueue,  -- ^ Return the event queue.
           bufferMaxCount :: Int,      -- ^ The maximum available number of items.
           bufferReadRes  :: Resource,
           bufferWriteRes :: Resource,
           bufferCountRef :: IORef Int,
           bufferLostCountRef :: IORef Int }
  
-- | Create a new queue with the specified maximum available number of items.  
newBuffer :: EventQueue -> Int -> Simulation Buffer  
newBuffer q count =
  do i <- liftIO $ newIORef 0
     l <- liftIO $ newIORef 0
     r <- newResourceWithCount q count 0
     w <- newResourceWithCount q count count
     return Buffer { bufferQueue = q,
                     bufferMaxCount = count,
                     bufferReadRes  = r,
                     bufferWriteRes = w,
                     bufferCountRef = i,
                     bufferLostCountRef = l }
  
-- | Test whether the queue is empty.
bufferNull :: Buffer -> Dynamics Bool
bufferNull q =
  do a <- bufferCount q
     return (a == 0)

-- | Test whether the queue is full.
bufferFull :: Buffer -> Dynamics Bool
bufferFull q =
  do a <- bufferCount q
     return (a == bufferMaxCount q)

-- | Return the queue size.
bufferCount :: Buffer -> Dynamics Int
bufferCount q =
  liftIO $ readIORef (bufferCountRef q)
  
-- | Return the number of lost items.
bufferLostCount :: Buffer -> Dynamics Int
bufferLostCount q =
  liftIO $ readIORef (bufferLostCountRef q)
  
-- | Dequeue suspending the process if the buffer is empty.
dequeueBuffer :: Buffer -> Process ()
dequeueBuffer q =
  do requestResource (bufferReadRes q)
     liftIO $ dequeueImpl q
     releaseResource (bufferWriteRes q)
  
-- | Try to dequeue immediately.  
tryDequeueBuffer :: Buffer -> Dynamics Bool
tryDequeueBuffer q =
  do x <- tryRequestResourceInDynamics (bufferReadRes q)
     if x 
       then do liftIO $ dequeueImpl q
               releaseResourceInDynamics (bufferWriteRes q)
               return True
       else return False

-- | Enqueue the item suspending the process 
-- if the buffer is full.  
enqueueBuffer :: Buffer -> Process ()
enqueueBuffer q =
  do requestResource (bufferWriteRes q)
     liftIO $ enqueueImpl q
     releaseResource (bufferReadRes q)
     
-- | Try to enqueue the item immediately.  
tryEnqueueBuffer :: Buffer -> Dynamics Bool
tryEnqueueBuffer q =
  do x <- tryRequestResourceInDynamics (bufferWriteRes q)
     if x 
       then do liftIO $ enqueueImpl q
               releaseResourceInDynamics (bufferReadRes q)
               return True
       else return False

-- | Try to enqueue the item. If the buffer is full
-- then the item will be lost.
enqueueBufferOrLost :: Buffer -> Dynamics ()
enqueueBufferOrLost q =
  do x <- tryRequestResourceInDynamics (bufferWriteRes q)
     if x
       then do liftIO $ enqueueImpl q
               releaseResourceInDynamics (bufferReadRes q)
       else liftIO $ modifyIORef (bufferLostCountRef q) $ (+) 1

-- | An implementation method.
dequeueImpl :: Buffer -> IO ()
dequeueImpl q =
  do i <- readIORef (bufferCountRef q)
     let i' = i - 1
     i' `seq` writeIORef (bufferCountRef q) i'

-- | An implementation method.
enqueueImpl :: Buffer -> IO ()
enqueueImpl q =
  do i <- readIORef (bufferCountRef q)
     let i' = i + 1
     i' `seq` writeIORef (bufferCountRef q) i'
