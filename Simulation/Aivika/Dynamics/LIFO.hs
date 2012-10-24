
-- |
-- Module     : Simulation.Aivika.Dynamics.LIFO
-- Copyright  : Copyright (c) 2009-2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- This module defines the LIFO queue.
--
module Simulation.Aivika.Dynamics.LIFO
       (LIFO,
        lifoQueue,
        lifoNull,
        lifoFull,
        lifoMaxCount,
        lifoCount,
        lifoLostCount,
        newLIFO,
        dequeueLIFO,
        tryDequeueLIFO,
        enqueueLIFO,
        tryEnqueueLIFO,
        enqueueLIFOOrLost) where

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

-- | Represents the LIFO queue with rule: last input - first output.
data LIFO a =
  LIFO { lifoQueue :: EventQueue,  -- ^ Return the event queue.
         lifoMaxCount :: Int,      -- ^ The maximum available number of items.
         lifoReadRes  :: Resource,
         lifoWriteRes :: Resource,
         lifoCountRef :: IORef Int,
         lifoLostCountRef :: IORef Int,
         lifoArray :: IOArray Int a }
  
-- | Create a new LIFO queue with the specified maximum available number of items.  
newLIFO :: EventQueue -> Int -> Simulation (LIFO a)  
newLIFO q count =
  do i <- liftIO $ newIORef 0
     l <- liftIO $ newIORef 0
     a <- liftIO $ newArray_ (0, count - 1)
     r <- newResourceWithCount q count 0
     w <- newResourceWithCount q count count
     return LIFO { lifoQueue = q,
                   lifoMaxCount = count,
                   lifoReadRes  = r,
                   lifoWriteRes = w,
                   lifoCountRef = i,
                   lifoLostCountRef = l,
                   lifoArray = a }
  
-- | Test whether the LIFO queue is empty.
lifoNull :: LIFO a -> Dynamics Bool
lifoNull lifo =
  do a <- lifoCount lifo
     return (a == 0)

-- | Test whether the LIFO queue is full.
lifoFull :: LIFO a -> Dynamics Bool
lifoFull lifo =
  do a <- lifoCount lifo
     return (a == lifoMaxCount lifo)

-- | Return the queue size.
lifoCount :: LIFO a -> Dynamics Int
lifoCount lifo =
  liftIO $ readIORef (lifoCountRef lifo)
  
-- | Return the number of lost items.
lifoLostCount :: LIFO a -> Dynamics Int
lifoLostCount lifo =
  liftIO $ readIORef (lifoLostCountRef lifo)
  
-- | Dequeue from the LIFO queue suspending the process if
-- the queue is empty.
dequeueLIFO :: LIFO a -> Process a  
dequeueLIFO lifo =
  do requestResource (lifoReadRes lifo)
     a <- liftIO $ dequeueImpl lifo
     releaseResource (lifoWriteRes lifo)
     return a
  
-- | Try to dequeue from the LIFO queue immediately.  
tryDequeueLIFO :: LIFO a -> Dynamics (Maybe a)
tryDequeueLIFO lifo =
  do x <- tryRequestResourceInDynamics (lifoReadRes lifo)
     if x 
       then do a <- liftIO $ dequeueImpl lifo
               releaseResourceInDynamics (lifoWriteRes lifo)
               return $ Just a
       else return Nothing

-- | Enqueue the item in the LIFO queue suspending the process if
-- the queue is full.  
enqueueLIFO :: LIFO a -> a -> Process ()
enqueueLIFO lifo a =
  do requestResource (lifoWriteRes lifo)
     liftIO $ enqueueImpl lifo a
     releaseResource (lifoReadRes lifo)
     
-- | Try to enqueue the item in the LIFO queue. Return 'False' in
-- the monad if the queue is full.
tryEnqueueLIFO :: LIFO a -> a -> Dynamics Bool
tryEnqueueLIFO lifo a =
  do x <- tryRequestResourceInDynamics (lifoWriteRes lifo)
     if x 
       then do liftIO $ enqueueImpl lifo a
               releaseResourceInDynamics (lifoReadRes lifo)
               return True
       else return False

-- | Try to enqueue the item in the LIFO queue. If the queue is full
-- then the item will be lost.
enqueueLIFOOrLost :: LIFO a -> a -> Dynamics ()
enqueueLIFOOrLost lifo a =
  do x <- tryRequestResourceInDynamics (lifoWriteRes lifo)
     if x
       then do liftIO $ enqueueImpl lifo a
               releaseResourceInDynamics (lifoReadRes lifo)
       else liftIO $ modifyIORef (lifoLostCountRef lifo) $ (+) 1

-- | An implementation method.
dequeueImpl :: LIFO a -> IO a
dequeueImpl lifo =
  do i <- readIORef (lifoCountRef lifo)
     let j = i - 1
     a <- j `seq` readArray (lifoArray lifo) j
     writeArray (lifoArray lifo) j undefined
     writeIORef (lifoCountRef lifo) j
     return a

-- | An implementation method.
enqueueImpl :: LIFO a -> a -> IO ()
enqueueImpl lifo a =
  do i <- readIORef (lifoCountRef lifo)
     let j = i + 1
     a `seq` writeArray (lifoArray lifo) i a
     j `seq` writeIORef (lifoCountRef lifo) j
