
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
        lifoMaxCount,
        lifoCount,
        lifoLostCount,
        newLIFO,
        readLIFO,
        tryReadLIFO,
        writeLIFO,
        tryWriteLIFO,
        writeLIFOOrLost) where

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

-- | Return the queue size.
lifoCount :: LIFO a -> Dynamics Int
lifoCount lifo =
  liftIO $ readIORef (lifoCountRef lifo)
  
-- | Return the number of lost items.
lifoLostCount :: LIFO a -> Dynamics Int
lifoLostCount lifo =
  liftIO $ readIORef (lifoLostCountRef lifo)
  
-- | Read the LIFO queue.
readLIFO :: LIFO a -> Process a  
readLIFO lifo =
  do requestResource (lifoReadRes lifo)
     a <- liftIO $ readImpl lifo
     releaseResource (lifoWriteRes lifo)
     return a
  
-- | Try to read the LIFO queue.  
tryReadLIFO :: LIFO a -> Dynamics (Maybe a)
tryReadLIFO lifo =
  do x <- tryRequestResourceInDynamics (lifoReadRes lifo)
     if x 
       then do a <- liftIO $ readImpl lifo
               releaseResourceInDynamics (lifoWriteRes lifo)
               return $ Just a
       else return Nothing

-- | Write in the LIFO queue.  
writeLIFO :: LIFO a -> a -> Process ()
writeLIFO lifo a =
  do requestResource (lifoWriteRes lifo)
     liftIO $ writeImpl lifo a
     releaseResource (lifoReadRes lifo)
     
-- | Try to write in the LIFO queue.  
tryWriteLIFO :: LIFO a -> a -> Dynamics Bool
tryWriteLIFO lifo a =
  do x <- tryRequestResourceInDynamics (lifoWriteRes lifo)
     if x 
       then do liftIO $ writeImpl lifo a
               releaseResourceInDynamics (lifoReadRes lifo)
               return True
       else return False

-- | Try to write in the LIFO queue. If the queue is full
-- then the item will be lost.
writeLIFOOrLost :: LIFO a -> a -> Dynamics ()
writeLIFOOrLost lifo a =
  do x <- tryRequestResourceInDynamics (lifoWriteRes lifo)
     if x
       then do liftIO $ writeImpl lifo a
               releaseResourceInDynamics (lifoReadRes lifo)
       else liftIO $ modifyIORef (lifoLostCountRef lifo) $ (+) 1

-- | An implementation method.
readImpl :: LIFO a -> IO a
readImpl lifo =
  do i <- readIORef (lifoCountRef lifo)
     let j = i - 1
     a <- j `seq` readArray (lifoArray lifo) j
     writeArray (lifoArray lifo) j undefined
     writeIORef (lifoCountRef lifo) j
     return a

-- | An implementation method.
writeImpl :: LIFO a -> a -> IO ()
writeImpl lifo a =
  do i <- readIORef (lifoCountRef lifo)
     let j = i + 1
     a `seq` writeArray (lifoArray lifo) i a
     j `seq` writeIORef (lifoCountRef lifo) j
