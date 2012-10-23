
-- |
-- Module     : Simulation.Aivika.Dynamics.FIFO
-- Copyright  : Copyright (c) 2009-2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- This module defines the FIFO queue.
--
module Simulation.Aivika.Dynamics.FIFO
       (FIFO,
        fifoQueue,
        fifoNull,
        fifoMaxCount,
        fifoCount,
        fifoLostCount,
        newFIFO,
        readFIFO,
        tryReadFIFO,
        writeFIFO,
        tryWriteFIFO,
        writeFIFOOrLost) where

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

-- | Represents the FIFO queue with rule: first input - first output.
data FIFO a =
  FIFO { fifoQueue :: EventQueue,  -- ^ Return the event queue.
         fifoMaxCount :: Int,      -- ^ The maximum available number of items.
         fifoReadRes  :: Resource,
         fifoWriteRes :: Resource,
         fifoCountRef :: IORef Int,
         fifoLostCountRef :: IORef Int,
         fifoStartRef :: IORef Int,
         fifoEndRef   :: IORef Int,
         fifoArray :: IOArray Int a }
  
-- | Create a new FIFO queue with the specified maximum available number of items.  
newFIFO :: EventQueue -> Int -> Simulation (FIFO a)  
newFIFO q count =
  do i <- liftIO $ newIORef 0
     l <- liftIO $ newIORef 0
     s <- liftIO $ newIORef 0
     e <- liftIO $ newIORef 0
     a <- liftIO $ newArray_ (0, count - 1)
     r <- newResourceWithCount q count 0
     w <- newResourceWithCount q count count
     return FIFO { fifoQueue = q,
                   fifoMaxCount = count,
                   fifoReadRes  = r,
                   fifoWriteRes = w,
                   fifoCountRef = i,
                   fifoLostCountRef = l,
                   fifoStartRef = s,
                   fifoEndRef   = e,
                   fifoArray = a }
  
-- | Test whether the FIFO queue is empty.
fifoNull :: FIFO a -> Dynamics Bool
fifoNull fifo =
  do a <- fifoCount fifo
     return (a == 0)

-- | Return the queue size.
fifoCount :: FIFO a -> Dynamics Int
fifoCount fifo =
  liftIO $ readIORef (fifoCountRef fifo)
  
-- | Return the number of lost items.
fifoLostCount :: FIFO a -> Dynamics Int
fifoLostCount fifo =
  liftIO $ readIORef (fifoLostCountRef fifo)
  
-- | Read the FIFO queue.
readFIFO :: FIFO a -> Process a  
readFIFO fifo =
  do requestResource (fifoReadRes fifo)
     a <- liftIO $ readImpl fifo
     releaseResource (fifoWriteRes fifo)
     return a
  
-- | Try to read the FIFO queue.  
tryReadFIFO :: FIFO a -> Dynamics (Maybe a)
tryReadFIFO fifo =
  do x <- tryRequestResourceInDynamics (fifoReadRes fifo)
     if x 
       then do a <- liftIO $ readImpl fifo
               releaseResourceInDynamics (fifoWriteRes fifo)
               return $ Just a
       else return Nothing

-- | Write in the FIFO queue.  
writeFIFO :: FIFO a -> a -> Process ()
writeFIFO fifo a =
  do requestResource (fifoWriteRes fifo)
     liftIO $ writeImpl fifo a
     releaseResource (fifoReadRes fifo)
     
-- | Try to write in the FIFO queue.  
tryWriteFIFO :: FIFO a -> a -> Dynamics Bool
tryWriteFIFO fifo a =
  do x <- tryRequestResourceInDynamics (fifoWriteRes fifo)
     if x 
       then do liftIO $ writeImpl fifo a
               releaseResourceInDynamics (fifoReadRes fifo)
               return True
       else return False

-- | Try to write in the FIFO queue. If the queue is full
-- then the item will be lost.
writeFIFOOrLost :: FIFO a -> a -> Dynamics ()
writeFIFOOrLost fifo a =
  do x <- tryRequestResourceInDynamics (fifoWriteRes fifo)
     if x
       then do liftIO $ writeImpl fifo a
               releaseResourceInDynamics (fifoReadRes fifo)
       else liftIO $ modifyIORef (fifoLostCountRef fifo) $ (+) 1

-- | An implementation method.
readImpl :: FIFO a -> IO a
readImpl fifo =
  do i <- readIORef (fifoCountRef fifo)
     s <- readIORef (fifoStartRef fifo)
     let i' = i - 1
         s' = (s + 1) `mod` (fifoMaxCount fifo)
     a <- readArray (fifoArray fifo) s
     writeArray (fifoArray fifo) s undefined
     i' `seq` writeIORef (fifoCountRef fifo) i'
     s' `seq` writeIORef (fifoStartRef fifo) s'
     return a

-- | An implementation method.
writeImpl :: FIFO a -> a -> IO ()
writeImpl fifo a =
  do i <- readIORef (fifoCountRef fifo)
     e <- readIORef (fifoEndRef fifo)
     let i' = i + 1
         e' = (e + 1) `mod` (fifoMaxCount fifo)
     a `seq` writeArray (fifoArray fifo) e a
     i' `seq` writeIORef (fifoCountRef fifo) i'
     e' `seq` writeIORef (fifoEndRef fifo) e'
