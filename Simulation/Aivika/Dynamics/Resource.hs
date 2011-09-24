
-- |
-- Module     : Simulation.Aivika.Dynamics.Resource
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module defines a limited resource which can be acquired and 
-- then released by the discontinuous process 'DynamicProc'.
--
module Simulation.Aivika.Dynamics.Resource
       (Resource,
        newResource,
        resourceQueue,
        resourceInitCount,
        resourceCount,
        requestResource,
        releaseResource) where

import Data.IORef
import Control.Monad

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Cont
import Simulation.Aivika.Dynamics.Internal.Process
import Simulation.Aivika.Dynamics.EventQueue
import qualified Simulation.Aivika.Queue as Q

-- | Represents a limited resource.
data Resource = 
  Resource { resourceQueue     :: EventQueue,  
             -- ^ Return the bound event queue.
             resourceInitCount :: Int,
             -- ^ Return the initial count of the resource.
             resourceCountRef  :: IORef Int, 
             resourceWaitQueue :: Q.Queue (Dynamics (() -> IO ()))}

instance Eq Resource where
  x == y = resourceCountRef x == resourceCountRef y  -- unique references

-- | Create a new resource with the specified initial count.
newResource :: EventQueue -> Int -> Dynamics Resource
newResource q initCount =
  Dynamics $ \p ->
  do countRef  <- newIORef initCount
     waitQueue <- Q.newQueue
     return Resource { resourceQueue     = q,
                       resourceInitCount = initCount,
                       resourceCountRef  = countRef,
                       resourceWaitQueue = waitQueue }

-- | Return the current count of the resource.
resourceCount :: Resource -> Process Int
resourceCount r =
  Process $ \_ ->
  Cont $ \(Dynamics c) ->
  Dynamics $ \p ->
  do cont' <- c p 
     a <- readIORef (resourceCountRef r)
     cont' a

-- | Request for the resource decreasing its count in case of success,
-- otherwise suspending the discontinuous process until some other 
-- process releases the resource.
requestResource :: Resource -> Process ()
requestResource r =
  Process $ \_ ->
  Cont $ \c@(Dynamics cont) ->
  Dynamics $ \p ->
  do a <- readIORef (resourceCountRef r)
     if a == 0 
       then Q.enqueue (resourceWaitQueue r) c
       else do let a' = a - 1
               a' `seq` writeIORef (resourceCountRef r) a'
               cont' <- cont p
               cont' ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended processes as possible.
releaseResource :: Resource -> Process ()
releaseResource r =
  Process $ \_ ->
  Cont $ \(Dynamics c) ->
  Dynamics $ \p ->
  do a <- readIORef (resourceCountRef r)
     let a' = a + 1
     when (a' > resourceInitCount r) $
       error $
       "The resource count cannot be greater than " ++
       "its initial value: releaseResource."
     f <- Q.queueNull (resourceWaitQueue r)
     if f 
       then a' `seq` writeIORef (resourceCountRef r) a'
       else do c2 <- Q.queueFront (resourceWaitQueue r)
               Q.dequeue (resourceWaitQueue r)
               let Dynamics m = enqueueCont (resourceQueue r) (pointTime p) c2
               m p
     cont' <- c p
     cont' ()
