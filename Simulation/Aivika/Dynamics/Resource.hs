
-- |
-- Module     : Simulation.Aivika.Dynamics.Resource
-- Copyright  : Copyright (c) 2009-2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module defines a limited resource which can be acquired and 
-- then released by the discontinuous process 'Process'.
--
module Simulation.Aivika.Dynamics.Resource
       (Resource,
        newResource,
        newResourceWithCount,
        resourceQueue,
        resourceInitCount,
        resourceCount,
        requestResource,
        tryRequestResourceInDynamics,
        releaseResource,
        releaseResourceInDynamics,
        usingResource) where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.Internal.Simulation
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
             resourceWaitQueue :: Q.Queue (ContParams ())}

instance Eq Resource where
  x == y = resourceCountRef x == resourceCountRef y  -- unique references

-- | Create a new resource with the specified initial count.
newResource :: EventQueue -> Int -> Simulation Resource
newResource q initCount =
  Simulation $ \r ->
  do countRef  <- newIORef initCount
     waitQueue <- Q.newQueue
     return Resource { resourceQueue     = q,
                       resourceInitCount = initCount,
                       resourceCountRef  = countRef,
                       resourceWaitQueue = waitQueue }

-- | Create a new resource with the specified initial count.
-- The third argument specifies how the resource is consumed 
-- at the beginning, i.e. it defines the current count, which must be 
-- non-negative and less or equal to the initial count.
newResourceWithCount :: EventQueue -> Int -> Int -> Simulation Resource
newResourceWithCount q initCount count = do
  when (count < 0) $
    error $
    "The resource count cannot be negative: " ++
    "newResourceWithCount."
  when (count > initCount) $
    error $
    "The resource count cannot be greater than " ++
    "its initial value: newResourceWithCount."
  Simulation $ \r ->
    do countRef  <- newIORef count
       waitQueue <- Q.newQueue
       return Resource { resourceQueue     = q,
                         resourceInitCount = initCount,
                         resourceCountRef  = countRef,
                         resourceWaitQueue = waitQueue }

-- | Return the current count of the resource.
resourceCount :: Resource -> Dynamics Int
resourceCount r =
  Dynamics $ \p ->
  do invokeDynamics p $ queueRun (resourceQueue r)
     readIORef (resourceCountRef r)

-- | Request for the resource decreasing its count in case of success,
-- otherwise suspending the discontinuous process until some other 
-- process releases the resource.
requestResource :: Resource -> Process ()
requestResource r =
  Process $ \pid ->
  Cont $ \c ->
  Dynamics $ \p ->
  do a <- readIORef (resourceCountRef r)
     if a == 0 
       then Q.enqueue (resourceWaitQueue r) c
       else do let a' = a - 1
               a' `seq` writeIORef (resourceCountRef r) a'
               invokeDynamics p $ resumeContByParams c ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended processes as possible.
releaseResource :: Resource -> Process ()
releaseResource r = 
  Process $ \_ ->
  Cont $ \c ->
  Dynamics $ \p ->
  do invokeDynamics p $ releaseResourceUnsafe r
     invokeDynamics p $ resumeContByParams c ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended processes as possible.
releaseResourceInDynamics :: Resource -> Dynamics ()
releaseResourceInDynamics r =
  Dynamics $ \p ->
  do invokeDynamics p $ queueRun (resourceQueue r)
     invokeDynamics p $ releaseResourceUnsafe r

releaseResourceUnsafe :: Resource -> Dynamics ()
{-# INLINE releaseResourceUnsafe #-}
releaseResourceUnsafe r =
  Dynamics $ \p ->
  do a <- readIORef (resourceCountRef r)
     let a' = a + 1
     when (a' > resourceInitCount r) $
       error $
       "The resource count cannot be greater than " ++
       "its initial value: releaseResourceUnsafe."
     f <- Q.queueNull (resourceWaitQueue r)
     if f 
       then a' `seq` writeIORef (resourceCountRef r) a'
       else do c <- Q.queueFront (resourceWaitQueue r)
               Q.dequeue (resourceWaitQueue r)
               invokeDynamics p $ enqueue (resourceQueue r) (pointTime p) $
                 Dynamics $ \p ->
                 do z <- contParamsCanceled c
                    if z
                      then do invokeDynamics p $ releaseResourceUnsafe r
                              invokeDynamics p $ resumeContByParams c ()
                      else invokeDynamics p $ resumeContByParams c ()

-- | Try to request for the resource decreasing its count in case of success
-- and returning 'True' in the 'Dynamics' monad; otherwise, returning 'False'.
tryRequestResourceInDynamics :: Resource -> Dynamics Bool
tryRequestResourceInDynamics r =
  Dynamics $ \p ->
  do invokeDynamics p $ queueRun (resourceQueue r)
     a <- readIORef (resourceCountRef r)
     if a == 0 
       then return False
       else do let a' = a - 1
               a' `seq` writeIORef (resourceCountRef r) a'
               return True
               
-- | Acquire the resource, perform some action and safely release the resource               
-- in the end, even if the 'IOException' was raised within the action. 
-- The process identifier must be created with support of exception 
-- handling, i.e. with help of function 'newProcessIDWithCatch'. Unfortunately,
-- such processes are slower than those that are created with help of
-- other function 'newProcessID'.
usingResource :: Resource -> Process a -> Process a
usingResource r m =
  do requestResource r
     finallyProcess m $ releaseResource r

invokeDynamics :: Point -> Dynamics a -> IO a
{-# INLINE invokeDynamics #-}
invokeDynamics p (Dynamics m) = m p 