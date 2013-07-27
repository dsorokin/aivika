
-- |
-- Module     : Simulation.Aivika.Resource
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines a limited resource which can be acquired and 
-- then released by the discontinuous process 'Process'.
--
module Simulation.Aivika.Resource
       (Resource,
        newResource,
        newResourceWithCount,
        resourceMaxCount,
        resourceCount,
        requestResource,
        requestResourceWithPriority,
        tryRequestResourceWithinEvent,
        releaseResource,
        releaseResourceWithinEvent,
        usingResource,
        usingResourceWithPriority) where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Cont
import Simulation.Aivika.Internal.Process

import Simulation.Aivika.QueueStrategy

-- | Represents a limited resource.
data Resource s q = 
  Resource { resourceStrategy :: s,
             resourceMaxCount :: Int,
             -- ^ Return the maximum count of the resource.
             resourceCountRef :: IORef Int, 
             resourceWaitList :: q (ContParams ())}

instance Eq (Resource s q) where
  x == y = resourceCountRef x == resourceCountRef y  -- unique references

-- | Create a new resource with the specified queue strategy and maximum count.
newResource :: QueueStrategy s q
               => s
               -- ^ the strategy for managing the queuing requests
               -> Int
               -- ^ the maximum count of the resource
               -> Simulation (Resource s q)
newResource s maxCount =
  Simulation $ \r ->
  do countRef <- newIORef maxCount
     waitList <- invokeSimulation r $ newStrategyQueue s
     return Resource { resourceStrategy = s,
                       resourceMaxCount = maxCount,
                       resourceCountRef = countRef,
                       resourceWaitList = waitList }

-- | Create a new resource with the specified queue strategy, maximum and initial count.
newResourceWithCount :: QueueStrategy s q
                        => s
                        -- ^ the strategy for managing the queuing requests
                        -> Int
                        -- ^ the maximum count of the resource
                        -> Int
                        -- ^ the initial count of the resource
                        -> Simulation (Resource s q)
newResourceWithCount s maxCount count = do
  when (count < 0) $
    error $
    "The resource count cannot be negative: " ++
    "newResourceWithCount."
  when (count > maxCount) $
    error $
    "The resource count cannot be greater than " ++
    "its maximum value: newResourceWithCount."
  Simulation $ \r ->
    do countRef <- newIORef count
       waitList <- invokeSimulation r $ newStrategyQueue s
       return Resource { resourceStrategy = s,
                         resourceMaxCount = maxCount,
                         resourceCountRef = countRef,
                         resourceWaitList = waitList }

-- | Return the current count of the resource.
resourceCount :: Resource s q -> Event Int
resourceCount r =
  Event $ \p -> readIORef (resourceCountRef r)

-- | Request for the resource decreasing its count in case of success,
-- otherwise suspending the discontinuous process until some other 
-- process releases the resource.
requestResource :: EnqueueStrategy s q
                   => Resource s q
                   -- ^ the requested resource
                   -> Process ()
requestResource r =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do a <- readIORef (resourceCountRef r)
     if a == 0 
       then invokeEvent p $
            strategyEnqueue (resourceStrategy r) (resourceWaitList r) c
       else do let a' = a - 1
               a' `seq` writeIORef (resourceCountRef r) a'
               invokeEvent p $ resumeCont c ()

-- | Request with the priority for the resource decreasing its count
-- in case of success, otherwise suspending the discontinuous process
-- until some other process releases the resource.
requestResourceWithPriority :: PriorityQueueStrategy s q p
                               => Resource s q
                               -- ^ the requested resource
                               -> p
                               -- ^ the priority
                               -> Process ()
requestResourceWithPriority r priority =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do a <- readIORef (resourceCountRef r)
     if a == 0 
       then invokeEvent p $
            strategyEnqueueWithPriority (resourceStrategy r) (resourceWaitList r) priority c
       else do let a' = a - 1
               a' `seq` writeIORef (resourceCountRef r) a'
               invokeEvent p $ resumeCont c ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended processes as possible.
releaseResource :: DequeueStrategy s q
                   => Resource s q
                   -- ^ the resource to release
                   -> Process ()
releaseResource r = 
  Process $ \_ ->
  Cont $ \c ->
  Event $ \p ->
  do invokeEvent p $ releaseResourceWithinEvent r
     invokeEvent p $ resumeCont c ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended processes as possible.
releaseResourceWithinEvent :: DequeueStrategy s q
                              => Resource s q
                              -- ^ the resource to release
                              -> Event ()
releaseResourceWithinEvent r =
  Event $ \p ->
  do a <- readIORef (resourceCountRef r)
     let a' = a + 1
     when (a' > resourceMaxCount r) $
       error $
       "The resource count cannot be greater than " ++
       "its maximum value: releaseResourceWithinEvent."
     f <- invokeEvent p $
          strategyQueueNull (resourceStrategy r) (resourceWaitList r)
     if f 
       then a' `seq` writeIORef (resourceCountRef r) a'
       else do c <- invokeEvent p $
                    strategyDequeue (resourceStrategy r) (resourceWaitList r)
               invokeEvent p $ enqueueEvent (pointTime p) $
                 Event $ \p ->
                 do z <- contCanceled c
                    if z
                      then do invokeEvent p $ releaseResourceWithinEvent r
                              invokeEvent p $ resumeCont c ()
                      else invokeEvent p $ resumeCont c ()

-- | Try to request for the resource decreasing its count in case of success
-- and returning 'True' in the 'Event' monad; otherwise, returning 'False'.
tryRequestResourceWithinEvent :: Resource s q
                                 -- ^ the resource which we try to request for
                                 -> Event Bool
tryRequestResourceWithinEvent r =
  Event $ \p ->
  do a <- readIORef (resourceCountRef r)
     if a == 0 
       then return False
       else do let a' = a - 1
               a' `seq` writeIORef (resourceCountRef r) a'
               return True
               
-- | Acquire the resource, perform some action and safely release the resource               
-- in the end, even if the 'IOException' was raised within the action. 
-- The process identifier must be created with support of exception 
-- handling, i.e. with help of function 'newProcessIdWithCatch'. Unfortunately,
-- such processes are slower than those that are created with help of
-- other function 'newProcessId'.
usingResource :: EnqueueStrategy s q
                 => Resource s q
                 -- ^ the resource we are going to request for and then release in the end
                 -> Process a
                 -- ^ the action we are going to apply having the resource
                 -> Process a
                 -- ^ the result of the action
usingResource r m =
  do requestResource r
     finallyProcess m $ releaseResource r

-- | Acquire the resource with the specified priority, perform some action and
-- safely release the resource in the end, even if the 'IOException' was raised
-- within the action. The process identifier must be created with support of exception 
-- handling, i.e. with help of function 'newProcessIdWithCatch'. Unfortunately,
-- such processes are slower than those that are created with help of
-- other function 'newProcessId'.
usingResourceWithPriority :: PriorityQueueStrategy s q p
                             => Resource s q
                             -- ^ the resource we are going to request for and then
                             -- release in the end
                             -> p
                             -- ^ the priority
                             -> Process a
                             -- ^ the action we are going to apply having the resource
                             -> Process a
                             -- ^ the result of the action
usingResourceWithPriority r priority m =
  do requestResourceWithPriority r priority
     finallyProcess m $ releaseResource r
