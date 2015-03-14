
-- |
-- Module     : Simulation.Aivika.Resource.Preemption
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the preemptible resource.
--
module Simulation.Aivika.Resource.Preemption
       (-- * Resource Type
        Resource,
        -- * Creating Resource
        newResource,
        newResourceWithMaxCount,
        -- * Resource Properties
        resourceMaxCount,
        resourceCount,
        -- * Requesting for and Releasing Resource
        requestResourceWithPriority,
        releaseResource,
        usingResourceWithPriority,
        -- * Altering Resource 
        incResourceCount,
        decResourceCount,
        alterResourceCount) where

import Data.IORef

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Cont
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.QueueStrategy

import qualified Simulation.Aivika.PriorityQueue as PQ

-- | Represents a preemptible resource.
data Resource = 
  Resource { resourceMaxCount :: Maybe Int,
             -- ^ Return the maximum count of the resource, where 'Nothing'
             -- means that the resource has no upper bound.
             resourceCountRef :: IORef Int,
             resourceActingQueue :: PQ.PriorityQueue ResourceActingItem,
             resourceWaitQueue :: PQ.PriorityQueue ResourceAwaitingItem }

-- | Identifies an acting item that acquired the resource.
data ResourceActingItem =
  ResourceActingItem { actingItemPriority :: Double,
                       actingItemId :: ProcessId }

-- | Idenitifies an awaiting item that waits for releasing of the resource to take it.
type ResourceAwaitingItem = Either ResourceRequestingItem ResourcePreemptedItem

-- | Idenitifies an item that requests for the resource.
data ResourceRequestingItem =
  ResourceRequestingItem { requestingItemPriority :: Double,
                           requestingItemId :: ProcessId,
                           requestedItemCont :: FrozenCont () }

-- | Idenitifies an item that was preempted.
data ResourcePreemptedItem =
  ResourcePreemptedItem { preemptedItemPriority :: Double,
                          preemptedItemId :: ProcessId }

instance Eq Resource where
  x == y = resourceCountRef x == resourceCountRef y  -- unique references

instance Eq ResourceActingItem where
  x == y = actingItemId x == actingItemId y

-- | Create a new resource with the specified initial count that becomes the upper bound as well.
newResource :: Int
               -- ^ the initial count (and maximal count too) of the resource
               -> Simulation Resource
newResource count =
  Simulation $ \r ->
  do when (count < 0) $
       error $
       "The resource count cannot be negative: " ++
       "newResource."
     countRef <- newIORef count
     actingQueue <- PQ.newQueue
     waitQueue <- PQ.newQueue
     return Resource { resourceMaxCount = Just count,
                       resourceCountRef = countRef,
                       resourceActingQueue = actingQueue,
                       resourceWaitQueue = waitQueue }

-- | Create a new resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newResourceWithMaxCount :: Int
                           -- ^ the initial count of the resource
                           -> Maybe Int
                           -- ^ the maximum count of the resource, which can be indefinite
                           -> Simulation Resource
newResourceWithMaxCount count maxCount =
  Simulation $ \r ->
  do when (count < 0) $
       error $
       "The resource count cannot be negative: " ++
       "newResourceWithMaxCount."
     case maxCount of
       Just maxCount | count > maxCount ->
         error $
         "The resource count cannot be greater than " ++
         "its maximum value: newResourceWithMaxCount."
       _ ->
         return ()
     countRef <- newIORef count
     actingQueue <- PQ.newQueue
     waitQueue <- PQ.newQueue
     return Resource { resourceMaxCount = maxCount,
                       resourceCountRef = countRef,
                       resourceActingQueue = actingQueue,
                       resourceWaitQueue = waitQueue }

-- | Return the current count of the resource.
resourceCount :: Resource -> Event Int
resourceCount r =
  Event $ \p -> readIORef (resourceCountRef r)

-- | Request with the priority for the resource decreasing its count
-- in case of success, otherwise suspending the discontinuous process
-- until some other process releases the resource.
--
-- It may preempt another process if the latter aquired the resource before
-- but had a lower priority. Then the current process takes an ownership of
-- the resource.
requestResourceWithPriority :: Resource
                               -- ^ the requested resource
                               -> Double
                               -- ^ the priority (the less value has a higher priority)
                               -> Process ()
requestResourceWithPriority r priority =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do a <- readIORef (resourceCountRef r)
     if a == 0
       then do f <- PQ.queueNull (resourceActingQueue r)
               if f
                 then do c <- invokeEvent p $
                              freezeContReentering c () $
                              invokeCont c $
                              invokeProcess pid $
                              requestResourceWithPriority r priority
                         PQ.enqueue (resourceWaitQueue r) priority (Left $ ResourceRequestingItem priority pid c)
                 else do (p0', item0) <- PQ.queueFront (resourceActingQueue r)
                         let p0 = - p0'
                             pid0 = actingItemId item0
                         if priority < p0
                           then do PQ.dequeue (resourceActingQueue r)
                                   PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                                   PQ.enqueue (resourceWaitQueue r) p0 (Right $ ResourcePreemptedItem p0 pid0)
                                   invokeEvent p $ preemptProcess pid0
                                   invokeEvent p $ resumeCont c ()
                           else do c <- invokeEvent p $
                                        freezeContReentering c () $
                                        invokeCont c $
                                        invokeProcess pid $
                                        requestResourceWithPriority r priority
                                   PQ.enqueue (resourceWaitQueue r) priority (Left $ ResourceRequestingItem priority pid c)
       else do let a' = a - 1
               a' `seq` writeIORef (resourceCountRef r) a'
               PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
               invokeEvent p $ resumeCont c ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended or preempted processes as possible.
releaseResource :: Resource
                   -- ^ the resource to release
                   -> Process ()
releaseResource r = 
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do f <- PQ.removeBy (resourceActingQueue r) (\item -> actingItemId item == pid)
     if f
       then do invokeEvent p $ releaseResource' r
               invokeEvent p $ resumeCont c ()
       else error $
            "The resource was not acquired by this process: releaseResource"

-- | Release the resource increasing its count and resuming one of the
-- previously suspended or preempted processes as possible.
releaseResource' :: Resource
                    -- ^ the resource to release
                    -> Event ()
releaseResource' r =
  Event $ \p ->
  do a <- readIORef (resourceCountRef r)
     let a' = a + 1
     case resourceMaxCount r of
       Just maxCount | a' > maxCount ->
         error $
         "The resource count cannot be greater than " ++
         "its maximum value: releaseResourceWithinEvent."
       _ ->
         return ()
     f <- PQ.queueNull (resourceWaitQueue r)
     if f 
       then a' `seq` writeIORef (resourceCountRef r) a'
       else do (priority', item) <- PQ.queueFront (resourceWaitQueue r)
               PQ.dequeue (resourceWaitQueue r)
               case item of
                 Left (ResourceRequestingItem priority pid c) ->
                   do c <- invokeEvent p $ unfreezeCont c
                      case c of
                        Nothing ->
                          invokeEvent p $ releaseResource' r
                        Just c ->
                          do PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                             invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()
                 Right (ResourcePreemptedItem priority pid) ->
                   do f <- invokeEvent p $ processCancelled pid
                      case f of
                        True ->
                          invokeEvent p $ releaseResource' r
                        False ->
                          do PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                             invokeEvent p $ reenterProcess pid
               
-- | Acquire the resource with the specified priority, perform some action and
-- safely release the resource in the end, even if the 'IOException' was raised
-- within the action.
usingResourceWithPriority :: Resource
                             -- ^ the resource we are going to request for and then
                             -- release in the end
                             -> Double
                             -- ^ the priority (the less value has a higher priority)
                             -> Process a
                             -- ^ the action we are going to apply having the resource
                             -> Process a
                             -- ^ the result of the action
usingResourceWithPriority r priority m =
  do requestResourceWithPriority r priority
     finallyProcess m $ releaseResource r

-- | Preempt a process with the lowest priority that acquires yet the resource
-- and decrease the count of available resource by 1. 
decResourceCount' :: Resource -> Event ()
decResourceCount' r =
  Event $ \p ->
  do a <- readIORef (resourceCountRef r)
     when (a == 0) $
       error $
       "The resource exceeded and its count is zero: decResourceCount'"
     f <- PQ.queueNull (resourceActingQueue r)
     when f $
       error $
       "The resource acting queue is null: decResourceCount'"
     (p0', item0) <- PQ.queueFront (resourceActingQueue r)
     let p0 = - p0'
         pid0 = actingItemId item0
     PQ.dequeue (resourceActingQueue r)
     PQ.enqueue (resourceWaitQueue r) p0 (Right $ ResourcePreemptedItem p0 pid0)
     invokeEvent p $ preemptProcess pid0
     let a' = a - 1
     a' `seq` writeIORef (resourceCountRef r) a'

-- | Increase the count of available resource by the specified number,
-- invoking the awaiting and preempted processes according to their priorities
-- as needed.
incResourceCount :: Resource
                    -- ^ the resource
                    -> Int
                    -- ^ the increment for the resource count
                    -> Event ()
incResourceCount r n
  | n < 0     = error "The increment cannot be negative: incResourceCount"
  | n == 0    = return ()
  | otherwise =
    do releaseResource' r
       incResourceCount r (n - 1)

-- | Decrease the count of available resource by the specified number,
-- preempting the processes according to their priorities as needed.
decResourceCount :: Resource
                    -- ^ the resource
                    -> Int
                    -- ^ the decrement for the resource count
                    -> Event ()
decResourceCount r n
  | n < 0     = error "The decrement cannot be negative: decResourceCount"
  | n == 0    = return ()
  | otherwise =
    do decResourceCount' r
       decResourceCount r (n - 1)

-- | Alter the resource count either increasing or decreasing it by calling
-- 'incResourceCount' or 'decResourceCount' respectively. 
alterResourceCount :: Resource
                      -- ^ the resource
                      -> Int
                      -- ^ a change of the resource count
                      -> Event ()
alterResourceCount r n
  | n < 0  = decResourceCount r (- n)
  | n > 0  = incResourceCount r n
  | n == 0 = return ()
