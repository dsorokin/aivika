
-- |
-- Module     : Simulation.Aivika.Resource
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines the resource which can be acquired and 
-- then released by the discontinuous process 'Process'.
-- The resource can be either limited by the upper bound
-- (run-time check), or it can have no upper bound. The latter
-- is useful for modeling the infinite queue, for example.
--
module Simulation.Aivika.Resource
       (-- * Resource Types
        FCFSResource,
        LCFSResource,
        SIROResource,
        PriorityResource,
        Resource,
        -- * Creating Resource
        newFCFSResource,
        newFCFSResourceWithMaxCount,
        newLCFSResource,
        newLCFSResourceWithMaxCount,
        newSIROResource,
        newSIROResourceWithMaxCount,
        newPriorityResource,
        newPriorityResourceWithMaxCount,
        newResource,
        newResourceWithMaxCount,
        -- * Resource Properties
        resourceStrategy,
        resourceMaxCount,
        resourceCount,
        -- * Requesting for and Releasing Resource
        requestResource,
        requestResourceWithPriority,
        tryRequestResourceWithinEvent,
        releaseResource,
        releaseResourceWithinEvent,
        usingResource,
        usingResourceWithPriority,
        -- * Altering Resource
        incResourceCount,
        decResourceCount) where

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

import qualified Simulation.Aivika.DoubleLinkedList as DLL 
import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.PriorityQueue as PQ

-- | The ordinary FCFS (First Come - First Serviced) resource.
type FCFSResource = Resource FCFS

-- | The ordinary LCFS (Last Come - First Serviced) resource.
type LCFSResource = Resource LCFS

-- | The SIRO (Serviced in Random Order) resource.
type SIROResource = Resource SIRO

-- | The resource with static priorities.
type PriorityResource = Resource StaticPriorities

-- | Represents the resource with strategy @s@ applied for queuing the requests.
data Resource s = 
  Resource { resourceStrategy :: s,
             -- ^ Return the strategy applied for queuing the requests.
             resourceMaxCount :: Maybe Int,
             -- ^ Return the maximum count of the resource, where 'Nothing'
             -- means that the resource has no upper bound.
             resourceCountRef :: IORef Int, 
             resourceWaitList :: StrategyQueue s (FrozenCont ()) }

instance Eq (Resource s) where
  x == y = resourceCountRef x == resourceCountRef y  -- unique references

-- | Create a new FCFS resource with the specified initial count which value becomes
-- the upper bound as well.
newFCFSResource :: Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Simulation FCFSResource
newFCFSResource = newResource FCFS

-- | Create a new FCFS resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newFCFSResourceWithMaxCount :: Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Simulation FCFSResource
newFCFSResourceWithMaxCount = newResourceWithMaxCount FCFS

-- | Create a new LCFS resource with the specified initial count which value becomes
-- the upper bound as well.
newLCFSResource :: Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Simulation LCFSResource
newLCFSResource = newResource LCFS

-- | Create a new LCFS resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newLCFSResourceWithMaxCount :: Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Simulation LCFSResource
newLCFSResourceWithMaxCount = newResourceWithMaxCount LCFS

-- | Create a new SIRO resource with the specified initial count which value becomes
-- the upper bound as well.
newSIROResource :: Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Simulation SIROResource
newSIROResource = newResource SIRO

-- | Create a new SIRO resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newSIROResourceWithMaxCount :: Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Simulation SIROResource
newSIROResourceWithMaxCount = newResourceWithMaxCount SIRO

-- | Create a new priority resource with the specified initial count which value becomes
-- the upper bound as well.
newPriorityResource :: Int
                       -- ^ the initial count (and maximal count too) of the resource
                       -> Simulation PriorityResource
newPriorityResource = newResource StaticPriorities

-- | Create a new priority resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newPriorityResourceWithMaxCount :: Int
                                   -- ^ the initial count of the resource
                                   -> Maybe Int
                                   -- ^ the maximum count of the resource, which can be indefinite
                                   -> Simulation PriorityResource
newPriorityResourceWithMaxCount = newResourceWithMaxCount StaticPriorities

-- | Create a new resource with the specified queue strategy and initial count.
-- The last value becomes the upper bound as well.
newResource :: QueueStrategy s
               => s
               -- ^ the strategy for managing the queuing requests
               -> Int
               -- ^ the initial count (and maximal count too) of the resource
               -> Simulation (Resource s)
newResource s count =
  Simulation $ \r ->
  do when (count < 0) $
       error $
       "The resource count cannot be negative: " ++
       "newResource."
     countRef <- newIORef count
     waitList <- invokeSimulation r $ newStrategyQueue s
     return Resource { resourceStrategy = s,
                       resourceMaxCount = Just count,
                       resourceCountRef = countRef,
                       resourceWaitList = waitList }

-- | Create a new resource with the specified queue strategy, initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newResourceWithMaxCount :: QueueStrategy s
                           => s
                           -- ^ the strategy for managing the queuing requests
                           -> Int
                           -- ^ the initial count of the resource
                           -> Maybe Int
                           -- ^ the maximum count of the resource, which can be indefinite
                           -> Simulation (Resource s)
newResourceWithMaxCount s count maxCount =
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
     waitList <- invokeSimulation r $ newStrategyQueue s
     return Resource { resourceStrategy = s,
                       resourceMaxCount = maxCount,
                       resourceCountRef = countRef,
                       resourceWaitList = waitList }

-- | Return the current count of the resource.
resourceCount :: Resource s -> Event Int
resourceCount r =
  Event $ \p -> readIORef (resourceCountRef r)

-- | Request for the resource decreasing its count in case of success,
-- otherwise suspending the discontinuous process until some other 
-- process releases the resource.
requestResource :: EnqueueStrategy s
                   => Resource s
                   -- ^ the requested resource
                   -> Process ()
requestResource r =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do a <- readIORef (resourceCountRef r)
     if a == 0 
       then do c <- invokeEvent p $
                    freezeContReentering c () $
                    invokeCont c $
                    invokeProcess pid $
                    requestResource r
               invokeEvent p $
                 strategyEnqueue (resourceWaitList r) c
       else do let a' = a - 1
               a' `seq` writeIORef (resourceCountRef r) a'
               invokeEvent p $ resumeCont c ()

-- | Request with the priority for the resource decreasing its count
-- in case of success, otherwise suspending the discontinuous process
-- until some other process releases the resource.
requestResourceWithPriority :: PriorityQueueStrategy s p
                               => Resource s
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
       then do c <- invokeEvent p $
                    freezeContReentering c () $
                    invokeCont c $
                    invokeProcess pid $
                    requestResourceWithPriority r priority
               invokeEvent p $
                 strategyEnqueueWithPriority (resourceWaitList r) priority c
       else do let a' = a - 1
               a' `seq` writeIORef (resourceCountRef r) a'
               invokeEvent p $ resumeCont c ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended processes as possible.
releaseResource :: DequeueStrategy s
                   => Resource s
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
releaseResourceWithinEvent :: DequeueStrategy s
                              => Resource s
                              -- ^ the resource to release
                              -> Event ()
releaseResourceWithinEvent r =
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
     f <- invokeEvent p $
          strategyQueueNull (resourceWaitList r)
     if f 
       then a' `seq` writeIORef (resourceCountRef r) a'
       else do c <- invokeEvent p $
                    strategyDequeue (resourceWaitList r)
               c <- invokeEvent p $ unfreezeCont c
               case c of
                 Nothing ->
                   invokeEvent p $ releaseResourceWithinEvent r
                 Just c  ->
                   invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()

-- | Try to request for the resource decreasing its count in case of success
-- and returning 'True' in the 'Event' monad; otherwise, returning 'False'.
tryRequestResourceWithinEvent :: Resource s
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
usingResource :: EnqueueStrategy s
                 => Resource s
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
-- within the action.
usingResourceWithPriority :: PriorityQueueStrategy s p
                             => Resource s
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

-- | Increase the count of available resource by the specified number,
-- invoking the awaiting processes as needed.
incResourceCount :: DequeueStrategy s
                    => Resource s
                    -- ^ the resource
                    -> Int
                    -- ^ the increment for the resource count
                    -> Event ()
incResourceCount r n
  | n < 0     = error "The increment cannot be negative: incResourceCount"
  | n == 0    = return ()
  | otherwise =
    do releaseResourceWithinEvent r
       incResourceCount r (n - 1)

-- | Decrease the count of available resource by the specified number,
-- waiting for the processes capturing the resource as needed.
decResourceCount :: EnqueueStrategy s
                    => Resource s
                    -- ^ the resource
                    -> Int
                    -- ^ the decrement for the resource count
                    -> Process ()
decResourceCount r n
  | n < 0     = error "The decrement cannot be negative: decResourceCount"
  | n == 0    = return ()
  | otherwise =
    do requestResource r
       decResourceCount r (n - 1)

