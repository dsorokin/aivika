
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Resource
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the resource which can be acquired and 
-- then released by the discontinuous process 'Process'.
-- The resource can be either limited by the upper bound
-- (run-time check), or it can have no upper bound. The latter
-- is useful for modeling the infinite queue, for example.
--
module Simulation.Aivika.Trans.Resource
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
        usingResourceWithPriority) where

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Internal.Process
import Simulation.Aivika.Trans.QueueStrategy

import qualified Simulation.Aivika.Trans.DoubleLinkedList as DLL 
import qualified Simulation.Aivika.Trans.Vector as V
import qualified Simulation.Aivika.Trans.PriorityQueue as PQ

-- | The ordinary FCFS (First Come - First Serviced) resource.
type FCFSResource m = Resource m FCFS

-- | The ordinary LCFS (Last Come - First Serviced) resource.
type LCFSResource m = Resource m LCFS

-- | The SIRO (Serviced in Random Order) resource.
type SIROResource m = Resource m SIRO

-- | The resource with static priorities.
type PriorityResource m = Resource m StaticPriorities

-- | Represents the resource with strategy @s@ applied for queuing the requests.
data Resource m s = 
  Resource { resourceStrategy :: s,
             -- ^ Return the strategy applied for queuing the requests.
             resourceMaxCount :: Maybe Int,
             -- ^ Return the maximum count of the resource, where 'Nothing'
             -- means that the resource has no upper bound.
             resourceCountRef :: ProtoRef m Int, 
             resourceWaitList :: StrategyQueue m s (Event m (Maybe (ContParams m ()))) }

-- | Create a new FCFS resource with the specified initial count which value becomes
-- the upper bound as well.
newFCFSResource :: Comp m
                   => Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Simulation m (FCFSResource m)
{-# INLINABLE newFCFSResource #-}
{-# SPECIALISE newFCFSResource :: Int -> Simulation IO (FCFSResource IO) #-}
newFCFSResource = newResource FCFS

-- | Create a new FCFS resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newFCFSResourceWithMaxCount :: Comp m
                               => Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Simulation m (FCFSResource m)
{-# INLINABLE newFCFSResourceWithMaxCount #-}
{-# SPECIALISE newFCFSResourceWithMaxCount :: Int -> Maybe Int -> Simulation IO (FCFSResource IO) #-}
newFCFSResourceWithMaxCount = newResourceWithMaxCount FCFS

-- | Create a new LCFS resource with the specified initial count which value becomes
-- the upper bound as well.
newLCFSResource :: Comp m
                   => Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Simulation m (LCFSResource m)
{-# INLINABLE newLCFSResource #-}
{-# SPECIALISE newLCFSResource :: Int -> Simulation IO (LCFSResource IO) #-}
newLCFSResource = newResource LCFS

-- | Create a new LCFS resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newLCFSResourceWithMaxCount :: Comp m
                               => Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Simulation m (LCFSResource m)
{-# INLINABLE newLCFSResourceWithMaxCount #-}
{-# SPECIALISE newLCFSResourceWithMaxCount :: Int -> Maybe Int -> Simulation IO (LCFSResource IO) #-}
newLCFSResourceWithMaxCount = newResourceWithMaxCount LCFS

-- | Create a new SIRO resource with the specified initial count which value becomes
-- the upper bound as well.
newSIROResource :: Comp m
                   => Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Simulation m (SIROResource m)
{-# INLINABLE newSIROResource #-}
{-# SPECIALISE newSIROResource :: Int -> Simulation IO (SIROResource IO) #-}
newSIROResource = newResource SIRO

-- | Create a new SIRO resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newSIROResourceWithMaxCount :: Comp m
                               => Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Simulation m (SIROResource m)
{-# INLINABLE newSIROResourceWithMaxCount #-}
{-# SPECIALISE newSIROResourceWithMaxCount :: Int -> Maybe Int -> Simulation IO (SIROResource IO) #-}
newSIROResourceWithMaxCount = newResourceWithMaxCount SIRO

-- | Create a new priority resource with the specified initial count which value becomes
-- the upper bound as well.
newPriorityResource :: Comp m
                       => Int
                       -- ^ the initial count (and maximal count too) of the resource
                       -> Simulation m (PriorityResource m)
{-# INLINABLE newPriorityResource #-}
{-# SPECIALISE newPriorityResource :: Int -> Simulation IO (PriorityResource IO) #-}
newPriorityResource = newResource StaticPriorities

-- | Create a new priority resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newPriorityResourceWithMaxCount :: Comp m
                                   => Int
                                   -- ^ the initial count of the resource
                                   -> Maybe Int
                                   -- ^ the maximum count of the resource, which can be indefinite
                                   -> Simulation m (PriorityResource m)
{-# INLINABLE newPriorityResourceWithMaxCount #-}
{-# SPECIALISE newPriorityResourceWithMaxCount :: Int -> Maybe Int -> Simulation IO (PriorityResource IO) #-}
newPriorityResourceWithMaxCount = newResourceWithMaxCount StaticPriorities

-- | Create a new resource with the specified queue strategy and initial count.
-- The last value becomes the upper bound as well.
newResource :: (Comp m, QueueStrategy m s)
               => s
               -- ^ the strategy for managing the queuing requests
               -> Int
               -- ^ the initial count (and maximal count too) of the resource
               -> Simulation m (Resource m s)
{-# INLINABLE newResource #-}
{-# SPECIALISE newResource :: QueueStrategy IO s => s -> Int -> Simulation IO (Resource IO s) #-}
newResource s count =
  Simulation $ \r ->
  do when (count < 0) $
       error $
       "The resource count cannot be negative: " ++
       "newResource."
     let session = runSession r 
     countRef <- newProtoRef session count
     waitList <- invokeSimulation r $ newStrategyQueue s
     return Resource { resourceStrategy = s,
                       resourceMaxCount = Just count,
                       resourceCountRef = countRef,
                       resourceWaitList = waitList }

-- | Create a new resource with the specified queue strategy, initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newResourceWithMaxCount :: (Comp m, QueueStrategy m s)
                           => s
                           -- ^ the strategy for managing the queuing requests
                           -> Int
                           -- ^ the initial count of the resource
                           -> Maybe Int
                           -- ^ the maximum count of the resource, which can be indefinite
                           -> Simulation m (Resource m s)
{-# INLINABLE newResourceWithMaxCount #-}
{-# SPECIALISE newResourceWithMaxCount :: QueueStrategy IO s => s -> Int -> Maybe Int -> Simulation IO (Resource IO s) #-}
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
     let session = runSession r
     countRef <- newProtoRef session count
     waitList <- invokeSimulation r $ newStrategyQueue s
     return Resource { resourceStrategy = s,
                       resourceMaxCount = maxCount,
                       resourceCountRef = countRef,
                       resourceWaitList = waitList }

-- | Return the current count of the resource.
resourceCount :: Comp m => Resource m s -> Event m Int
{-# INLINABLE resourceCount #-}
{-# SPECIALISE resourceCount :: Resource IO s -> Event IO Int #-}
resourceCount r =
  Event $ \p -> readProtoRef (resourceCountRef r)

-- | Request for the resource decreasing its count in case of success,
-- otherwise suspending the discontinuous process until some other 
-- process releases the resource.
requestResource :: (Comp m, EnqueueStrategy m s)
                   => Resource m s 
                   -- ^ the requested resource
                   -> Process m ()
{-# INLINABLE requestResource #-}
{-# SPECIALISE requestResource :: EnqueueStrategy IO s => Resource IO s -> Process IO () #-}
requestResource r =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do a <- readProtoRef (resourceCountRef r)
     if a == 0 
       then do c <- invokeEvent p $ contFreeze c
               invokeEvent p $
                 strategyEnqueue (resourceWaitList r) c
       else do let a' = a - 1
               a' `seq` writeProtoRef (resourceCountRef r) a'
               invokeEvent p $ resumeCont c ()

-- | Request with the priority for the resource decreasing its count
-- in case of success, otherwise suspending the discontinuous process
-- until some other process releases the resource.
requestResourceWithPriority :: (Comp m, PriorityQueueStrategy m s p)
                               => Resource m s
                               -- ^ the requested resource
                               -> p
                               -- ^ the priority
                               -> Process m ()
{-# INLINABLE requestResourceWithPriority #-}
{-# SPECIALISE requestResourceWithPriority :: PriorityQueueStrategy IO s p => Resource IO s -> p -> Process IO () #-}
requestResourceWithPriority r priority =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do a <- readProtoRef (resourceCountRef r)
     if a == 0 
       then do c <- invokeEvent p $ contFreeze c
               invokeEvent p $
                 strategyEnqueueWithPriority (resourceWaitList r) priority c
       else do let a' = a - 1
               a' `seq` writeProtoRef (resourceCountRef r) a'
               invokeEvent p $ resumeCont c ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended processes as possible.
releaseResource :: (Comp m, DequeueStrategy m s)
                   => Resource m s
                   -- ^ the resource to release
                   -> Process m ()
{-# INLINABLE releaseResource #-}
{-# SPECIALISE releaseResource :: DequeueStrategy IO s => Resource IO s -> Process IO () #-}
releaseResource r = 
  Process $ \_ ->
  Cont $ \c ->
  Event $ \p ->
  do invokeEvent p $ releaseResourceWithinEvent r
     invokeEvent p $ resumeCont c ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended processes as possible.
releaseResourceWithinEvent :: (Comp m, DequeueStrategy m s)
                              => Resource m s
                              -- ^ the resource to release
                              -> Event m ()
{-# INLINABLE releaseResourceWithinEvent #-}
{-# SPECIALISE releaseResourceWithinEvent :: DequeueStrategy IO s => Resource IO s -> Event IO () #-}
releaseResourceWithinEvent r =
  Event $ \p ->
  do a <- readProtoRef (resourceCountRef r)
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
       then a' `seq` writeProtoRef (resourceCountRef r) a'
       else do c <- invokeEvent p $
                    strategyDequeue (resourceWaitList r)
               c <- invokeEvent p c
               case c of
                 Nothing ->
                   invokeEvent p $ releaseResourceWithinEvent r
                 Just c  ->
                   invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()

-- | Try to request for the resource decreasing its count in case of success
-- and returning 'True' in the 'Event' monad; otherwise, returning 'False'.
tryRequestResourceWithinEvent :: Comp m
                                 => Resource m s
                                 -- ^ the resource which we try to request for
                                 -> Event m Bool
{-# INLINABLE tryRequestResourceWithinEvent #-}
{-# SPECIALISE tryRequestResourceWithinEvent :: Resource IO s -> Event IO Bool #-}
tryRequestResourceWithinEvent r =
  Event $ \p ->
  do a <- readProtoRef (resourceCountRef r)
     if a == 0 
       then return False
       else do let a' = a - 1
               a' `seq` writeProtoRef (resourceCountRef r) a'
               return True
               
-- | Acquire the resource, perform some action and safely release the resource               
-- in the end, even if the 'IOException' was raised within the action. 
usingResource :: (Comp m, EnqueueStrategy m s)
                 => Resource m s
                 -- ^ the resource we are going to request for and then release in the end
                 -> Process m a
                 -- ^ the action we are going to apply having the resource
                 -> Process m a
                 -- ^ the result of the action
{-# INLINABLE usingResource #-}
{-# SPECIALISE usingResource :: EnqueueStrategy IO s => Resource IO s -> Process IO a -> Process IO a #-}
usingResource r m =
  do requestResource r
     finallyProcess m $ releaseResource r

-- | Acquire the resource with the specified priority, perform some action and
-- safely release the resource in the end, even if the 'IOException' was raised
-- within the action.
usingResourceWithPriority :: (Comp m, PriorityQueueStrategy m s p)
                             => Resource m s
                             -- ^ the resource we are going to request for and then
                             -- release in the end
                             -> p
                             -- ^ the priority
                             -> Process m a
                             -- ^ the action we are going to apply having the resource
                             -> Process m a
                             -- ^ the result of the action
{-# INLINABLE usingResourceWithPriority #-}
{-# SPECIALISE usingResourceWithPriority :: PriorityQueueStrategy IO s p => Resource IO s -> p -> Process IO a -> Process IO a #-}
usingResourceWithPriority r priority m =
  do requestResourceWithPriority r priority
     finallyProcess m $ releaseResource r
