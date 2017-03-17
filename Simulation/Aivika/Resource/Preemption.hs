
-- |
-- Module     : Simulation.Aivika.Resource.Preemption
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
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
        resourceCountStats,
        resourceUtilisationCount,
        resourceUtilisationCountStats,
        resourceQueueCount,
        resourceQueueCountStats,
        resourceTotalWaitTime,
        resourceWaitTime,
        -- * Requesting for and Releasing Resource
        requestResourceWithPriority,
        releaseResource,
        usingResourceWithPriority,
        -- * Altering Resource 
        incResourceCount,
        decResourceCount,
        alterResourceCount,
        -- * Signals
        resourceCountChanged,
        resourceCountChanged_,
        resourceUtilisationCountChanged,
        resourceUtilisationCountChanged_,
        resourceQueueCountChanged,
        resourceQueueCountChanged_,
        resourceWaitTimeChanged,
        resourceWaitTimeChanged_,
        resourceChanged_) where

import Data.IORef
import Data.Monoid
import Data.Maybe

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Cont
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.QueueStrategy
import Simulation.Aivika.Statistics
import Simulation.Aivika.Signal

import qualified Simulation.Aivika.PriorityQueue as PQ

-- | Represents a preemptible resource.
data Resource = 
  Resource { resourceMaxCount :: Maybe Int,
             -- ^ Return the maximum count of the resource, where 'Nothing'
             -- means that the resource has no upper bound.
             resourceCountRef :: IORef Int,
             resourceCountStatsRef :: IORef (TimingStats Int),
             resourceCountSource :: SignalSource Int,
             resourceUtilisationCountRef :: IORef Int,
             resourceUtilisationCountStatsRef :: IORef (TimingStats Int),
             resourceUtilisationCountSource :: SignalSource Int,
             resourceQueueCountRef :: IORef Int,
             resourceQueueCountStatsRef :: IORef (TimingStats Int),
             resourceQueueCountSource :: SignalSource Int,
             resourceTotalWaitTimeRef :: IORef Double,
             resourceWaitTimeRef :: IORef (SamplingStats Double),
             resourceWaitTimeSource :: SignalSource (),
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
                           requestingItemTime :: Double,
                           requestingItemId :: ProcessId,
                           requestingItemCont :: FrozenCont () }

-- | Idenitifies an item that was preempted.
data ResourcePreemptedItem =
  ResourcePreemptedItem { preemptedItemPriority :: Double,
                          preemptedItemTime :: Double,
                          preemptedItemId :: ProcessId }

instance Eq Resource where
  x == y = resourceCountRef x == resourceCountRef y  -- unique references

instance Eq ResourceActingItem where
  x == y = actingItemId x == actingItemId y

-- | Create a new resource with the specified initial count that becomes the upper bound as well.
newResource :: Int
               -- ^ the initial count (and maximal count too) of the resource
               -> Event Resource
newResource count =
  newResourceWithMaxCount count (Just count)

-- | Create a new resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newResourceWithMaxCount :: Int
                           -- ^ the initial count of the resource
                           -> Maybe Int
                           -- ^ the maximum count of the resource, which can be indefinite
                           -> Event Resource
newResourceWithMaxCount count maxCount =
  Event $ \p ->
  do let r = pointRun p
         t = pointTime p
     when (count < 0) $
       throwIO $
       SimulationRetry $
       "The resource count cannot be negative: " ++
       "newResourceWithMaxCount."
     case maxCount of
       Just maxCount | count > maxCount ->
         throwIO $
         SimulationRetry $
         "The resource count cannot be greater than " ++
         "its maximum value: newResourceWithMaxCount."
       _ ->
         return ()
     countRef <- newIORef count
     countStatsRef <- newIORef $ returnTimingStats t count
     countSource <- invokeSimulation r newSignalSource
     utilCountRef <- newIORef 0
     utilCountStatsRef <- newIORef $ returnTimingStats t 0
     utilCountSource <- invokeSimulation r newSignalSource
     queueCountRef <- newIORef 0
     queueCountStatsRef <- newIORef $ returnTimingStats t 0
     queueCountSource <- invokeSimulation r newSignalSource
     totalWaitTimeRef <- newIORef 0
     waitTimeRef <- newIORef emptySamplingStats
     waitTimeSource <- invokeSimulation r newSignalSource
     actingQueue <- PQ.newQueue
     waitQueue <- PQ.newQueue
     return Resource { resourceMaxCount = maxCount,
                       resourceCountRef = countRef,
                       resourceCountStatsRef = countStatsRef,
                       resourceCountSource = countSource,
                       resourceUtilisationCountRef = utilCountRef,
                       resourceUtilisationCountStatsRef = utilCountStatsRef,
                       resourceUtilisationCountSource = utilCountSource,
                       resourceQueueCountRef = queueCountRef,
                       resourceQueueCountStatsRef = queueCountStatsRef,
                       resourceQueueCountSource = queueCountSource,
                       resourceTotalWaitTimeRef = totalWaitTimeRef,
                       resourceWaitTimeRef = waitTimeRef,
                       resourceWaitTimeSource = waitTimeSource,
                       resourceActingQueue = actingQueue,
                       resourceWaitQueue = waitQueue }

-- | Return the current available count of the resource.
resourceCount :: Resource -> Event Int
resourceCount r =
  Event $ \p -> readIORef (resourceCountRef r)

-- | Return the statistics for the available count of the resource.
resourceCountStats :: Resource -> Event (TimingStats Int)
resourceCountStats r =
  Event $ \p -> readIORef (resourceCountStatsRef r)

-- | Signal triggered when the 'resourceCount' property changes.
resourceCountChanged :: Resource -> Signal Int
resourceCountChanged r =
  publishSignal $ resourceCountSource r

-- | Signal triggered when the 'resourceCount' property changes.
resourceCountChanged_ :: Resource -> Signal ()
resourceCountChanged_ r =
  mapSignal (const ()) $ resourceCountChanged r

-- | Return the current utilisation count of the resource.
resourceUtilisationCount :: Resource -> Event Int
resourceUtilisationCount r =
  Event $ \p -> readIORef (resourceUtilisationCountRef r)

-- | Return the statistics for the utilisation count of the resource.
resourceUtilisationCountStats :: Resource -> Event (TimingStats Int)
resourceUtilisationCountStats r =
  Event $ \p -> readIORef (resourceUtilisationCountStatsRef r)

-- | Signal triggered when the 'resourceUtilisationCount' property changes.
resourceUtilisationCountChanged :: Resource -> Signal Int
resourceUtilisationCountChanged r =
  publishSignal $ resourceUtilisationCountSource r

-- | Signal triggered when the 'resourceUtilisationCount' property changes.
resourceUtilisationCountChanged_ :: Resource -> Signal ()
resourceUtilisationCountChanged_ r =
  mapSignal (const ()) $ resourceUtilisationCountChanged r

-- | Return the current queue length of the resource.
resourceQueueCount :: Resource -> Event Int
resourceQueueCount r =
  Event $ \p -> readIORef (resourceQueueCountRef r)

-- | Return the statistics for the queue length of the resource.
resourceQueueCountStats :: Resource -> Event (TimingStats Int)
resourceQueueCountStats r =
  Event $ \p -> readIORef (resourceQueueCountStatsRef r)

-- | Signal triggered when the 'resourceQueueCount' property changes.
resourceQueueCountChanged :: Resource -> Signal Int
resourceQueueCountChanged r =
  publishSignal $ resourceQueueCountSource r

-- | Signal triggered when the 'resourceQueueCount' property changes.
resourceQueueCountChanged_ :: Resource -> Signal ()
resourceQueueCountChanged_ r =
  mapSignal (const ()) $ resourceQueueCountChanged r

-- | Return the total wait time of the resource.
resourceTotalWaitTime :: Resource -> Event Double
resourceTotalWaitTime r =
  Event $ \p -> readIORef (resourceTotalWaitTimeRef r)

-- | Return the statistics for the wait time of the resource.
resourceWaitTime :: Resource -> Event (SamplingStats Double)
resourceWaitTime r =
  Event $ \p -> readIORef (resourceWaitTimeRef r)

-- | Signal triggered when the 'resourceTotalWaitTime' and 'resourceWaitTime' properties change.
resourceWaitTimeChanged :: Resource -> Signal (SamplingStats Double)
resourceWaitTimeChanged r =
  mapSignalM (\() -> resourceWaitTime r) $ resourceWaitTimeChanged_ r

-- | Signal triggered when the 'resourceTotalWaitTime' and 'resourceWaitTime' properties change.
resourceWaitTimeChanged_ :: Resource -> Signal ()
resourceWaitTimeChanged_ r =
  publishSignal $ resourceWaitTimeSource r

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
  do let t = pointTime p
     a <- readIORef (resourceCountRef r)
     if a == 0
       then do f <- PQ.queueNull (resourceActingQueue r)
               if f
                 then do c <- invokeEvent p $
                              freezeContReentering c () $
                              invokeCont c $
                              invokeProcess pid $
                              requestResourceWithPriority r priority
                         PQ.enqueue (resourceWaitQueue r) priority (Left $ ResourceRequestingItem priority t pid c)
                         invokeEvent p $ updateResourceQueueCount r 1
                 else do (p0', item0) <- PQ.queueFront (resourceActingQueue r)
                         let p0 = - p0'
                             pid0 = actingItemId item0
                         if priority < p0
                           then do PQ.dequeue (resourceActingQueue r)
                                   PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                                   PQ.enqueue (resourceWaitQueue r) p0 (Right $ ResourcePreemptedItem p0 t pid0)
                                   invokeEvent p $ updateResourceWaitTime r 0
                                   invokeEvent p $ updateResourceQueueCount r 1
                                   invokeEvent p $ processPreemptionBegin pid0
                                   invokeEvent p $ resumeCont c ()
                           else do c <- invokeEvent p $
                                        freezeContReentering c () $
                                        invokeCont c $
                                        invokeProcess pid $
                                        requestResourceWithPriority r priority
                                   PQ.enqueue (resourceWaitQueue r) priority (Left $ ResourceRequestingItem priority t pid c)
                                   invokeEvent p $ updateResourceQueueCount r 1
       else do PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
               invokeEvent p $ updateResourceWaitTime r 0
               invokeEvent p $ updateResourceCount r (-1)
               invokeEvent p $ updateResourceUtilisationCount r 1
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
  do f <- fmap isJust $ PQ.queueDeleteBy (resourceActingQueue r) (\item -> actingItemId item == pid)
     if f
       then do invokeEvent p $ updateResourceUtilisationCount r (-1)
               invokeEvent p $ releaseResource' r
               invokeEvent p $ resumeCont c ()
       else throwIO $
            SimulationRetry
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
         throwIO $
         SimulationRetry $
         "The resource count cannot be greater than " ++
         "its maximum value: releaseResource'."
       _ ->
         return ()
     f <- PQ.queueNull (resourceWaitQueue r)
     if f 
       then invokeEvent p $ updateResourceCount r 1
       else do (priority', item) <- PQ.queueFront (resourceWaitQueue r)
               PQ.dequeue (resourceWaitQueue r)
               invokeEvent p $ updateResourceQueueCount r (-1)
               case item of
                 Left (ResourceRequestingItem priority t pid c) ->
                   do c <- invokeEvent p $ unfreezeCont c
                      case c of
                        Nothing ->
                          invokeEvent p $ releaseResource' r
                        Just c ->
                          do PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                             invokeEvent p $ updateResourceWaitTime r (pointTime p - t)
                             invokeEvent p $ updateResourceUtilisationCount r 1
                             invokeEvent p $ enqueueEvent (pointTime p) $ reenterCont c ()
                 Right (ResourcePreemptedItem priority t pid) ->
                   do f <- invokeEvent p $ processCancelled pid
                      case f of
                        True ->
                          invokeEvent p $ releaseResource' r
                        False ->
                          do PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                             invokeEvent p $ updateResourceWaitTime r (pointTime p - t)
                             invokeEvent p $ updateResourceUtilisationCount r 1
                             invokeEvent p $ processPreemptionEnd pid
               
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
  do let t = pointTime p
     a <- readIORef (resourceCountRef r)
     when (a == 0) $
       throwIO $
       SimulationRetry
       "The resource exceeded and its count is zero: decResourceCount'"
     f <- PQ.queueNull (resourceActingQueue r)
     unless f $
       do (p0', item0) <- PQ.queueFront (resourceActingQueue r)
          let p0 = - p0'
              pid0 = actingItemId item0
          PQ.dequeue (resourceActingQueue r)
          PQ.enqueue (resourceWaitQueue r) p0 (Right $ ResourcePreemptedItem p0 t pid0)
          invokeEvent p $ processPreemptionBegin pid0
          invokeEvent p $ updateResourceUtilisationCount r (-1)
          invokeEvent p $ updateResourceQueueCount r 1
     invokeEvent p $ updateResourceCount r (-1)

-- | Increase the count of available resource by the specified number,
-- invoking the awaiting and preempted processes according to their priorities
-- as needed.
incResourceCount :: Resource
                    -- ^ the resource
                    -> Int
                    -- ^ the increment for the resource count
                    -> Event ()
incResourceCount r n
  | n < 0     = throwEvent $ SimulationRetry "The increment cannot be negative: incResourceCount"
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
  | n < 0     = throwEvent $ SimulationRetry "The decrement cannot be negative: decResourceCount"
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

-- | Signal triggered when one of the resource counters changes.
resourceChanged_ :: Resource -> Signal ()
resourceChanged_ r =
  resourceCountChanged_ r <>
  resourceUtilisationCountChanged_ r <>
  resourceQueueCountChanged_ r

-- | Update the resource count and its statistics.
updateResourceCount :: Resource -> Int -> Event ()
updateResourceCount r delta =
  Event $ \p ->
  do a <- readIORef (resourceCountRef r)
     let a' = a + delta
     a' `seq` writeIORef (resourceCountRef r) a'
     modifyIORef' (resourceCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (resourceCountSource r) a'

-- | Update the resource queue length and its statistics.
updateResourceQueueCount :: Resource -> Int -> Event ()
updateResourceQueueCount r delta =
  Event $ \p ->
  do a <- readIORef (resourceQueueCountRef r)
     let a' = a + delta
     a' `seq` writeIORef (resourceQueueCountRef r) a'
     modifyIORef' (resourceQueueCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (resourceQueueCountSource r) a'

-- | Update the resource utilisation count and its statistics.
updateResourceUtilisationCount :: Resource -> Int -> Event ()
updateResourceUtilisationCount r delta =
  Event $ \p ->
  do a <- readIORef (resourceUtilisationCountRef r)
     let a' = a + delta
     a' `seq` writeIORef (resourceUtilisationCountRef r) a'
     modifyIORef' (resourceUtilisationCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (resourceUtilisationCountSource r) a'

-- | Update the resource wait time and its statistics.
updateResourceWaitTime :: Resource -> Double -> Event ()
updateResourceWaitTime r delta =
  Event $ \p ->
  do a <- readIORef (resourceTotalWaitTimeRef r)
     let a' = a + delta
     a' `seq` writeIORef (resourceTotalWaitTimeRef r) a'
     modifyIORef' (resourceWaitTimeRef r) $
       addSamplingStats delta
     invokeEvent p $
       triggerSignal (resourceWaitTimeSource r) ()
