
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Results.Transform
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- The module defines useful result transformations that can
-- be used in simulation experiments.
--
module Simulation.Aivika.Results.Transform
       (-- * Basic Class Type
        ResultTransformer(..),
        -- * Sampling Statistics
        SamplingStats(..),
        samplingStatsCount,
        samplingStatsMin,
        samplingStatsMax,
        samplingStatsMean,
        samplingStatsMean2,
        samplingStatsVariance,
        samplingStatsDeviation,
        -- * Time-dependent Statistics
        TimingStats(..),
        timingStatsCount,
        timingStatsMin,
        timingStatsMax,
        timingStatsMean,
        timingStatsVariance,
        timingStatsDeviation,
        timingStatsMinTime,
        timingStatsMaxTime,
        timingStatsStartTime,
        timingStatsLastTime,
        timingStatsSum,
        timingStatsSum2,
        -- * Sampling-based Counter
        SamplingCounter(..),
        samplingCounterValue,
        samplingCounterStats,
        -- * Time-dependent Counter
        TimingCounter(..),
        timingCounterValue,
        timingCounterStats,
        -- * Queue
        Queue(..),
        enqueueStrategy,
        enqueueStoringStrategy,
        dequeueStrategy,
        queueNull,
        queueFull,
        queueMaxCount,
        queueCount,
        queueCountStats,
        enqueueCount,
        enqueueLostCount,
        enqueueStoreCount,
        dequeueCount,
        dequeueExtractCount,
        queueLoadFactor,
        enqueueRate,
        enqueueStoreRate,
        dequeueRate,
        dequeueExtractRate,
        queueWaitTime,
        queueTotalWaitTime,
        enqueueWaitTime,
        dequeueWaitTime,
        queueRate,
        -- * Arrival Timer
        ArrivalTimer(..),
        arrivalProcessingTime,
        -- * Server
        Server(..),
        serverInitState,
        serverState,
        serverTotalInputWaitTime,
        serverTotalProcessingTime,
        serverTotalOutputWaitTime,
        serverTotalPreemptionTime,
        serverInputWaitTime,
        serverProcessingTime,
        serverOutputWaitTime,
        serverPreemptionTime,
        serverInputWaitFactor,
        serverProcessingFactor,
        serverOutputWaitFactor,
        serverPreemptionFactor,
        -- * Activity
        Activity(..),
        activityInitState,
        activityState,
        activityTotalUtilisationTime,
        activityTotalIdleTime,
        activityTotalPreemptionTime,
        activityUtilisationTime,
        activityIdleTime,
        activityPreemptionTime,
        activityUtilisationFactor,
        activityIdleFactor,
        activityPreemptionFactor,
        -- * Resource
        Resource(..),
        resourceCount,
        resourceCountStats,
        resourceUtilisationCount,
        resourceUtilisationCountStats,
        resourceQueueCount,
        resourceQueueCountStats,
        resourceTotalWaitTime,
        resourceWaitTime) where

import Control.Arrow

import Simulation.Aivika.Results
import Simulation.Aivika.Results.Locale

-- | Something that can transform the results.
class ResultTransformer a where

  -- | Return the result transform.
  tr :: a -> ResultTransform

instance ResultTransformer ResultTransform where
  tr = id

-- | Represents a statistics based upon observations.
newtype SamplingStats = SamplingStats ResultTransform

instance ResultTransformer SamplingStats where
  tr (SamplingStats a) = a

-- | The total number of samples.
samplingStatsCount :: SamplingStats -> ResultTransform
samplingStatsCount (SamplingStats a) =
  a >>> resultById SamplingStatsCountId

-- | The minimum value among the samples.
samplingStatsMin :: SamplingStats -> ResultTransform
samplingStatsMin (SamplingStats a) =
  a >>> resultById SamplingStatsMinId

-- | The maximum value among the samples.
samplingStatsMax :: SamplingStats -> ResultTransform
samplingStatsMax (SamplingStats a) =
  a >>> resultById SamplingStatsMaxId
  
-- | The average value.
samplingStatsMean :: SamplingStats -> ResultTransform
samplingStatsMean (SamplingStats a) =
  a >>> resultById SamplingStatsMeanId

-- | The average square value.
samplingStatsMean2 :: SamplingStats -> ResultTransform
samplingStatsMean2 (SamplingStats a) =
  a >>> resultById SamplingStatsMean2Id

-- | Return tha variance.
samplingStatsVariance :: SamplingStats -> ResultTransform
samplingStatsVariance (SamplingStats a) =
  a >>> resultById SamplingStatsVarianceId

-- | Return the deviation.
samplingStatsDeviation :: SamplingStats -> ResultTransform
samplingStatsDeviation (SamplingStats a) =
  a >>> resultById SamplingStatsDeviationId

-- | A counter for which the statistics is collected too.
newtype SamplingCounter = SamplingCounter ResultTransform

instance ResultTransformer SamplingCounter where
  tr (SamplingCounter a) = a

-- | The counter value.
samplingCounterValue :: SamplingCounter -> ResultTransform
samplingCounterValue (SamplingCounter a) =
  a >>> resultById SamplingCounterValueId

-- | The counter statistics.
samplingCounterStats :: SamplingCounter -> SamplingStats
samplingCounterStats (SamplingCounter a) =
  SamplingStats (a >>> resultById SamplingCounterStatsId)

-- | The time-dependent statistics.
newtype TimingStats = TimingStats ResultTransform

instance ResultTransformer TimingStats where
  tr (TimingStats a) = a

-- | Return the number of samples.
timingStatsCount :: TimingStats -> ResultTransform
timingStatsCount (TimingStats a) =
  a >>> resultById TimingStatsCountId

-- | Return the minimum value.
timingStatsMin :: TimingStats -> ResultTransform
timingStatsMin (TimingStats a) =
  a >>> resultById TimingStatsMinId

-- | Return the maximum value.
timingStatsMax :: TimingStats -> ResultTransform
timingStatsMax (TimingStats a) =
  a >>> resultById TimingStatsMaxId

-- | Return the average value.
timingStatsMean :: TimingStats -> ResultTransform
timingStatsMean (TimingStats a) =
  a >>> resultById TimingStatsMeanId

-- | Return the variance.
timingStatsVariance :: TimingStats -> ResultTransform
timingStatsVariance (TimingStats a) =
  a >>> resultById TimingStatsVarianceId

-- | Return the deviation.
timingStatsDeviation :: TimingStats -> ResultTransform
timingStatsDeviation (TimingStats a) =
  a >>> resultById TimingStatsDeviationId

-- | Return the time at which the minimum is attained.
timingStatsMinTime :: TimingStats -> ResultTransform
timingStatsMinTime (TimingStats a) =
  a >>> resultById TimingStatsMinTimeId

-- | Return the time at which the maximum is attained.
timingStatsMaxTime :: TimingStats -> ResultTransform
timingStatsMaxTime (TimingStats a) =
  a >>> resultById TimingStatsMaxTimeId

-- | Return the start time of sampling.
timingStatsStartTime :: TimingStats -> ResultTransform
timingStatsStartTime (TimingStats a) =
  a >>> resultById TimingStatsStartTimeId

-- | Return the last time of sampling.
timingStatsLastTime :: TimingStats -> ResultTransform
timingStatsLastTime (TimingStats a) =
  a >>> resultById TimingStatsLastTimeId

-- | Return the sum of values.
timingStatsSum :: TimingStats -> ResultTransform
timingStatsSum (TimingStats a) =
  a >>> resultById TimingStatsSumId

-- | Return the sum of square values.
timingStatsSum2 :: TimingStats -> ResultTransform
timingStatsSum2 (TimingStats a) =
  a >>> resultById TimingStatsSum2Id

-- | A time-dependent counter that collects the statistics too.
newtype TimingCounter = TimingCounter ResultTransform

instance ResultTransformer TimingCounter where
  tr (TimingCounter a) = a

-- | The counter value.
timingCounterValue :: TimingCounter -> ResultTransform
timingCounterValue (TimingCounter a) =
  a >>> resultById TimingCounterValueId

-- | The counter statistics.
timingCounterStats :: TimingCounter -> TimingStats
timingCounterStats (TimingCounter a) =
  TimingStats (a >>> resultById TimingCounterStatsId)

-- | Represents either finite or infinite queue.
newtype Queue = Queue ResultTransform

instance ResultTransformer Queue where
  tr (Queue a) = a

-- | The strategy applied to the enqueueing (input) processes when the finite queue is full.
enqueueStrategy :: Queue -> ResultTransform
enqueueStrategy (Queue a) =
  a >>> resultById EnqueueStrategyId

-- | The strategy applied when storing (in memory) items in the queue.
enqueueStoringStrategy :: Queue -> ResultTransform
enqueueStoringStrategy (Queue a) =
  a >>> resultById EnqueueStoringStrategyId

-- | The strategy applied to the dequeueing (output) processes when the queue is empty.
dequeueStrategy :: Queue -> ResultTransform
dequeueStrategy (Queue a) =
  a >>> resultById DequeueStrategyId

-- | Test whether the queue is empty.
queueNull :: Queue -> ResultTransform
queueNull (Queue a) =
  a >>> resultById QueueNullId

-- | Test whether the finite queue is full.
queueFull :: Queue -> ResultTransform
queueFull (Queue a) =
  a >>> resultById QueueFullId

-- | The finite queue capacity.
queueMaxCount :: Queue -> ResultTransform
queueMaxCount (Queue a) =
  a >>> resultById QueueMaxCountId

-- | Return the current queue size.
queueCount :: Queue -> ResultTransform
queueCount (Queue a) =
  a >>> resultById QueueCountId

-- | Return the queue size statistics.
queueCountStats :: Queue -> TimingStats
queueCountStats (Queue a) =
  TimingStats (a >>> resultById QueueCountStatsId)

-- | Return the total number of input items that were enqueued in the finite queue.
enqueueCount :: Queue -> ResultTransform
enqueueCount (Queue a) =
  a >>> resultById EnqueueCountId

-- | Return the number of lost items for the finite queue.
enqueueLostCount :: Queue -> ResultTransform
enqueueLostCount (Queue a) =
  a >>> resultById EnqueueLostCountId

-- | Return the total number of input items that were stored.
enqueueStoreCount :: Queue -> ResultTransform
enqueueStoreCount (Queue a) =
  a >>> resultById EnqueueStoreCountId

-- | Return the total number of requests for dequeueing the items, not taking
-- into account the failed attempts to dequeue immediately without suspension.
dequeueCount :: Queue -> ResultTransform
dequeueCount (Queue a) =
  a >>> resultById DequeueCountId

-- | Return the total number of output items that were actually dequeued.
dequeueExtractCount :: Queue -> ResultTransform
dequeueExtractCount (Queue a) =
  a >>> resultById DequeueExtractCountId

-- | Return the load factor: the finite queue size divided by its capacity.
queueLoadFactor :: Queue -> ResultTransform
queueLoadFactor (Queue a) =
  a >>> resultById QueueLoadFactorId

-- | Return the rate of the input items that were enqueued in the finite queue:
-- how many items per time.
enqueueRate :: Queue -> ResultTransform
enqueueRate (Queue a) =
  a >>> resultById EnqueueRateId

-- | Return the rate of the items that were stored: how many items per time.
enqueueStoreRate :: Queue -> ResultTransform
enqueueStoreRate (Queue a) =
  a >>> resultById EnqueueStoreRateId

-- | Return the rate of the requests for dequeueing the items: how many
-- requests per time. It does not include the failed attempts to dequeue
-- immediately without suspension.
dequeueRate :: Queue -> ResultTransform
dequeueRate (Queue a) =
  a >>> resultById DequeueRateId

-- | Return the rate of the output items that were dequeued: how many items per time.
dequeueExtractRate :: Queue -> ResultTransform
dequeueExtractRate (Queue a) =
  a >>> resultById DequeueExtractRateId

-- | Return the wait time from the time at which the item was stored in
-- the queue to the time at which it was dequeued.
queueWaitTime :: Queue -> SamplingStats
queueWaitTime (Queue a) =
  SamplingStats (a >>> resultById QueueWaitTimeId)

-- | Return the total wait time for the finite queue from the time at which
-- the enqueueing operation was initiated to the time at which the item was dequeued.
queueTotalWaitTime :: Queue -> SamplingStats
queueTotalWaitTime (Queue a) =
  SamplingStats (a >>> resultById QueueTotalWaitTimeId)

-- | Return the wait time from the time at which the item was stored in
-- the queue to the time at which it was dequeued.
enqueueWaitTime :: Queue -> SamplingStats
enqueueWaitTime (Queue a) =
  SamplingStats (a >>> resultById EnqueueWaitTimeId)

-- | Return the dequeue wait time from the time at which the item was requested
-- for dequeueing to the time at which it was actually dequeued.
dequeueWaitTime :: Queue -> SamplingStats
dequeueWaitTime (Queue a) =
  SamplingStats (a >>> resultById DequeueWaitTimeId)

-- | Return a long-term average queue rate calculated as the average queue size
-- divided by the average wait time.
queueRate :: Queue -> ResultTransform
queueRate (Queue a) =
  a >>> resultById QueueRateId

-- | Accumulates the statistics about that how long the arrived events are processed.
newtype ArrivalTimer = ArrivalTimer ResultTransform

instance ResultTransformer ArrivalTimer where
  tr (ArrivalTimer a) = a

-- | Return the statistics about that how long the arrived events were processed.
arrivalProcessingTime :: ArrivalTimer -> SamplingStats
arrivalProcessingTime (ArrivalTimer a) =
  SamplingStats (a >>> resultById ArrivalProcessingTimeId)

-- | It models the server that prodives a service.
newtype Server = Server ResultTransform

instance ResultTransformer Server where
  tr (Server a) = a

-- | The initial state of the server.
serverInitState :: Server -> ResultTransform
serverInitState (Server a) =
  a >>> resultById ServerInitStateId

-- | Return the current state of the server.
serverState :: Server -> ResultTransform
serverState (Server a) =
  a >>> resultById ServerStateId

-- | Return the counted total time when the server was locked while
-- awaiting the input.
serverTotalInputWaitTime :: Server -> ResultTransform
serverTotalInputWaitTime (Server a) =
  a >>> resultById ServerTotalInputWaitTimeId

-- | Return the counted total time spent by the server while
-- processing the tasks.
serverTotalProcessingTime :: Server -> ResultTransform
serverTotalProcessingTime (Server a) =
  a >>> resultById ServerTotalProcessingTimeId

-- | Return the counted total time when the server was locked while
-- trying to deliver the output.
serverTotalOutputWaitTime :: Server -> ResultTransform
serverTotalOutputWaitTime (Server a) =
  a >>> resultById ServerTotalOutputWaitTimeId

-- | Return the counted total time spent by the server while it was
-- preempted waiting for the further proceeding.
serverTotalPreemptionTime :: Server -> ResultTransform
serverTotalPreemptionTime (Server a) =
  a >>> resultById ServerTotalPreemptionTimeId

-- | Return the statistics of the time when the server was locked
-- while awaiting the input.
serverInputWaitTime :: Server -> SamplingStats
serverInputWaitTime (Server a) =
  SamplingStats (a >>> resultById ServerInputWaitTimeId)

-- | Return the statistics of the time spent by the server while
-- processing the tasks.
serverProcessingTime :: Server -> SamplingStats
serverProcessingTime (Server a) =
  SamplingStats (a >>> resultById ServerProcessingTimeId)

-- | Return the statistics of the time when the server was locked
-- while trying to deliver the output.
serverOutputWaitTime :: Server -> SamplingStats
serverOutputWaitTime (Server a) =
  SamplingStats (a >>> resultById ServerOutputWaitTimeId)

-- | Return the statistics of the time spent by the server while
-- it was preempted waiting for the further proceeding.
serverPreemptionTime :: Server -> SamplingStats
serverPreemptionTime (Server a) =
  SamplingStats (a >>> resultById ServerPreemptionTimeId)

-- | It returns the factor changing from 0 to 1, which estimates
-- how often the server was awaiting for the next input task.
serverInputWaitFactor :: Server -> ResultTransform
serverInputWaitFactor (Server a) =
  a >>> resultById ServerInputWaitFactorId

-- | It returns the factor changing from 0 to 1, which estimates
-- how often the server was busy with direct processing its tasks.
serverProcessingFactor :: Server -> ResultTransform
serverProcessingFactor (Server a) =
  a >>> resultById ServerProcessingFactorId

-- | It returns the factor changing from 0 to 1, which estimates
-- how often the server was locked trying to deliver the output
-- after the task is finished.
serverOutputWaitFactor :: Server -> ResultTransform
serverOutputWaitFactor (Server a) =
  a >>> resultById ServerOutputWaitFactorId

-- | It returns the factor changing from 0 to 1, which estimates
-- how often the server was preempted waiting for the further proceeding.
serverPreemptionFactor :: Server -> ResultTransform
serverPreemptionFactor (Server a) =
  a >>> resultById ServerPreemptionFactorId

-- | It models an activity that can be utilised.
newtype Activity = Activity ResultTransform

instance ResultTransformer Activity where
  tr (Activity a) = a

-- | The initial state of the activity.
activityInitState :: Activity -> ResultTransform
activityInitState (Activity a) =
  a >>> resultById ActivityInitStateId

-- | Return the current state of the activity.
activityState :: Activity -> ResultTransform
activityState (Activity a) =
  a >>> resultById ActivityStateId

-- | Return the counted total time when the activity was utilised.
activityTotalUtilisationTime :: Activity -> ResultTransform
activityTotalUtilisationTime (Activity a) =
  a >>> resultById ActivityTotalUtilisationTimeId

-- | Return the counted total time when the activity was idle.
activityTotalIdleTime :: Activity -> ResultTransform
activityTotalIdleTime (Activity a) =
  a >>> resultById ActivityTotalIdleTimeId

-- | Return the counted total time when the activity was preemted
-- waiting for the further proceeding.
activityTotalPreemptionTime :: Activity -> ResultTransform
activityTotalPreemptionTime (Activity a) =
  a >>> resultById ActivityTotalPreemptionTimeId

-- | Return the statistics for the time when the activity was utilised.
activityUtilisationTime :: Activity -> SamplingStats
activityUtilisationTime (Activity a) =
  SamplingStats (a >>> resultById ActivityUtilisationTimeId)

-- | Return the statistics for the time when the activity was idle.
activityIdleTime :: Activity -> SamplingStats
activityIdleTime (Activity a) =
  SamplingStats (a >>> resultById ActivityIdleTimeId)

-- | Return the statistics for the time when the activity was preempted
-- waiting for the further proceeding.
activityPreemptionTime :: Activity -> SamplingStats
activityPreemptionTime (Activity a) =
  SamplingStats (a >>> resultById ActivityPreemptionTimeId)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was utilised.
activityUtilisationFactor :: Activity -> ResultTransform
activityUtilisationFactor (Activity a) =
  a >>> resultById ActivityUtilisationFactorId

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was idle.
activityIdleFactor :: Activity -> ResultTransform
activityIdleFactor (Activity a) =
  a >>> resultById ActivityIdleFactorId

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was preempted waiting for the further proceeding.
activityPreemptionFactor :: Activity -> ResultTransform
activityPreemptionFactor (Activity a) =
  a >>> resultById ActivityPreemptionFactorId

-- | The resource which can be acquired and then released.
newtype Resource = Resource ResultTransform

instance ResultTransformer Resource where
  tr (Resource a) = a

-- | Return the current available count of the resource.
resourceCount :: Resource -> ResultTransform
resourceCount (Resource a) =
  a >>> resultById ResourceCountId

-- | Return the statistics for the available count of the resource.
resourceCountStats :: Resource -> TimingStats
resourceCountStats (Resource a) =
  TimingStats (a >>> resultById ResourceCountStatsId)

-- | Return the current utilisation count of the resource.
resourceUtilisationCount :: Resource -> ResultTransform
resourceUtilisationCount (Resource a) =
  a >>> resultById ResourceUtilisationCountId

-- | Return the statistics for the utilisation count of the resource.
resourceUtilisationCountStats :: Resource -> TimingStats
resourceUtilisationCountStats (Resource a) =
  TimingStats (a >>> resultById ResourceUtilisationCountStatsId)

-- | Return the current queue length of the resource.
resourceQueueCount :: Resource -> ResultTransform
resourceQueueCount (Resource a) =
  a >>> resultById ResourceQueueCountId

-- | Return the statistics for the queue length of the resource.
resourceQueueCountStats :: Resource -> TimingStats
resourceQueueCountStats (Resource a) =
  TimingStats (a >>> resultById ResourceQueueCountStatsId)

-- | Return the total wait time of the resource.
resourceTotalWaitTime :: Resource -> ResultTransform
resourceTotalWaitTime (Resource a) =
  a >>> resultById ResourceTotalWaitTimeId

-- | Return the statistics for the wait time of the resource.
resourceWaitTime :: Resource -> SamplingStats
resourceWaitTime (Resource a) =
  SamplingStats (a >>> resultById ResourceWaitTimeId)
