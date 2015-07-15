
-- |
-- Module     : Simulation.Aivika.Results.Locale
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- The module defines locales for outputting and printing the simulation results.
--
module Simulation.Aivika.Results.Locale
       (-- * Basic Types
        ResultLocale,
        ResultLocalisation,
        ResultDescription,
        -- * Locale Codes
        russianResultLocale,
        englishResultLocale,
        -- * Localisations
        lookupResultLocalisation,
        russianResultLocalisation,
        englishResultLocalisation,
        -- * Unique Identifiers
        ResultId(..)) where

import qualified Data.Map as M

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Statistics
import Simulation.Aivika.Statistics.Accumulator
import qualified Simulation.Aivika.Queue as Q
import qualified Simulation.Aivika.Queue.Infinite as IQ
import Simulation.Aivika.Arrival
import Simulation.Aivika.Server
import Simulation.Aivika.Activity
import Simulation.Aivika.Resource
import Simulation.Aivika.Operation

-- | A locale to output the simulation results.
--
-- Examples are: @\"ru\", @\"en\" etc.
type ResultLocale = String

-- | It localises the description of simulation results.
type ResultLocalisation = ResultId -> ResultDescription

-- | A description used for describing the results when generating output.
type ResultDescription = String

-- | The result entity identifier.
data ResultId = TimeId
                -- ^ A 'time' computation.
              | VectorId
                -- ^ Describes a vector.
              | VectorItemId String
                -- ^ Describes a vector item with the specified subscript.
              | SamplingStatsId
                -- ^ A 'SamplingStats' value.
              | SamplingStatsCountId
                -- ^ Property 'samplingStatsCount'.
              | SamplingStatsMinId
                -- ^ Property 'samplingStatsMin'.
              | SamplingStatsMaxId
                -- ^ Property 'samplingStatsMax'.
              | SamplingStatsMeanId
                -- ^ Property 'samplingStatsMean'.
              | SamplingStatsMean2Id
                -- ^ Property 'samplingStatsMean2'.
              | SamplingStatsVarianceId
                -- ^ Property 'samplingStatsVariance'.
              | SamplingStatsDeviationId
                -- ^ Property 'samplingStatsDeviation'.
              | SamplingCounterId
                -- ^ A 'SamplingCounter' value.
              | SamplingCounterValueId
                -- ^ Property 'samplingCounterValue'.
              | SamplingCounterStatsId
                -- ^ Property 'samplingCounterStats'.
              | TimingStatsId
                -- ^ A 'TimingStats' value.
              | TimingStatsCountId
                -- ^ Property 'timingStatsCount'.
              | TimingStatsMinId
                -- ^ Property 'timingStatsMin'.
              | TimingStatsMaxId
                -- ^ Property 'timingStatsMax'.
              | TimingStatsMeanId
                -- ^ Property 'timingStatsMean'.
              | TimingStatsVarianceId
                -- ^ Property 'timingStatsVariance'.
              | TimingStatsDeviationId
                -- ^ Property 'timingStatsDeviation'.
              | TimingStatsMinTimeId
                -- ^ Property 'timingStatsMinTime'.
              | TimingStatsMaxTimeId
                -- ^ Property 'timingStatsMaxTime'.
              | TimingStatsStartTimeId
                -- ^ Property 'timingStatsStartTime'.
              | TimingStatsLastTimeId
                -- ^ Property 'timingStatsLastTime'.
              | TimingStatsSumId
                -- ^ Property 'timingStatsSum'.
              | TimingStatsSum2Id
                -- ^ Property 'timingStatsSum2'.
              | TimingCounterId
                -- ^ A 'TimingCounter' value.
              | TimingCounterValueId
                -- ^ Property 'timingCounterValue'.
              | TimingCounterStatsId
                -- ^ Property 'timingCounterStats'.
              | FiniteQueueId
                -- ^ A finite 'Q.Queue'.
              | InfiniteQueueId
                -- ^ An infinite 'IQ.Queue'.
              | EnqueueStrategyId
                -- ^ Property 'Q.enqueueStrategy'.
              | EnqueueStoringStrategyId
                -- ^ Property 'Q.enqueueStoringStrategy'.
              | DequeueStrategyId
                -- ^ Property 'Q.dequeueStrategy'.
              | QueueNullId
                -- ^ Property 'Q.queueNull'.
              | QueueFullId
                -- ^ Property 'Q.queueFull'.
              | QueueMaxCountId
                -- ^ Property 'Q.queueMaxCount'.
              | QueueCountId
                -- ^ Property 'Q.queueCount'.
              | QueueCountStatsId
                -- ^ Property 'Q.queueCountStats'.
              | EnqueueCountId
                -- ^ Property 'Q.enqueueCount'.
              | EnqueueLostCountId
                -- ^ Property 'Q.enqueueLostCount'.
              | EnqueueStoreCountId
                -- ^ Property 'Q.enqueueStoreCount'.
              | DequeueCountId
                -- ^ Property 'Q.dequeueCount'.
              | DequeueExtractCountId
                -- ^ Property 'Q.dequeueExtractCount'.
              | QueueLoadFactorId
                -- ^ Property 'Q.queueLoadFactor'.
              | EnqueueRateId
                -- ^ Property 'Q.enqueueRate'.
              | EnqueueStoreRateId
                -- ^ Property 'Q.enqueueStoreRate'.
              | DequeueRateId
                -- ^ Property 'Q.dequeueRate'.
              | DequeueExtractRateId
                -- ^ Property 'Q.dequeueExtractRate'.
              | QueueWaitTimeId
                -- ^ Property 'Q.queueWaitTime'.
              | QueueTotalWaitTimeId
                -- ^ Property 'Q.queueTotalWaitTime'.
              | EnqueueWaitTimeId
                -- ^ Property 'Q.enqueueWaitTime'.
              | DequeueWaitTimeId
                -- ^ Property 'Q.dequeueWaitTime'.
              | QueueRateId
                -- ^ Property 'Q.queueRate'.
              | ArrivalTimerId
                -- ^ An 'ArrivalTimer'.
              | ArrivalProcessingTimeId
                -- ^ Property 'arrivalProcessingTime'.
              | ServerId
                -- ^ Represents a 'Server'.
              | ServerInitStateId
                -- ^ Property 'serverInitState'.
              | ServerStateId
                -- ^ Property 'serverState'.
              | ServerTotalInputWaitTimeId
                -- ^ Property 'serverTotalInputWaitTime'.
              | ServerTotalProcessingTimeId
                -- ^ Property 'serverTotalProcessingTime'.
              | ServerTotalOutputWaitTimeId
                -- ^ Property 'serverTotalOutputWaitTime'.
              | ServerTotalPreemptionTimeId
                -- ^ Property 'serverTotalPreemptionTime'.
              | ServerInputWaitTimeId
                -- ^ Property 'serverInputWaitTime'.
              | ServerProcessingTimeId
                -- ^ Property 'serverProcessingTime'.
              | ServerOutputWaitTimeId
                -- ^ Property 'serverOutputWaitTime'.
              | ServerPreemptionTimeId
                -- ^ Property 'serverPreemptionTime'.
              | ServerInputWaitFactorId
                -- ^ Property 'serverInputWaitFactor'.
              | ServerProcessingFactorId
                -- ^ Property 'serverProcessingFactor'.
              | ServerOutputWaitFactorId
                -- ^ Property 'serverOutputWaitFactor'.
              | ServerPreemptionFactorId
                -- ^ Property 'serverPreemptionFactor'.
              | ActivityId
                -- ^ Represents an 'Activity'.
              | ActivityInitStateId
                -- ^ Property 'activityInitState'.
              | ActivityStateId
                -- ^ Property 'activityState'.
              | ActivityTotalUtilisationTimeId
                -- ^ Property 'activityTotalUtilisationTime'.
              | ActivityTotalIdleTimeId
                -- ^ Property 'activityTotalIdleTime'.
              | ActivityTotalPreemptionTimeId
                -- ^ Property 'activityTotalPreemptionTime'.
              | ActivityUtilisationTimeId
                -- ^ Property 'activityUtilisationTime'.
              | ActivityIdleTimeId
                -- ^ Property 'activityIdleTime'.
              | ActivityPreemptionTimeId
                -- ^ Property 'activityPreemptionTime'.
              | ActivityUtilisationFactorId
                -- ^ Property 'activityUtilisationFactor'.
              | ActivityIdleFactorId
                -- ^ Property 'activityIdleFactor'.
              | ActivityPreemptionFactorId
                -- ^ Property 'activityPreemptionFactor'.
              | ResourceId
                -- ^ Represents a 'Resource'.
              | ResourceCountId
                -- ^ Property 'resourceCount'.
              | ResourceCountStatsId
                -- ^ Property 'resourceCountStats'.
              | ResourceUtilisationCountId
                -- ^ Property 'resourceUtilisationCount'.
              | ResourceUtilisationCountStatsId
                -- ^ Property 'resourceUtilisationCountStats'.
              | ResourceQueueCountId
                -- ^ Property 'resourceQueueCount'.
              | ResourceQueueCountStatsId
                -- ^ Property 'resourceQueueCountStats'.
              | ResourceTotalWaitTimeId
                -- ^ Property 'resourceTotalWaitTime'.
              | ResourceWaitTimeId
                -- ^ Property 'resourceWaitTime'.
              | OperationId
                -- ^ Represents an 'Operation'.
              | OperationTotalUtilisationTimeId
                -- ^ Property 'operationTotalUtilisationTime'.
              | OperationTotalPreemptionTimeId
                -- ^ Property 'operationTotalPreemptionTime'.
              | OperationUtilisationTimeId
                -- ^ Property 'operationUtilisationTime'.
              | OperationPreemptionTimeId
                -- ^ Property 'operationPreemptionTime'.
              | UserDefinedResultId ResultDescription
                -- ^ An user defined description.
              | LocalisedResultId (M.Map ResultLocale ResultDescription)
                -- ^ A localised property or object name.
              deriving (Eq, Ord, Show)

-- | The Russian locale.
russianResultLocale :: ResultLocale
russianResultLocale = "ru"

-- | The English locale.
englishResultLocale :: ResultLocale
englishResultLocale = "en"

-- | The Russian localisation of the simulation results.
russianResultLocalisation :: ResultLocalisation
russianResultLocalisation TimeId = "модельное время"
russianResultLocalisation VectorId = "вектор"
russianResultLocalisation (VectorItemId x) = "элемент с индексом " ++ x
russianResultLocalisation SamplingStatsId = "сводная статистика"
russianResultLocalisation SamplingStatsCountId = "количество"
russianResultLocalisation SamplingStatsMinId = "минимальное значение"
russianResultLocalisation SamplingStatsMaxId = "максимальное значение"
russianResultLocalisation SamplingStatsMeanId = "среднее значение"
russianResultLocalisation SamplingStatsMean2Id = "среднее квадратов"
russianResultLocalisation SamplingStatsVarianceId = "дисперсия"
russianResultLocalisation SamplingStatsDeviationId = "среднеквадратическое отклонение"
russianResultLocalisation TimingStatsId = "временная статистика"
russianResultLocalisation TimingStatsCountId = "количество"
russianResultLocalisation TimingStatsMinId = "минимальное значение"
russianResultLocalisation TimingStatsMaxId = "максимальное значение"
russianResultLocalisation TimingStatsMeanId = "среднее значение"
russianResultLocalisation TimingStatsVarianceId = "дисперсия"
russianResultLocalisation TimingStatsDeviationId = "среднеквадратическое отклонение"
russianResultLocalisation TimingStatsMinTimeId = "время достижения минимума"
russianResultLocalisation TimingStatsMaxTimeId = "время достижения максимума"
russianResultLocalisation TimingStatsStartTimeId = "начальное время сбора статистики"
russianResultLocalisation TimingStatsLastTimeId = "конечное время сбора статистики"
russianResultLocalisation TimingStatsSumId = "сумма"
russianResultLocalisation TimingStatsSum2Id = "сумма квадратов"
russianResultLocalisation SamplingCounterId = "счетчик"
russianResultLocalisation SamplingCounterValueId = "текущее значение"
russianResultLocalisation SamplingCounterStatsId = "статистика"
russianResultLocalisation TimingCounterId = "временной счетчик"
russianResultLocalisation TimingCounterValueId = "текущее значение"
russianResultLocalisation TimingCounterStatsId = "статистика"
russianResultLocalisation FiniteQueueId = "конечная очередь"
russianResultLocalisation InfiniteQueueId = "бесконечная очередь"
russianResultLocalisation EnqueueStrategyId = "стратегия добавления элементов"
russianResultLocalisation EnqueueStoringStrategyId = "стратегия хранения элементов"
russianResultLocalisation DequeueStrategyId = "стратегия извлечения элементов"
russianResultLocalisation QueueNullId = "очередь пуста?"
russianResultLocalisation QueueFullId = "очередь заполнена?"
russianResultLocalisation QueueMaxCountId = "емкость очереди"
russianResultLocalisation QueueCountId = "текущий размер очереди"
russianResultLocalisation QueueCountStatsId = "статистика по размеру очереди"
russianResultLocalisation EnqueueCountId = "общее количество попыток добавить элементы"
russianResultLocalisation EnqueueLostCountId = "общее количество неудачных попыток добавить элементы"
russianResultLocalisation EnqueueStoreCountId = "общее количество сохраненных элементов"
russianResultLocalisation DequeueCountId = "общее количество запросов на извлечение элементов"
russianResultLocalisation DequeueExtractCountId = "общее количество извлеченных элементов"
russianResultLocalisation QueueLoadFactorId = "коэфф. загрузки (размер, поделенный на емкость)"
russianResultLocalisation EnqueueRateId = "количество попыток добавить на ед. времени"
russianResultLocalisation EnqueueStoreRateId = "количество сохраненных на ед. времени"
russianResultLocalisation DequeueRateId = "количество запросов на извлечение в ед. времени"
russianResultLocalisation DequeueExtractRateId = "количество извлеченных на ед. времени"
russianResultLocalisation QueueWaitTimeId = "время ожидания (сохранили -> извлекли)"
russianResultLocalisation QueueTotalWaitTimeId = "общее время ожидания (попытались добавить -> извлекли)"
russianResultLocalisation EnqueueWaitTimeId = "время ожидания добавления (попытались добавить -> сохранили)"
russianResultLocalisation DequeueWaitTimeId = "время ожидания извлечения (запросили извлечь -> извлекли)"
russianResultLocalisation QueueRateId = "усредненная скорость (как средняя длина очереди на среднее время ожидания)"
russianResultLocalisation ArrivalTimerId = "как долго обрабатываются заявки?"
russianResultLocalisation ArrivalProcessingTimeId = "время обработки заявки"
russianResultLocalisation ServerId = "сервер"
russianResultLocalisation ServerInitStateId = "начальное состояние"
russianResultLocalisation ServerStateId = "текущее состояние"
russianResultLocalisation ServerTotalInputWaitTimeId = "общее время блокировки в ожидании ввода"
russianResultLocalisation ServerTotalProcessingTimeId = "общее время, потраченное на саму обработку заданий"
russianResultLocalisation ServerTotalOutputWaitTimeId = "общее время блокировки при попытке доставить вывод"
russianResultLocalisation ServerTotalPreemptionTimeId = "общее время вытеснения"
russianResultLocalisation ServerInputWaitTimeId = "время блокировки в ожидании ввода"
russianResultLocalisation ServerProcessingTimeId = "время, потраченное на саму обработку заданий"
russianResultLocalisation ServerOutputWaitTimeId = "время блокировки при попытке доставить вывод"
russianResultLocalisation ServerPreemptionTimeId = "время вытеснения"
russianResultLocalisation ServerInputWaitFactorId = "относительное время блокировки в ожидании ввода (от 0 до 1)"
russianResultLocalisation ServerProcessingFactorId = "относительное время, потраченное на саму обработку заданий (от 0 до 1)"
russianResultLocalisation ServerOutputWaitFactorId = "относительное время блокировки при попытке доставить вывод (от 0 до 1)"
russianResultLocalisation ServerPreemptionFactorId = "относительное время вытеснения (от 0 до 1)"
russianResultLocalisation ActivityId = "активность"
russianResultLocalisation ActivityInitStateId = "начальное состояние"
russianResultLocalisation ActivityStateId = "текущее состояние"
russianResultLocalisation ActivityTotalUtilisationTimeId = "общее время использования"
russianResultLocalisation ActivityTotalIdleTimeId = "общее время простоя"
russianResultLocalisation ActivityTotalPreemptionTimeId = "общее время вытеснения"
russianResultLocalisation ActivityUtilisationTimeId = "статистика времени использования"
russianResultLocalisation ActivityIdleTimeId = "статистика времени простоя"
russianResultLocalisation ActivityPreemptionTimeId = "статистика времени вытеснения"
russianResultLocalisation ActivityUtilisationFactorId = "относительное время использования (от 0 до 1)"
russianResultLocalisation ActivityIdleFactorId = "относительное время простоя (от 0 до 1)"
russianResultLocalisation ActivityPreemptionFactorId = "относительное время вытеснения (от 0 до 1)"
russianResultLocalisation ResourceId = "ресурс"
russianResultLocalisation ResourceCountId = "текущее доступное количество ресурса"
russianResultLocalisation ResourceCountStatsId = "статистика по доступному количеству ресурса"
russianResultLocalisation ResourceUtilisationCountId = "текущее используемое количество ресурса"
russianResultLocalisation ResourceUtilisationCountStatsId = "статистика по используемому количеству ресурса"
russianResultLocalisation ResourceQueueCountId = "текущая длина очереди к ресурсу"
russianResultLocalisation ResourceQueueCountStatsId = "статистика длины очереди к ресурсу"
russianResultLocalisation ResourceTotalWaitTimeId = "общее время ожидания ресурса"
russianResultLocalisation ResourceWaitTimeId = "время ожидания ресурса"
russianResultLocalisation OperationId = "операция"
russianResultLocalisation OperationTotalUtilisationTimeId = "общее время использования"
russianResultLocalisation OperationTotalPreemptionTimeId = "общее время вытеснения"
russianResultLocalisation OperationUtilisationTimeId = "статистика времени использования"
russianResultLocalisation OperationPreemptionTimeId = "статистика времени вытеснения"
russianResultLocalisation (UserDefinedResultId m) = m
russianResultLocalisation x@(LocalisedResultId m) =
  lookupResultLocalisation russianResultLocale x

-- | The English localisation of the simulation results.
englishResultLocalisation :: ResultLocalisation
englishResultLocalisation TimeId = "simulation time"
englishResultLocalisation VectorId = "vector"
englishResultLocalisation (VectorItemId x) = "item #" ++ x
englishResultLocalisation SamplingStatsId = "statistics summary"
englishResultLocalisation SamplingStatsCountId = "count"
englishResultLocalisation SamplingStatsMinId = "minimum"
englishResultLocalisation SamplingStatsMaxId = "maximum"
englishResultLocalisation SamplingStatsMeanId = "mean"
englishResultLocalisation SamplingStatsMean2Id = "mean square"
englishResultLocalisation SamplingStatsVarianceId = "variance"
englishResultLocalisation SamplingStatsDeviationId = "deviation"
englishResultLocalisation TimingStatsId = "timing statistics"
englishResultLocalisation TimingStatsCountId = "count"
englishResultLocalisation TimingStatsMinId = "minimum"
englishResultLocalisation TimingStatsMaxId = "maximum"
englishResultLocalisation TimingStatsMeanId = "mean"
englishResultLocalisation TimingStatsVarianceId = "variance"
englishResultLocalisation TimingStatsDeviationId = "deviation"
englishResultLocalisation TimingStatsMinTimeId = "the time of minimum"
englishResultLocalisation TimingStatsMaxTimeId = "the time of maximum"
englishResultLocalisation TimingStatsStartTimeId = "the start time"
englishResultLocalisation TimingStatsLastTimeId = "the last time"
englishResultLocalisation TimingStatsSumId = "sum"
englishResultLocalisation TimingStatsSum2Id = "sum square"
englishResultLocalisation SamplingCounterId = "counter"
englishResultLocalisation SamplingCounterValueId = "current value"
englishResultLocalisation SamplingCounterStatsId = "statistics"
englishResultLocalisation TimingCounterId = "timing counter"
englishResultLocalisation TimingCounterValueId = "current value"
englishResultLocalisation TimingCounterStatsId = "statistics"
englishResultLocalisation FiniteQueueId = "the finite queue"
englishResultLocalisation InfiniteQueueId = "the infinite queue"
englishResultLocalisation EnqueueStrategyId = "the enqueueing strategy"
englishResultLocalisation EnqueueStoringStrategyId = "the storing strategy"
englishResultLocalisation DequeueStrategyId = "the dequeueing strategy"
englishResultLocalisation QueueNullId = "is the queue empty?"
englishResultLocalisation QueueFullId = "is the queue full?"
englishResultLocalisation QueueMaxCountId = "the queue capacity"
englishResultLocalisation QueueCountId = "the current queue size"
englishResultLocalisation QueueCountStatsId = "the queue size statistics"
englishResultLocalisation EnqueueCountId = "a total number of attempts to enqueue the items"
englishResultLocalisation EnqueueLostCountId = "a total number of the lost items when trying to enqueue"
englishResultLocalisation EnqueueStoreCountId = "a total number of the stored items"
englishResultLocalisation DequeueCountId = "a total number of requests for dequeueing"
englishResultLocalisation DequeueExtractCountId = "a total number of the dequeued items"
englishResultLocalisation QueueLoadFactorId = "the queue load (its size divided by its capacity)"
englishResultLocalisation EnqueueRateId = "how many attempts to enqueue per time?"
englishResultLocalisation EnqueueStoreRateId = "how many items were stored per time?"
englishResultLocalisation DequeueRateId = "how many requests for dequeueing per time?"
englishResultLocalisation DequeueExtractRateId = "how many items were dequeued per time?"
englishResultLocalisation QueueWaitTimeId = "the wait time (stored -> dequeued)"
englishResultLocalisation QueueTotalWaitTimeId = "the total wait time (tried to enqueue -> dequeued)"
englishResultLocalisation EnqueueWaitTimeId = "the enqueue wait time (tried to enqueue -> stored)"
englishResultLocalisation DequeueWaitTimeId = "the dequeue wait time (requested for dequeueing -> dequeued)"
englishResultLocalisation QueueRateId = "the average queue rate (= queue size / wait time)"
englishResultLocalisation ArrivalTimerId = "how long the arrivals are processed?"
englishResultLocalisation ArrivalProcessingTimeId = "the processing time of arrivals"
englishResultLocalisation ServerId = "the server"
englishResultLocalisation ServerInitStateId = "the initial state"
englishResultLocalisation ServerStateId = "the current state"
englishResultLocalisation ServerTotalInputWaitTimeId = "the total time spent while waiting for input"
englishResultLocalisation ServerTotalProcessingTimeId = "the total time spent on actual processing the tasks"
englishResultLocalisation ServerTotalOutputWaitTimeId = "the total time spent on delivering the output"
englishResultLocalisation ServerTotalPreemptionTimeId = "the total time spent being preempted"
englishResultLocalisation ServerInputWaitTimeId = "the time spent while waiting for input"
englishResultLocalisation ServerProcessingTimeId = "the time spent on processing the tasks"
englishResultLocalisation ServerOutputWaitTimeId = "the time spent on delivering the output"
englishResultLocalisation ServerPreemptionTimeId = "the time spent being preempted"
englishResultLocalisation ServerInputWaitFactorId = "the relative time spent while waiting for input (from 0 to 1)"
englishResultLocalisation ServerProcessingFactorId = "the relative time spent on processing the tasks (from 0 to 1)"
englishResultLocalisation ServerOutputWaitFactorId = "the relative time spent on delivering the output (from 0 to 1)"
englishResultLocalisation ServerPreemptionFactorId = "the relative time spent being preempted (from 0 to 1)"
englishResultLocalisation ActivityId = "the activity"
englishResultLocalisation ActivityInitStateId = "the initial state"
englishResultLocalisation ActivityStateId = "the current state"
englishResultLocalisation ActivityTotalUtilisationTimeId = "the total time of utilisation"
englishResultLocalisation ActivityTotalIdleTimeId = "the total idle time"
englishResultLocalisation ActivityTotalPreemptionTimeId = "the total time of preemption"
englishResultLocalisation ActivityUtilisationTimeId = "the utilisation time"
englishResultLocalisation ActivityIdleTimeId = "the idle time"
englishResultLocalisation ActivityPreemptionTimeId = "the preemption time"
englishResultLocalisation ActivityUtilisationFactorId = "the relative utilisation time (from 0 to 1)"
englishResultLocalisation ActivityIdleFactorId = "the relative idle time (от 0 до 1)"
englishResultLocalisation ActivityPreemptionFactorId = "the relative preemption time (from 0 to 1)"
englishResultLocalisation ResourceId = "the resource"
englishResultLocalisation ResourceCountId = "the current available count"
englishResultLocalisation ResourceCountStatsId = "the available count statistics"
englishResultLocalisation ResourceUtilisationCountId = "the current utilisation count"
englishResultLocalisation ResourceUtilisationCountStatsId = "the utilisation count statistics"
englishResultLocalisation ResourceQueueCountId = "the current queue length"
englishResultLocalisation ResourceQueueCountStatsId = "the queue length statistics"
englishResultLocalisation ResourceTotalWaitTimeId = "the total wait time"
englishResultLocalisation ResourceWaitTimeId = "the wait time"
englishResultLocalisation OperationId = "the operation"
englishResultLocalisation OperationTotalUtilisationTimeId = "the total time of utilisation"
englishResultLocalisation OperationTotalPreemptionTimeId = "the total time of preemption"
englishResultLocalisation OperationUtilisationTimeId = "the utilisation time"
englishResultLocalisation OperationPreemptionTimeId = "the preemption time"
englishResultLocalisation (UserDefinedResultId m) = m
englishResultLocalisation x@(LocalisedResultId m) =
  lookupResultLocalisation englishResultLocale x

-- | Lookup a localisation by the specified locale.
lookupResultLocalisation :: ResultLocale -> ResultLocalisation
lookupResultLocalisation loc (UserDefinedResultId m) = m
lookupResultLocalisation loc (LocalisedResultId m) =
  case M.lookup loc m of
    Just x -> x
    Nothing ->
      case M.lookup russianResultLocale m of
        Just x -> x
        Nothing ->
          case M.lookup englishResultLocale m of
            Just x -> x
            Nothing -> ""
lookupResultLocalisation loc resultId = russianResultLocalisation resultId
