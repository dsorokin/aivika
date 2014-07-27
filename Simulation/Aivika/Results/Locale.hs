
-- |
-- Module     : Simulation.Aivika.Results.Locale
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines locales for outputting and printing the simulation results.
--
module Simulation.Aivika.Results.Locale
       (-- * Locale Codes
        russianResultLocale,
        englishResultLocale,
        -- * Localisations
        lookupResultLocalisation,
        russianResultLocalisation,
        englishResultLocalisation) where

import qualified Data.Map as M

import Simulation.Aivika.Results

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
russianResultLocalisation FiniteQueueId = "конечная очередь"
russianResultLocalisation InfiniteQueueId = "бесконечная очередь"
russianResultLocalisation EnqueueStrategyId = "стратегия добавления элементов"
russianResultLocalisation EnqueueStoringStrategyId = "стратегия хранения элементов"
russianResultLocalisation DequeueStrategyId = "стратегия извлечения элементов"
russianResultLocalisation QueueNullId = "очередь пуста?"
russianResultLocalisation QueueFullId = "очередь заполнена?"
russianResultLocalisation QueueMaxCountId = "емкость очереди"
russianResultLocalisation QueueCountId = "размер очереди"
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
russianResultLocalisation ArrivalTimerId = "как долго обрабатываются заявки?"
russianResultLocalisation ArrivalProcessingTimeId = "время обработки заявки"
russianResultLocalisation ServerId = "сервер"
russianResultLocalisation ServerInitStateId = "начальное состояние"
russianResultLocalisation ServerStateId = "текущее состояние"
russianResultLocalisation ServerTotalInputWaitTimeId = "общее время блокировки в ожидании ввода"
russianResultLocalisation ServerTotalProcessingTimeId = "общее время, потраченное на саму обработку заданий"
russianResultLocalisation ServerTotalOutputWaitTimeId = "общее время блокировки при попытке доставить вывод"
russianResultLocalisation ServerInputWaitTimeId = "время блокировки в ожидании ввода"
russianResultLocalisation ServerProcessingTimeId = "время, потраченное на саму обработку заданий"
russianResultLocalisation ServerOutputWaitTimeId = "время блокировки при попытке доставить вывод"
russianResultLocalisation ServerInputWaitFactorId = "относительное время блокировки в ожидании ввода (от 0 до 1)"
russianResultLocalisation ServerProcessingFactorId = "относительное время, потраченное на саму обработку заданий (от 0 до 1)"
russianResultLocalisation ServerOutputWaitFactorId = "относительное время блокировки при попытке доставить вывод (от 0 до 1)"
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
englishResultLocalisation FiniteQueueId = "the finite queue"
englishResultLocalisation InfiniteQueueId = "the infinite queue"
englishResultLocalisation EnqueueStrategyId = "the enqueueing strategy"
englishResultLocalisation EnqueueStoringStrategyId = "the storing strategy"
englishResultLocalisation DequeueStrategyId = "the dequeueing strategy"
englishResultLocalisation QueueNullId = "is the queue empty?"
englishResultLocalisation QueueFullId = "is the queue full?"
englishResultLocalisation QueueMaxCountId = "the queue capacity"
englishResultLocalisation QueueCountId = "the queue size"
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
englishResultLocalisation ArrivalTimerId = "how long the arrivals are processed?"
englishResultLocalisation ArrivalProcessingTimeId = "the arrival processing time"
englishResultLocalisation ServerId = "the server"
englishResultLocalisation ServerInitStateId = "the initial state"
englishResultLocalisation ServerStateId = "the current state"
englishResultLocalisation ServerTotalInputWaitTimeId = "the total lock time when awaiting the input"
englishResultLocalisation ServerTotalProcessingTimeId = "the total time spent on actual processing the tasks"
englishResultLocalisation ServerTotalOutputWaitTimeId = "the total lock time when trying to deliver the output"
englishResultLocalisation ServerInputWaitTimeId = "the lock time when awaiting the input"
englishResultLocalisation ServerProcessingTimeId = "the time spent on actual processing the tasks"
englishResultLocalisation ServerOutputWaitTimeId = "the lock time when trying to deliver the output"
englishResultLocalisation ServerInputWaitFactorId = "the lock factor when awaiting the input (from 0 to 1)"
englishResultLocalisation ServerProcessingFactorId = "the factor spent on actual processing the tasks (from 0 to 1)"
englishResultLocalisation ServerOutputWaitFactorId = "the lock factor when trying to deliver the output (from 0 to 1)"
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
