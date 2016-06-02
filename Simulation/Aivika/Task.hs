
-- |
-- Module     : Simulation.Aivika.Task
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The 'Task' value represents a process that was already started in background.
-- We can check the completion of the task, receive notifications about changing
-- its state and even suspend an outer process awaiting the final result of the task.
-- It complements the 'Process' monad as it allows immediately continuing the main
-- computation without suspension.
--
module Simulation.Aivika.Task
       (-- * Task
        Task,
        TaskResult(..),
        taskId,
        tryGetTaskResult,
        taskResult,
        taskResultReceived,
        taskProcess,
        cancelTask,
        taskCancelled,
        -- * Running Task
        runTask,
        runTaskUsingId,
        -- * Spawning Tasks
        spawnTask,
        spawnTaskUsingId,
        spawnTaskWith,
        spawnTaskUsingIdWith,
        -- * Enqueueing Task
        enqueueTask,
        enqueueTaskUsingId,
        -- * Parallel Tasks
        taskParallelResult,
        taskParallelProcess) where

import Data.IORef
import Data.Monoid

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Cont
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.Signal

-- | The task represents a process that was already started in background.
data Task a =
  Task { taskId :: ProcessId,
         -- ^ Return an identifier for the process that was launched
         -- in background for this task.
         taskResultRef :: IORef (Maybe (TaskResult a)),
         -- ^ It contains the result of the computation.
         taskResultReceived :: Signal (TaskResult a)
         -- ^ Return a signal that notifies about receiving
         -- the result of the task.
       }

-- | Represents the result of the task.
data TaskResult a = TaskCompleted a
                    -- ^ the task was successfully completed and
                    -- it returned the specified result
                  | TaskError IOException
                    -- ^ the specified exception was raised when performing the task.
                  | TaskCancelled
                    -- ^ the task was cancelled

-- | Try to get the task result immediately without suspension.
tryGetTaskResult :: Task a -> Event (Maybe (TaskResult a))
tryGetTaskResult t =
  Event $ \p -> readIORef (taskResultRef t)

-- | Return the task result suspending the outer process if required.
taskResult :: Task a -> Process (TaskResult a)
taskResult t =
  do x <- liftIO $ readIORef (taskResultRef t)
     case x of
       Just x -> return x
       Nothing -> processAwait (taskResultReceived t)

-- | Cancel the task.
cancelTask :: Task a -> Event ()
cancelTask t =
  cancelProcessWithId (taskId t)

-- | Test whether the task was cancelled.
taskCancelled :: Task a -> Event Bool
taskCancelled t =
  processCancelled (taskId t)

-- | Create a task by the specified process and its identifier.
newTaskUsingId :: ProcessId -> Process a -> Event (Task a, Process ())
newTaskUsingId pid p =
  do r <- liftIO $ newIORef Nothing
     s <- liftSimulation newSignalSource
     let t = Task { taskId = pid,
                    taskResultRef = r,
                    taskResultReceived = publishSignal s }
     let m =
           do v <- liftIO $ newIORef TaskCancelled
              finallyProcess
                (catchProcess
                 (do a <- p
                     liftIO $ writeIORef v (TaskCompleted a))
                 (\e ->
                   liftIO $ writeIORef v (TaskError e)))
                (liftEvent $
                 do x <- liftIO $ readIORef v
                    liftIO $ writeIORef r (Just x)
                    triggerSignal s x)
     return (t, m)

-- | Run the process with the specified identifier in background and
-- return the corresponding task immediately.
runTaskUsingId :: ProcessId -> Process a -> Event (Task a)
runTaskUsingId pid p =
  do (t, m) <- newTaskUsingId pid p
     runProcessUsingId pid m
     return t

-- | Run the process in background and return the corresponding task immediately.
runTask :: Process a -> Event (Task a)
runTask p =
  do pid <- liftSimulation newProcessId
     runTaskUsingId pid p

-- | Enqueue the process that will be started at the specified time with the given
-- identifier from the event queue. It returns the corresponding task immediately.
enqueueTaskUsingId :: Double -> ProcessId -> Process a -> Event (Task a)
enqueueTaskUsingId time pid p =
  do (t, m) <- newTaskUsingId pid p
     enqueueProcessUsingId time pid m
     return t

-- | Enqueue the process that will be started at the specified time from the event queue.
-- It returns the corresponding task immediately.
enqueueTask :: Double -> Process a -> Event (Task a)
enqueueTask time p =
  do pid <- liftSimulation newProcessId
     enqueueTaskUsingId time pid p

-- | Run using the specified identifier a child process in background and return
-- immediately the corresponding task.
spawnTaskUsingId :: ProcessId -> Process a -> Process (Task a)
spawnTaskUsingId = spawnTaskUsingIdWith CancelTogether

-- | Run a child process in background and return immediately the corresponding task.
spawnTask :: Process a -> Process (Task a)
spawnTask = spawnTaskWith CancelTogether

-- | Run using the specified identifier a child process in background and return
-- immediately the corresponding task.
spawnTaskUsingIdWith :: ContCancellation -> ProcessId -> Process a -> Process (Task a)
spawnTaskUsingIdWith cancellation pid p =
  do (t, m) <- liftEvent $ newTaskUsingId pid p
     spawnProcessUsingIdWith cancellation pid m
     return t

-- | Run a child process in background and return immediately the corresponding task.
spawnTaskWith :: ContCancellation -> Process a -> Process (Task a)
spawnTaskWith cancellation p =
  do pid <- liftSimulation newProcessId
     spawnTaskUsingIdWith cancellation pid p

-- | Return an outer process that behaves like the task itself, for example,
-- when the task is cancelled if the outer process is cancelled. 
taskProcess :: Task a -> Process a
taskProcess t =
  do x <- finallyProcess
          (taskResult t)
          (do pid <- processId
              liftEvent $
                do cancelled <- processCancelled pid
                   when cancelled $
                     cancelTask t)
     case x of
       TaskCompleted a -> return a
       TaskError e -> throwProcess e
       TaskCancelled -> cancelProcess

-- | Return the result of two parallel tasks.
taskParallelResult :: Task a -> Task a -> Process (TaskResult a, Task a)
taskParallelResult t1 t2 =
  do x1 <- liftIO $ readIORef (taskResultRef t1)
     case x1 of
       Just x1 -> return (x1, t2)
       Nothing ->
         do x2 <- liftIO $ readIORef (taskResultRef t2)
            case x2 of
              Just x2 -> return (x2, t1)
              Nothing ->
                do let s1 = fmap Left $ taskResultReceived t1
                       s2 = fmap Right $ taskResultReceived t2
                   x <- processAwait $ s1 <> s2
                   case x of
                     Left x1  -> return (x1, t2)
                     Right x2 -> return (x2, t1) 

-- | Return an outer process for two parallel tasks returning the result of
-- the first finished task and the rest task in pair. 
taskParallelProcess :: Task a -> Task a -> Process (a, Task a)
taskParallelProcess t1 t2 =
  do (x, t) <-
       finallyProcess
       (taskParallelResult t1 t2)
       (do pid <- processId
           liftEvent $
             do cancelled <- processCancelled pid
                when cancelled $
                  do cancelTask t1
                     cancelTask t2)
     case x of
       TaskCompleted a -> return (a, t)
       TaskError e ->
         do liftEvent $ cancelTask t
            throwProcess e
       TaskCancelled ->
         do liftEvent $ cancelTask t
            cancelProcess
