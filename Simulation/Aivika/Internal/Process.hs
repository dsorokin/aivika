
-- |
-- Module     : Simulation.Aivika.Internal.Process
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- A value in the 'Process' monad represents a discontinuous process that 
-- can suspend in any simulation time point and then resume later in the same 
-- or another time point. 
-- 
-- The process of this type can involve the 'Event', 'Dynamics' and 'Simulation'
-- computations. Moreover, a value in the @Process@ monad can be run within
-- the @Event@ computation.
--
-- A value of the 'ProcessId' type is just an identifier of such a process.
--
module Simulation.Aivika.Internal.Process
       (-- * Process Monad
        ProcessId,
        Process(..),
        ProcessLift(..),
        invokeProcess,
        -- * Running Process
        runProcess,
        runProcessUsingId,
        runProcessInStartTime,
        runProcessInStartTimeUsingId,
        runProcessInStopTime,
        runProcessInStopTimeUsingId,
        -- * Forking Process
        forkProcess,
        forkProcessUsingId,
        -- * Enqueuing Process
        enqueueProcess,
        enqueueProcessUsingId,
        enqueueProcessWithStartTime,
        enqueueProcessWithStartTimeUsingId,
        enqueueProcessWithStopTime,
        enqueueProcessWithStopTimeUsingId,
        -- * Creating Process Identifier
        newProcessId,
        processId,
        processUsingId,
        -- * Holding, Interrupting, Passivating and Canceling Process
        holdProcess,
        interruptProcess,
        processInterrupted,
        passivateProcess,
        processPassive,
        reactivateProcess,
        cancelProcess,
        processCanceled,
        -- * Parallelizing Processes
        processParallel,
        processParallelUsingIds,
        processParallel_,
        processParallelUsingIds_,
        -- * Exception Handling
        catchProcess,
        finallyProcess,
        throwProcess) where

import Data.Maybe
import Data.IORef
import Control.Exception (IOException, throw)
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Cont
import Simulation.Aivika.Internal.Signal

-- | Represents a process identifier.
data ProcessId = 
  ProcessId { processStarted :: IORef Bool,
              processReactCont     :: IORef (Maybe (ContParams ())), 
              processCancel        :: ContCancellation,
              processInterruptRef  :: IORef Bool, 
              processInterruptCont :: IORef (Maybe (ContParams ())), 
              processInterruptVersion :: IORef Int }

-- | Specifies a discontinuous process that can suspend at any time
-- and then resume later.
newtype Process a = Process (ProcessId -> Cont a)

-- | A type class to lift the 'Process' computation to other computations.
class ProcessLift m where
  
  -- | Lift the specified 'Process' computation to another computation.
  liftProcess :: Process a -> m a

instance ProcessLift Process where
  liftProcess = id

-- | Invoke the process computation.
invokeProcess :: ProcessId -> Process a -> Cont a
{-# INLINE invokeProcess #-}
invokeProcess pid (Process m) = m pid

-- | Hold the process for the specified time period.
holdProcess :: Double -> Process ()
holdProcess dt =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let x = processInterruptCont pid
     writeIORef x $ Just c
     writeIORef (processInterruptRef pid) False
     v <- readIORef (processInterruptVersion pid)
     invokeEvent p $
       enqueueEvent (pointTime p + dt) $
       Event $ \p ->
       do v' <- readIORef (processInterruptVersion pid)
          when (v == v') $ 
            do writeIORef x Nothing
               invokeEvent p $ resumeCont c ()

-- | Interrupt a process with the specified identifier if the process
-- is held by computation 'holdProcess'.
interruptProcess :: ProcessId -> Event ()
interruptProcess pid =
  Event $ \p ->
  do let x = processInterruptCont pid
     a <- readIORef x
     case a of
       Nothing -> return ()
       Just c ->
         do writeIORef x Nothing
            writeIORef (processInterruptRef pid) True
            modifyIORef (processInterruptVersion pid) $ (+) 1
            invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()
            
-- | Test whether the process with the specified identifier was interrupted.
processInterrupted :: ProcessId -> Event Bool
processInterrupted pid =
  Event $ \p ->
  readIORef (processInterruptRef pid)

-- | Passivate the process.
passivateProcess :: Process ()
passivateProcess =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let x = processReactCont pid
     a <- readIORef x
     case a of
       Nothing -> writeIORef x $ Just c
       Just _  -> error "Cannot passivate the process twice: passivate"

-- | Test whether the process with the specified identifier is passivated.
processPassive :: ProcessId -> Event Bool
processPassive pid =
  Event $ \p ->
  do let x = processReactCont pid
     a <- readIORef x
     return $ isJust a

-- | Reactivate a process with the specified identifier.
reactivateProcess :: ProcessId -> Event ()
reactivateProcess pid =
  Event $ \p ->
  do let x = processReactCont pid
     a <- readIORef x
     case a of
       Nothing -> 
         return ()
       Just c ->
         do writeIORef x Nothing
            invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()

-- | Prepare the processes identifier for running.
processIdPrepare :: ProcessId -> Event ()
processIdPrepare pid =
  Event $ \p ->
  do y <- readIORef (processStarted pid)
     if y
       then error $
            "Another process with the specified identifier " ++
            "has been started already: processIdPrepare"
       else writeIORef (processStarted pid) True
     let signal = (contCancellationInitiating $ processCancel pid)
     invokeEvent p $
       handleSignal_ signal $ \_ -> interruptProcess pid

-- | Start immediately the process. A new 'ProcessId' identifier will be
-- assigned to the process.
--            
-- To run the process at the specified time, you can use
-- the 'enqueueProcess' function.
runProcess :: Process () -> Event ()
runProcess p =
  do pid <- liftSimulation newProcessId
     runProcessUsingId pid p
             
-- | Start immediately the process with the specified identifier.
-- It will be more efficient than as you would specify the process identifier
-- with help of the 'processUsingId' combinator and then would call 'runProcess'.
--            
-- To run the process at the specified time, you can use
-- the 'enqueueProcessUsingId' function.
runProcessUsingId :: ProcessId -> Process () -> Event ()
runProcessUsingId pid p =
  do processIdPrepare pid
     runCont m cont econt ccont (processCancel pid) False
       where cont  = return
             econt = throwEvent
             ccont = return
             m = invokeProcess pid p

-- | Start the process in the start time immediately.
runProcessInStartTime :: EventProcessing -> Process () -> Simulation ()
runProcessInStartTime processing p =
  runEventInStartTime processing $ runProcess p

-- | Start the process in the start time immediately using the specified identifier.
runProcessInStartTimeUsingId :: EventProcessing -> ProcessId -> Process () -> Simulation ()
runProcessInStartTimeUsingId processing pid p =
  runEventInStartTime processing $ runProcessUsingId pid p

-- | Start the process in the stop time immediately.
runProcessInStopTime :: EventProcessing -> Process () -> Simulation ()
runProcessInStopTime processing p =
  runEventInStopTime processing $ runProcess p

-- | Start the process in the stop time immediately using the specified identifier.
runProcessInStopTimeUsingId :: EventProcessing -> ProcessId -> Process () -> Simulation ()
runProcessInStopTimeUsingId processing pid p =
  runEventInStopTime processing $ runProcessUsingId pid p

-- | Fork the process in parallel in the sense that the process will be executed
-- by the event queue simultaneously on a single operating system thread.
-- A new 'ProcessId' identifier will be assigned to the forked process.
forkProcess :: Process () -> Event ()
forkProcess p =
  enqueueEventWithCurrentTime $ runProcess p

-- | Fork the process in parallel using the specified identifier in the sense
-- that the process will be executed by the event queue simultaneously on a single
-- operating system thread. Calling this function is more efficient than
-- as you would specify the process identifier with help of the 'processUsingId'
-- combinator and then would call 'forkProcess'.
forkProcessUsingId :: ProcessId -> Process () -> Event ()
forkProcessUsingId pid p =
  enqueueEventWithCurrentTime $ runProcessUsingId pid p

-- | Enqueue the process that will be then started at the specified time
-- from the event queue.
enqueueProcess :: Double -> Process () -> Event ()
enqueueProcess t p =
  enqueueEvent t $ runProcess p

-- | Enqueue the process that will be then started at the specified time
-- from the event queue.
enqueueProcessUsingId :: Double -> ProcessId -> Process () -> Event ()
enqueueProcessUsingId t pid p =
  enqueueEvent t $ runProcessUsingId pid p

-- | Enqueue the process that will be then started in the start time
-- from the event queue.
enqueueProcessWithStartTime :: Process () -> Event ()
enqueueProcessWithStartTime p =
  enqueueEventWithStartTime $ runProcess p

-- | Enqueue the process that will be then started in the start time
-- from the event queue.
enqueueProcessWithStartTimeUsingId :: ProcessId -> Process () -> Event ()
enqueueProcessWithStartTimeUsingId pid p =
  enqueueEventWithStartTime $ runProcessUsingId pid p

-- | Enqueue the process that will be then started in the stop time
-- from the event queue.
enqueueProcessWithStopTime :: ProcessId -> Process () -> Event ()
enqueueProcessWithStopTime pid p =
  enqueueEventWithStopTime $ runProcess p

-- | Enqueue the process that will be then started in the stop time
-- from the event queue.
enqueueProcessWithStopTimeUsingId :: ProcessId -> Process () -> Event ()
enqueueProcessWithStopTimeUsingId pid p =
  enqueueEventWithStopTime $ runProcessUsingId pid p

-- | Return the current process identifier.
processId :: Process ProcessId
processId = Process return

-- | Create a new process identifier.
newProcessId :: Simulation ProcessId
newProcessId =
  do x <- liftIO $ newIORef Nothing
     y <- liftIO $ newIORef False
     c <- newContCancellation
     i <- liftIO $ newIORef False
     z <- liftIO $ newIORef Nothing
     v <- liftIO $ newIORef 0
     return ProcessId { processStarted = y,
                        processReactCont     = x, 
                        processCancel        = c, 
                        processInterruptRef  = i,
                        processInterruptCont = z, 
                        processInterruptVersion = v }

-- | Cancel a process with the specified identifier, interrupting it if needed.
cancelProcess :: ProcessId -> Event ()
cancelProcess pid = contCancellationInitiate (processCancel pid)

-- | Test whether the process with the specified identifier was canceled.
processCanceled :: ProcessId -> Event Bool
processCanceled pid = contCancellationInitiated (processCancel pid)

instance Eq ProcessId where
  x == y = processReactCont x == processReactCont y    -- for the references are unique

instance Monad Process where
  return  = returnP
  m >>= k = bindP m k

instance Functor Process where
  fmap = liftM

instance SimulationLift Process where
  liftSimulation = liftSP
  
instance DynamicsLift Process where
  liftDynamics = liftDP
  
instance EventLift Process where
  liftEvent = liftEP
  
instance MonadIO Process where
  liftIO = liftIOP
  
returnP :: a -> Process a
{-# INLINE returnP #-}
returnP a = Process $ \pid -> return a

bindP :: Process a -> (a -> Process b) -> Process b
{-# INLINE bindP #-}
bindP (Process m) k = 
  Process $ \pid -> 
  do a <- m pid
     let Process m' = k a
     m' pid

liftSP :: Simulation a -> Process a
{-# INLINE liftSP #-}
liftSP m = Process $ \pid -> liftSimulation m

liftDP :: Dynamics a -> Process a
{-# INLINE liftDP #-}
liftDP m = Process $ \pid -> liftDynamics m

liftEP :: Event a -> Process a
{-# INLINE liftEP #-}
liftEP m = Process $ \pid -> liftEvent m

liftIOP :: IO a -> Process a
{-# INLINE liftIOP #-}
liftIOP m = Process $ \pid -> liftIO m

-- | Exception handling within 'Process' computations.
catchProcess :: Process a -> (IOException -> Process a) -> Process a
catchProcess (Process m) h =
  Process $ \pid ->
  catchCont (m pid) $ \e ->
  let Process m' = h e in m' pid
                           
-- | A computation with finalization part.
finallyProcess :: Process a -> Process b -> Process a
finallyProcess (Process m) (Process m') =
  Process $ \pid ->
  finallyCont (m pid) (m' pid)

-- | Throw the exception with the further exception handling.
-- By some reasons, the standard 'throw' function per se is not handled 
-- properly within 'Process' computations, although it will be still 
-- handled if it will be hidden under the 'liftIO' function. The problem 
-- arises namely with the @throw@ function, not 'IO' computations.
throwProcess :: IOException -> Process a
throwProcess = liftIO . throw

-- | Execute the specified computations in parallel within
-- the current computation and return their results. The cancellation
-- of any of the nested computations affects the current computation.
-- The exception raised in any of the nested computations is propogated
-- to the current computation as well.
--
-- Here word @parallel@ literally means that the computations are
-- actually executed on a single operating system thread but
-- they are processed simultaneously by the event queue.
--
-- New 'ProcessId' identifiers will be assigned to the started processes.
processParallel :: [Process a] -> Process [a]
processParallel xs =
  processParallelCreateIds xs >>= processParallelUsingIds 

-- | Like 'processParallel' but allows specifying the process identifiers.
-- It will be more efficient than as you would specify the process identifiers
-- with help of the 'processUsingId' combinator and then would call 'processParallel'.
processParallelUsingIds :: [(ProcessId, Process a)] -> Process [a]
processParallelUsingIds xs =
  Process $ \pid ->
  do liftEvent $ processParallelPrepare xs
     contParallel $
       flip map xs $ \(pid, m) ->
       (invokeProcess pid m, processCancel pid)

-- | Like 'processParallel' but ignores the result.
processParallel_ :: [Process a] -> Process ()
processParallel_ xs =
  processParallelCreateIds xs >>= processParallelUsingIds_ 

-- | Like 'processParallelUsingIds' but ignores the result.
processParallelUsingIds_ :: [(ProcessId, Process a)] -> Process ()
processParallelUsingIds_ xs =
  Process $ \pid ->
  do liftEvent $ processParallelPrepare xs
     contParallel_ $
       flip map xs $ \(pid, m) ->
       (invokeProcess pid m, processCancel pid)

-- | Create the new process identifiers.
processParallelCreateIds :: [Process a] -> Process [(ProcessId, Process a)]
processParallelCreateIds xs =
  do pid  <- processId
     pids <- liftSimulation $ forM xs $ const newProcessId
     return $ zip pids xs

-- | Prepare the processes for parallel execution.
processParallelPrepare :: [(ProcessId, Process a)] -> Event ()
processParallelPrepare xs =
  Event $ \p ->
  forM_ xs $ invokeEvent p . processIdPrepare . fst

-- | Allow calling the process with the specified identifier.
-- It creates a nested process when canceling any of two, or raising an
-- @IO@ exception in any of the both, affects the 'Process' computation.
--
-- At the same time, the interruption has no such effect as it requires
-- explicit specifying the 'ProcessId' identifier of the nested process itself,
-- that is the nested process cannot be interrupted using only the parent
-- process identifier.
processUsingId :: ProcessId -> Process a -> Process a
processUsingId pid x =
  Process $ \pid' ->
  do liftEvent $ processIdPrepare pid
     rerunCont (invokeProcess pid x) (processCancel pid)
