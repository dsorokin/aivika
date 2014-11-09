
-- |
-- Module     : Simulation.Aivika.Internal.Process
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This is an internal implementation module that should never be used directly.
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
-- The characteristic property of the @Process@ type is function 'holdProcess'
-- that suspends the current process for the specified time interval.
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
        -- * Spawning Processes
        spawnProcess,
        spawnProcessUsingId,
        spawnProcessWith,
        spawnProcessUsingIdWith,
        -- * Enqueuing Process
        enqueueProcess,
        enqueueProcessUsingId,
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
        cancelProcessWithId,
        cancelProcess,
        processCancelled,
        processCancelling,
        whenCancellingProcess,
        -- * Awaiting Signal
        processAwait,
        -- * Yield of Process
        processYield,
        -- * Process Timeout
        timeoutProcess,
        timeoutProcessUsingId,
        -- * Parallelizing Processes
        processParallel,
        processParallelUsingIds,
        processParallel_,
        processParallelUsingIds_,
        -- * Exception Handling
        catchProcess,
        finallyProcess,
        throwProcess,
        -- * Utilities
        zipProcessParallel,
        zip3ProcessParallel,
        unzipProcess,
        -- * Memoizing Process
        memoProcess,
        -- * Never Ending Process
        neverProcess,
        -- * Debugging
        traceProcess) where

import Data.Maybe
import Data.IORef

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Cont
import Simulation.Aivika.Signal

-- | Represents a process identifier.
data ProcessId = 
  ProcessId { processStarted :: IORef Bool,
              processReactCont     :: IORef (Maybe (ContParams ())), 
              processCancelSource  :: ContCancellationSource,
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
       Just _  -> error "Cannot passivate the process twice: passivateProcess"

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
     let signal = processCancelling pid
     invokeEvent p $
       handleSignal_ signal $ \_ ->
       do interruptProcess pid
          reactivateProcess pid

-- | Run immediately the process. A new 'ProcessId' identifier will be
-- assigned to the process.
--            
-- To run the process at the specified time, you can use
-- the 'enqueueProcess' function.
runProcess :: Process () -> Event ()
runProcess p =
  do pid <- liftSimulation newProcessId
     runProcessUsingId pid p
             
-- | Run immediately the process with the specified identifier.
-- It will be more efficient than as you would specify the process identifier
-- with help of the 'processUsingId' combinator and then would call 'runProcess'.
--            
-- To run the process at the specified time, you can use
-- the 'enqueueProcessUsingId' function.
runProcessUsingId :: ProcessId -> Process () -> Event ()
runProcessUsingId pid p =
  do processIdPrepare pid
     runCont m cont econt ccont (processCancelSource pid) False
       where cont  = return
             econt = throwEvent
             ccont = return
             m = invokeProcess pid p

-- | Run the process in the start time immediately involving all pending
-- 'CurrentEvents' in the computation too.
runProcessInStartTime :: Process () -> Simulation ()
runProcessInStartTime = runEventInStartTime . runProcess

-- | Run the process in the start time immediately using the specified identifier
-- and involving all pending 'CurrentEvents' in the computation too.
runProcessInStartTimeUsingId :: ProcessId -> Process () -> Simulation ()
runProcessInStartTimeUsingId pid p =
  runEventInStartTime $ runProcessUsingId pid p

-- | Run the process in the final simulation time immediately involving all
-- pending 'CurrentEvents' in the computation too.
runProcessInStopTime :: Process () -> Simulation ()
runProcessInStopTime = runEventInStopTime . runProcess

-- | Run the process in the final simulation time immediately using 
-- the specified identifier and involving all pending 'CurrentEvents'
-- in the computation too.
runProcessInStopTimeUsingId :: ProcessId -> Process () -> Simulation ()
runProcessInStopTimeUsingId pid p =
  runEventInStopTime $ runProcessUsingId pid p

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

-- | Return the current process identifier.
processId :: Process ProcessId
processId = Process return

-- | Create a new process identifier.
newProcessId :: Simulation ProcessId
newProcessId =
  do x <- liftIO $ newIORef Nothing
     y <- liftIO $ newIORef False
     c <- newContCancellationSource
     i <- liftIO $ newIORef False
     z <- liftIO $ newIORef Nothing
     v <- liftIO $ newIORef 0
     return ProcessId { processStarted = y,
                        processReactCont     = x, 
                        processCancelSource  = c, 
                        processInterruptRef  = i,
                        processInterruptCont = z, 
                        processInterruptVersion = v }

-- | Cancel a process with the specified identifier, interrupting it if needed.
cancelProcessWithId :: ProcessId -> Event ()
cancelProcessWithId pid = contCancellationInitiate (processCancelSource pid)

-- | The process cancels itself.
cancelProcess :: Process a
cancelProcess =
  do pid <- processId
     liftEvent $ cancelProcessWithId pid
     throwProcess $
       (error "The process must be cancelled already: cancelProcess." :: SomeException)

-- | Test whether the process with the specified identifier was cancelled.
processCancelled :: ProcessId -> Event Bool
processCancelled pid = contCancellationInitiated (processCancelSource pid)

-- | Return a signal that notifies about cancelling the process with 
-- the specified identifier.
processCancelling :: ProcessId -> Signal ()
processCancelling pid = contCancellationInitiating (processCancelSource pid)

-- | Register a handler that will be invoked in case of cancelling the current process.
whenCancellingProcess :: Event () -> Process ()
whenCancellingProcess h =
  Process $ \pid ->
  liftEvent $
  handleSignal_ (processCancelling pid) $ \() -> h

instance Eq ProcessId where
  x == y = processReactCont x == processReactCont y    -- for the references are unique

instance Monad Process where
  return  = returnP
  m >>= k = bindP m k

instance Functor Process where
  fmap = liftM

instance Applicative Process where
  pure = return
  (<*>) = ap

instance ParameterLift Process where
  liftParameter = liftPP

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

liftPP :: Parameter a -> Process a
{-# INLINE liftPP #-}
liftPP m = Process $ \pid -> liftParameter m

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
catchProcess :: Exception e => Process a -> (e -> Process a) -> Process a
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
--
-- By some reason, an exception raised with help of the standard 'throw' function
-- is not handled properly within 'Process' computation, altough it will be still handled 
-- if it will be wrapped in the 'IO' monad. Therefore, you should use specialised
-- functions like the stated one that use the 'throw' function but within the 'IO' computation,
-- which allows already handling the exception.
throwProcess :: Exception e => e -> Process a
throwProcess = liftIO . throw

-- | Execute the specified computations in parallel within
-- the current computation and return their results. The cancellation
-- of any of the nested computations affects the current computation.
-- The exception raised in any of the nested computations is propagated
-- to the current computation as well.
--
-- Here word @parallel@ literally means that the computations are
-- actually executed on a single operating system thread but
-- they are processed simultaneously by the event queue.
--
-- New 'ProcessId' identifiers will be assigned to the started processes.
processParallel :: [Process a] -> Process [a]
processParallel xs =
  liftSimulation (processParallelCreateIds xs) >>= processParallelUsingIds 

-- | Like 'processParallel' but allows specifying the process identifiers.
-- It will be more efficient than as you would specify the process identifiers
-- with help of the 'processUsingId' combinator and then would call 'processParallel'.
processParallelUsingIds :: [(ProcessId, Process a)] -> Process [a]
processParallelUsingIds xs =
  Process $ \pid ->
  do liftEvent $ processParallelPrepare xs
     contParallel $
       flip map xs $ \(pid, m) ->
       (invokeProcess pid m, processCancelSource pid)

-- | Like 'processParallel' but ignores the result.
processParallel_ :: [Process a] -> Process ()
processParallel_ xs =
  liftSimulation (processParallelCreateIds xs) >>= processParallelUsingIds_ 

-- | Like 'processParallelUsingIds' but ignores the result.
processParallelUsingIds_ :: [(ProcessId, Process a)] -> Process ()
processParallelUsingIds_ xs =
  Process $ \pid ->
  do liftEvent $ processParallelPrepare xs
     contParallel_ $
       flip map xs $ \(pid, m) ->
       (invokeProcess pid m, processCancelSource pid)

-- | Create the new process identifiers.
processParallelCreateIds :: [Process a] -> Simulation [(ProcessId, Process a)]
processParallelCreateIds xs =
  do pids <- liftSimulation $ forM xs $ const newProcessId
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
     rerunCont (invokeProcess pid x) (processCancelSource pid)

-- | Spawn the child process. In case of cancelling one of the processes,
-- other process will be cancelled too.
spawnProcess :: Process () -> Process ()
spawnProcess = spawnProcessWith CancelTogether

-- | Spawn the child process with the specified process identifier.
-- In case of cancelling one of the processes, other process will
-- be cancelled too.
spawnProcessUsingId :: ProcessId -> Process () -> Process ()
spawnProcessUsingId = spawnProcessUsingIdWith CancelTogether

-- | Spawn the child process specifying how the child and parent processes
-- should be cancelled in case of need.
spawnProcessWith :: ContCancellation -> Process () -> Process ()
spawnProcessWith cancellation x =
  do pid <- liftSimulation newProcessId
     spawnProcessUsingIdWith cancellation pid x

-- | Spawn the child process specifying how the child and parent processes
-- should be cancelled in case of need.
spawnProcessUsingIdWith :: ContCancellation -> ProcessId -> Process () -> Process ()
spawnProcessUsingIdWith cancellation pid x =
  Process $ \pid' ->
  do liftEvent $ processIdPrepare pid
     spawnCont cancellation (invokeProcess pid x) (processCancelSource pid)

-- | Await the signal.
processAwait :: Signal a -> Process a
processAwait signal =
  Process $ \pid -> contAwait signal

-- | The result of memoization.
data MemoResult a = MemoComputed a
                  | MemoError IOException
                  | MemoCancelled

-- | Memoize the process so that it would always return the same value
-- within the simulation run.
memoProcess :: Process a -> Simulation (Process a)
memoProcess x =
  do started  <- liftIO $ newIORef False
     computed <- newSignalSource
     value    <- liftIO $ newIORef Nothing
     let result =
           do Just x <- liftIO $ readIORef value
              case x of
                MemoComputed a -> return a
                MemoError e    -> throwProcess e
                MemoCancelled  -> cancelProcess
     return $
       do v <- liftIO $ readIORef value
          case v of
            Just _ -> result
            Nothing ->
              do f <- liftIO $ readIORef started
                 case f of
                   True ->
                     do processAwait $ publishSignal computed
                        result
                   False ->
                     do liftIO $ writeIORef started True
                        r <- liftIO $ newIORef MemoCancelled
                        finallyProcess
                          (catchProcess
                           (do a <- x    -- compute only once!
                               liftIO $ writeIORef r (MemoComputed a))
                           (\e ->
                             liftIO $ writeIORef r (MemoError e)))
                          (liftEvent $
                           do liftIO $
                                do x <- readIORef r
                                   writeIORef value (Just x)
                              triggerSignal computed ())
                        result

-- | Zip two parallel processes waiting for the both.
zipProcessParallel :: Process a -> Process b -> Process (a, b)
zipProcessParallel x y =
  do [Left a, Right b] <- processParallel [fmap Left x, fmap Right y]
     return (a, b)

-- | Zip three parallel processes waiting for their results.
zip3ProcessParallel :: Process a -> Process b -> Process c -> Process (a, b, c)
zip3ProcessParallel x y z =
  do [Left a,
      Right (Left b),
      Right (Right c)] <-
       processParallel [fmap Left x,
                        fmap (Right . Left) y,
                        fmap (Right . Right) z]
     return (a, b, c)

-- | Unzip the process using memoization so that the both returned
-- processes could be applied independently, although they will refer
-- to the same pair of values.
unzipProcess :: Process (a, b) -> Simulation (Process a, Process b)
unzipProcess xy =
  do xy' <- memoProcess xy
     return (fmap fst xy', fmap snd xy')

-- | Try to run the child process within the specified timeout.
-- If the process will finish successfully within this time interval then
-- the result wrapped in 'Just' will be returned; otherwise, the child process
-- will be cancelled and 'Nothing' will be returned.
--
-- If an exception is raised in the child process then it is propagated to
-- the parent computation as well.
--
-- A cancellation of the child process doesn't lead to cancelling the parent process.
-- Then 'Nothing' is returned within the computation.
timeoutProcess :: Double -> Process a -> Process (Maybe a)
timeoutProcess timeout p =
  do pid <- liftSimulation newProcessId
     timeoutProcessUsingId timeout pid p

-- | Try to run the child process with the given identifier within the specified timeout.
-- If the process will finish successfully within this time interval then
-- the result wrapped in 'Just' will be returned; otherwise, the child process
-- will be cancelled and 'Nothing' will be returned.
--
-- If an exception is raised in the child process then it is propagated to
-- the parent computation as well.
--
-- A cancellation of the child process doesn't lead to cancelling the parent process.
-- Then 'Nothing' is returned within the computation.
timeoutProcessUsingId :: Double -> ProcessId -> Process a -> Process (Maybe a)
timeoutProcessUsingId timeout pid p =
  do s <- liftSimulation newSignalSource
     timeoutPid <- liftSimulation newProcessId
     spawnProcessUsingIdWith CancelChildAfterParent timeoutPid $
       finallyProcess
       (holdProcess timeout)
       (liftEvent $
        cancelProcessWithId pid)
     spawnProcessUsingIdWith CancelChildAfterParent pid $
       do r <- liftIO $ newIORef Nothing
          finallyProcess
            (catchProcess
             (do a <- p
                 liftIO $ writeIORef r $ Just (Right a))
             (\e ->
               liftIO $ writeIORef r $ Just (Left e)))
            (liftEvent $
             do x <- liftIO $ readIORef r
                triggerSignal s x)
     x <- processAwait $ publishSignal s
     case x of
       Nothing -> return Nothing
       Just (Right a) -> return (Just a)
       Just (Left (SomeException e)) -> throwProcess e

-- | Yield to allow other 'Process' and 'Event' computations to run
-- at the current simulation time point.
processYield :: Process ()
processYield =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  invokeEvent p $
  enqueueEvent (pointTime p) $
  resumeCont c ()

-- | A computation that never computes the result. It behaves like a black hole for
-- the discontinuous process, although such a process can still be canceled outside
-- (see 'cancelProcessWithId'), but then only its finalization parts (see 'finallyProcess')
-- will be called, usually, to release the resources acquired before.
neverProcess :: Process a
neverProcess =
  Process $ \pid ->
  Cont $ \c ->
  let signal = processCancelling pid
  in handleSignal_ signal $ \_ ->
     resumeCont c $ error "It must never be computed: neverProcess"

-- | Show the debug message with the current simulation time.
traceProcess :: String -> Process a -> Process a
traceProcess message m =
  Process $ \pid ->
  traceCont message $
  invokeProcess pid m
