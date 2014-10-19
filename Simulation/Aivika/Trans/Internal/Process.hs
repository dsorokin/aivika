
-- |
-- Module     : Simulation.Aivika.Trans.Internal.Process
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
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
module Simulation.Aivika.Trans.Internal.Process
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
        neverProcess) where

import Data.Maybe

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Internal.Signal

-- | Represents a process identifier.
data ProcessId m = 
  ProcessId { processStarted :: ProtoRef m Bool,
              processMarker  :: SessionMarker m,
              processReactCont     :: ProtoRef m (Maybe (ContParams m ())), 
              processCancelSource  :: ContCancellationSource m,
              processInterruptRef  :: ProtoRef m Bool, 
              processInterruptCont :: ProtoRef m (Maybe (ContParams m ())), 
              processInterruptVersion :: ProtoRef m Int }

-- | Specifies a discontinuous process that can suspend at any time
-- and then resume later.
newtype Process m a = Process (ProcessId m -> Cont m a)

-- | A type class to lift the 'Process' computation into other computations.
class ProcessLift t where
  
  -- | Lift the specified 'Process' computation into another computation.
  liftProcess :: Comp m => Process m a -> t m a

-- | Invoke the process computation.
invokeProcess :: ProcessId m -> Process m a -> Cont m a
{-# INLINE invokeProcess #-}
invokeProcess pid (Process m) = m pid

-- | Hold the process for the specified time period.
holdProcess :: Comp m => Double -> Process m ()
{-# INLINABLE holdProcess #-}
holdProcess dt =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let x = processInterruptCont pid
     writeProtoRef x $ Just c
     writeProtoRef (processInterruptRef pid) False
     v <- readProtoRef (processInterruptVersion pid)
     invokeEvent p $
       enqueueEvent (pointTime p + dt) $
       Event $ \p ->
       do v' <- readProtoRef (processInterruptVersion pid)
          when (v == v') $ 
            do writeProtoRef x Nothing
               invokeEvent p $ resumeCont c ()

-- | Interrupt a process with the specified identifier if the process
-- is held by computation 'holdProcess'.
interruptProcess :: Comp m => ProcessId m -> Event m ()
{-# INLINABLE interruptProcess #-}
interruptProcess pid =
  Event $ \p ->
  do let x = processInterruptCont pid
     a <- readProtoRef x
     case a of
       Nothing -> return ()
       Just c ->
         do writeProtoRef x Nothing
            writeProtoRef (processInterruptRef pid) True
            modifyProtoRef (processInterruptVersion pid) $ (+) 1
            invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()
            
-- | Test whether the process with the specified identifier was interrupted.
processInterrupted :: Comp m => ProcessId m -> Event m Bool
{-# INLINABLE processInterrupted #-}
processInterrupted pid =
  Event $ \p ->
  readProtoRef (processInterruptRef pid)

-- | Passivate the process.
passivateProcess :: Comp m => Process m ()
{-# INLINABLE passivateProcess #-}
passivateProcess =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let x = processReactCont pid
     a <- readProtoRef x
     case a of
       Nothing -> writeProtoRef x $ Just c
       Just _  -> error "Cannot passivate the process twice: passivateProcess"

-- | Test whether the process with the specified identifier is passivated.
processPassive :: Comp m => ProcessId m -> Event m Bool
{-# INLINABLE processPassive #-}
processPassive pid =
  Event $ \p ->
  do let x = processReactCont pid
     a <- readProtoRef x
     return $ isJust a

-- | Reactivate a process with the specified identifier.
reactivateProcess :: Comp m => ProcessId m -> Event m ()
{-# INLINABLE reactivateProcess #-}
reactivateProcess pid =
  Event $ \p ->
  do let x = processReactCont pid
     a <- readProtoRef x
     case a of
       Nothing -> 
         return ()
       Just c ->
         do writeProtoRef x Nothing
            invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()

-- | Prepare the processes identifier for running.
processIdPrepare :: Comp m => ProcessId m -> Event m ()
{-# INLINABLE processIdPrepare #-}
processIdPrepare pid =
  Event $ \p ->
  do y <- readProtoRef (processStarted pid)
     if y
       then error $
            "Another process with the specified identifier " ++
            "has been started already: processIdPrepare"
       else writeProtoRef (processStarted pid) True
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
runProcess :: Comp m => Process m () -> Event m ()
{-# INLINABLE runProcess #-}
runProcess p =
  do pid <- liftSimulation newProcessId
     runProcessUsingId pid p
             
-- | Run immediately the process with the specified identifier.
-- It will be more efficient than as you would specify the process identifier
-- with help of the 'processUsingId' combinator and then would call 'runProcess'.
--            
-- To run the process at the specified time, you can use
-- the 'enqueueProcessUsingId' function.
runProcessUsingId :: Comp m => ProcessId m -> Process m () -> Event m ()
{-# INLINABLE runProcessUsingId #-}
runProcessUsingId pid p =
  do processIdPrepare pid
     runCont m cont econt ccont (processCancelSource pid) False
       where cont  = return
             econt = throwEvent
             ccont = return
             m = invokeProcess pid p

-- | Run the process in the start time immediately involving all pending
-- 'CurrentEvents' in the computation too.
runProcessInStartTime :: Comp m => Process m () -> Simulation m ()
{-# INLINABLE runProcessInStartTime #-}
runProcessInStartTime = runEventInStartTime . runProcess

-- | Run the process in the start time immediately using the specified identifier
-- and involving all pending 'CurrentEvents' in the computation too.
runProcessInStartTimeUsingId :: Comp m => ProcessId m -> Process m () -> Simulation m ()
{-# INLINABLE runProcessInStartTimeUsingId #-}
runProcessInStartTimeUsingId pid p =
  runEventInStartTime $ runProcessUsingId pid p

-- | Run the process in the final simulation time immediately involving all
-- pending 'CurrentEvents' in the computation too.
runProcessInStopTime :: Comp m => Process m () -> Simulation m ()
{-# INLINABLE runProcessInStopTime #-}
runProcessInStopTime = runEventInStopTime . runProcess

-- | Run the process in the final simulation time immediately using 
-- the specified identifier and involving all pending 'CurrentEvents'
-- in the computation too.
runProcessInStopTimeUsingId :: Comp m => ProcessId m -> Process m () -> Simulation m ()
{-# INLINABLE runProcessInStopTimeUsingId #-}
runProcessInStopTimeUsingId pid p =
  runEventInStopTime $ runProcessUsingId pid p

-- | Enqueue the process that will be then started at the specified time
-- from the event queue.
enqueueProcess :: Comp m => Double -> Process m () -> Event m ()
{-# INLINABLE enqueueProcess #-}
enqueueProcess t p =
  enqueueEvent t $ runProcess p

-- | Enqueue the process that will be then started at the specified time
-- from the event queue.
enqueueProcessUsingId :: Comp m => Double -> ProcessId m -> Process m () -> Event m ()
{-# INLINABLE enqueueProcessUsingId #-}
enqueueProcessUsingId t pid p =
  enqueueEvent t $ runProcessUsingId pid p

-- | Return the current process identifier.
processId :: Comp m => Process m (ProcessId m)
{-# INLINE processId #-}
processId = Process return

-- | Create a new process identifier.
newProcessId :: Comp m => Simulation m (ProcessId m)
{-# INLINABLE newProcessId #-}
newProcessId =
  Simulation $ \r ->
  do let s = runSession r
     m <- newSessionMarker s       
     x <- newProtoRef s Nothing
     y <- newProtoRef s False
     c <- invokeSimulation r newContCancellationSource
     i <- newProtoRef s False
     z <- newProtoRef s Nothing
     v <- newProtoRef s 0
     return ProcessId { processStarted = y,
                        processMarker  = m,
                        processReactCont     = x, 
                        processCancelSource  = c, 
                        processInterruptRef  = i,
                        processInterruptCont = z, 
                        processInterruptVersion = v }

-- | Cancel a process with the specified identifier, interrupting it if needed.
cancelProcessWithId :: Comp m => ProcessId m -> Event m ()
{-# INLINABLE cancelProcessWithId #-}
cancelProcessWithId pid = contCancellationInitiate (processCancelSource pid)

-- | The process cancels itself.
cancelProcess :: (Comp m, MonadIO m) => Process m a
{-# INLINABLE cancelProcess #-}
cancelProcess =
  do pid <- processId
     liftEvent $ cancelProcessWithId pid
     error "The process must be cancelled already: cancelProcess."

-- | Test whether the process with the specified identifier was cancelled.
processCancelled :: Comp m => ProcessId m -> Event m Bool
{-# INLINABLE processCancelled #-}
processCancelled pid = contCancellationInitiated (processCancelSource pid)

-- | Return a signal that notifies about cancelling the process with 
-- the specified identifier.
processCancelling :: ProcessId m -> Signal m ()
{-# INLINABLE processCancelling #-}
processCancelling pid = contCancellationInitiating (processCancelSource pid)

-- | Register a handler that will be invoked in case of cancelling the current process.
whenCancellingProcess :: Comp m => Event m () -> Process m ()
{-# INLINABLE whenCancellingProcess #-}
whenCancellingProcess h =
  Process $ \pid ->
  liftEvent $
  handleSignal_ (processCancelling pid) $ \() -> h

instance Comp m => Eq (ProcessId m) where

  {-# INLINE (==) #-}
  x == y = processMarker x == processMarker y

instance Comp m => Monad (Process m) where

  {-# INLINE return #-}
  return a = Process $ \pid -> return a

  {-# INLINE (>>=) #-}
  (Process m) >>= k =
    Process $ \pid -> 
    do a <- m pid
       let Process m' = k a
       m' pid

instance CompTrans Process where

  {-# INLINE liftComp #-}
  liftComp = Process . const . liftComp

instance Comp m => Functor (Process m) where
  
  {-# INLINE fmap #-}
  fmap f (Process x) = Process $ \pid -> fmap f $ x pid

instance Comp m => Applicative (Process m) where
  
  {-# INLINE pure #-}
  pure = Process . const . pure
  
  {-# INLINE (<*>) #-}
  (Process x) <*> (Process y) = Process $ \pid -> x pid <*> y pid

instance (Comp m, MonadIO m) => MonadIO (Process m) where
  
  {-# INLINE liftIO #-}
  liftIO = Process . const . liftIO

instance ParameterLift Process where

  {-# INLINE liftParameter #-}
  liftParameter = Process . const . liftParameter

instance SimulationLift Process where

  {-# INLINE liftSimulation #-}
  liftSimulation = Process . const . liftSimulation
  
instance DynamicsLift Process where

  {-# INLINE liftDynamics #-}
  liftDynamics = Process . const . liftDynamics
  
instance EventLift Process where

  {-# INLINE liftEvent #-}
  liftEvent = Process . const . liftEvent

instance ProcessLift Process where

  {-# INLINE liftProcess #-}
  liftProcess = id

-- | Exception handling within 'Process' computations.
catchProcess :: (Comp m, Exception e) => Process m a -> (e -> Process m a) -> Process m a
{-# INLINABLE catchProcess #-}
catchProcess (Process m) h =
  Process $ \pid ->
  catchCont (m pid) $ \e ->
  let Process m' = h e in m' pid
                           
-- | A computation with finalization part.
finallyProcess :: Comp m => Process m a -> Process m b -> Process m a
{-# INLINABLE finallyProcess #-}
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
throwProcess :: (Comp m, Exception e) => e -> Process m a
{-# INLINABLE throwProcess #-}
throwProcess = Process . const . throwCont

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
processParallel :: Comp m => [Process m a] -> Process m [a]
{-# INLINABLE processParallel #-}
processParallel xs =
  liftSimulation (processParallelCreateIds xs) >>= processParallelUsingIds 

-- | Like 'processParallel' but allows specifying the process identifiers.
-- It will be more efficient than as you would specify the process identifiers
-- with help of the 'processUsingId' combinator and then would call 'processParallel'.
processParallelUsingIds :: Comp m => [(ProcessId m, Process m a)] -> Process m [a]
{-# INLINABLE processParallelUsingIds #-}
processParallelUsingIds xs =
  Process $ \pid ->
  do liftEvent $ processParallelPrepare xs
     contParallel $
       flip map xs $ \(pid, m) ->
       (invokeProcess pid m, processCancelSource pid)

-- | Like 'processParallel' but ignores the result.
processParallel_ :: Comp m => [Process m a] -> Process m ()
{-# INLINABLE processParallel_ #-}
processParallel_ xs =
  liftSimulation (processParallelCreateIds xs) >>= processParallelUsingIds_ 

-- | Like 'processParallelUsingIds' but ignores the result.
processParallelUsingIds_ :: Comp m => [(ProcessId m, Process m a)] -> Process m ()
{-# INLINABLE processParallelUsingIds_ #-}
processParallelUsingIds_ xs =
  Process $ \pid ->
  do liftEvent $ processParallelPrepare xs
     contParallel_ $
       flip map xs $ \(pid, m) ->
       (invokeProcess pid m, processCancelSource pid)

-- | Create the new process identifiers.
processParallelCreateIds :: Comp m => [Process m a] -> Simulation m [(ProcessId m, Process m a)]
{-# INLINABLE processParallelCreateIds #-}
processParallelCreateIds xs =
  do pids <- liftSimulation $ forM xs $ const newProcessId
     return $ zip pids xs

-- | Prepare the processes for parallel execution.
processParallelPrepare :: Comp m => [(ProcessId m, Process m a)] -> Event m ()
{-# INLINABLE processParallelPrepare #-}
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
processUsingId :: Comp m => ProcessId m -> Process m a -> Process m a
{-# INLINABLE processUsingId #-}
processUsingId pid x =
  Process $ \pid' ->
  do liftEvent $ processIdPrepare pid
     rerunCont (invokeProcess pid x) (processCancelSource pid)

-- | Spawn the child process specifying how the child and parent processes
-- should be cancelled in case of need.
spawnProcess :: Comp m => ContCancellation -> Process m () -> Process m ()
{-# INLINABLE spawnProcess #-}
spawnProcess cancellation x =
  do pid <- liftSimulation newProcessId
     spawnProcessUsingId cancellation pid x

-- | Spawn the child process specifying how the child and parent processes
-- should be cancelled in case of need.
spawnProcessUsingId :: Comp m => ContCancellation -> ProcessId m -> Process m () -> Process m ()
{-# INLINABLE spawnProcessUsingId #-}
spawnProcessUsingId cancellation pid x =
  Process $ \pid' ->
  do liftEvent $ processIdPrepare pid
     spawnCont cancellation (invokeProcess pid x) (processCancelSource pid)

-- | Await the signal.
processAwait :: Comp m => Signal m a -> Process m a
{-# INLINABLE processAwait #-}
processAwait signal =
  Process $ \pid -> contAwait signal

-- | The result of memoization.
data MemoResult a = MemoComputed a
                  | MemoError IOException
                  | MemoCancelled

-- | Memoize the process so that it would always return the same value
-- within the simulation run.
memoProcess :: Comp m => Process m a -> Simulation m (Process m a)
{-# INLINABLE memoProcess #-}
memoProcess x =
  Simulation $ \r ->
  do let s = runSession r
     started  <- newProtoRef s False
     computed <- invokeSimulation r newSignalSource
     value    <- newProtoRef s Nothing
     let result =
           do Just x <- liftComp $ readProtoRef value
              case x of
                MemoComputed a -> return a
                MemoError e    -> throwProcess e
                MemoCancelled  -> cancelProcess
     return $
       do v <- liftComp $ readProtoRef value
          case v of
            Just _ -> result
            Nothing ->
              do f <- liftComp $ readProtoRef started
                 case f of
                   True ->
                     do processAwait $ publishSignal computed
                        result
                   False ->
                     do liftComp $ writeProtoRef started True
                        r <- liftComp $ newProtoRef s MemoCancelled
                        finallyProcess
                          (catchProcess
                           (do a <- x    -- compute only once!
                               liftComp $ writeProtoRef r (MemoComputed a))
                           (\e ->
                             liftComp $ writeProtoRef r (MemoError e)))
                          (liftEvent $
                           do liftComp $
                                do x <- readProtoRef r
                                   writeProtoRef value (Just x)
                              triggerSignal computed ())
                        result

-- | Zip two parallel processes waiting for the both.
zipProcessParallel :: Comp m => Process m a -> Process m b -> Process m (a, b)
{-# INLINABLE zipProcessParallel #-}
zipProcessParallel x y =
  do [Left a, Right b] <- processParallel [fmap Left x, fmap Right y]
     return (a, b)

-- | Zip three parallel processes waiting for their results.
zip3ProcessParallel :: Comp m => Process m a -> Process m b -> Process m c -> Process m (a, b, c)
{-# INLINABLE zip3ProcessParallel #-}
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
unzipProcess :: (Comp m, MonadIO m) => Process m (a, b) -> Simulation m (Process m a, Process m b)
{-# INLINABLE unzipProcess #-}
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
timeoutProcess :: (Comp m, MonadIO m) => Double -> Process m a -> Process m (Maybe a)
{-# INLINABLE timeoutProcess #-}
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
timeoutProcessUsingId :: (Comp m, MonadIO m) => Double -> ProcessId m -> Process m a -> Process m (Maybe a)
{-# INLINABLE timeoutProcessUsingId #-}
timeoutProcessUsingId timeout pid p =
  do s <- liftSimulation newSignalSource
     timeoutPid <- liftSimulation newProcessId
     spawnProcessUsingId CancelChildAfterParent timeoutPid $
       finallyProcess
       (holdProcess timeout)
       (liftEvent $
        cancelProcessWithId pid)
     spawnProcessUsingId CancelChildAfterParent pid $
       do sn <- liftParameter simulationSession
          r <- liftComp $ newProtoRef sn Nothing
          finallyProcess
            (catchProcess
             (do a <- p
                 liftComp $ writeProtoRef r $ Just (Right a))
             (\e ->
               liftComp $ writeProtoRef r $ Just (Left e)))
            (liftEvent $
             do x <- liftComp $ readProtoRef r
                triggerSignal s x)
     x <- processAwait $ publishSignal s
     case x of
       Nothing -> return Nothing
       Just (Right a) -> return (Just a)
       Just (Left (SomeException e)) -> throwProcess e

-- | Yield to allow other 'Process' and 'Event' computations to run
-- at the current simulation time point.
processYield :: Comp m => Process m ()
{-# INLINABLE processYield #-}
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
neverProcess :: Comp m => Process m a
{-# INLINABLE neverProcess #-}
neverProcess =
  Process $ \pid ->
  Cont $ \c ->
  let signal = processCancelling pid
  in handleSignal_ signal $ \_ ->
     resumeCont c $ error "It must never be computed: neverProcess"
