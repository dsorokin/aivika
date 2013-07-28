
-- |
-- Module     : Simulation.Aivika.Internal.Process
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
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
       (ProcessId,
        Process(..),
        invokeProcess,
        runProcess,
        runProcessInStartTime,
        runProcessInStopTime,
        enqueueProcess,
        enqueueProcessWithStartTime,
        enqueueProcessWithStopTime,
        newProcessId,
        newProcessIdWithCatch,
        processIdWithCatch,
        holdProcess,
        interruptProcess,
        processInterrupted,
        passivateProcess,
        processPassive,
        reactivateProcess,
        processId,
        cancelProcess,
        processCanceled,
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

-- | Represents a process identifier.
data ProcessId = 
  ProcessId { processStarted :: IORef Bool,
              processCatchFlag     :: Bool,
              processReactCont     :: IORef (Maybe (ContParams ())), 
              processCancelRef     :: IORef Bool, 
              processCancelToken   :: IORef Bool,
              processInterruptRef  :: IORef Bool, 
              processInterruptCont :: IORef (Maybe (ContParams ())), 
              processInterruptVersion :: IORef Int }

-- | Specifies a discontinuous process that can suspend at any time
-- and then resume later.
newtype Process a = Process (ProcessId -> Cont a)

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

-- | Start immediately the process with the specified identifier.
--            
-- To run the process at the specified time, you can use
-- the 'enqueueProcess' function.
runProcess :: ProcessId -> Process () -> Event ()
runProcess pid p =
  runCont m cont econt ccont
  (processCancelRef pid) (processCancelToken pid) (processCatchFlag pid)
    where cont  = return
          econt = throwEvent
          ccont = return
          m = do y <- liftIO $ readIORef (processStarted pid)
                 if y 
                   then error $
                        "Another process with this identifier " ++
                        "has been started already: runProcess"
                   else liftIO $ writeIORef (processStarted pid) True
                 invokeProcess pid p

-- | Start the process in the start time immediately.
runProcessInStartTime :: EventProcessing -> ProcessId -> Process () -> Simulation ()
runProcessInStartTime processing pid p =
  runEventInStartTime processing $ runProcess pid p

-- | Start the process in the stop time immediately.
runProcessInStopTime :: EventProcessing -> ProcessId -> Process () -> Simulation ()
runProcessInStopTime processing pid p =
  runEventInStopTime processing $ runProcess pid p

-- | Enqueue the process that will be then started at the specified time
-- from the event queue.
enqueueProcess :: Double -> ProcessId -> Process () -> Event ()
enqueueProcess t pid p =
  enqueueEvent t $ runProcess pid p

-- | Enqueue the process that will be then started in the start time
-- from the event queue.
enqueueProcessWithStartTime :: ProcessId -> Process () -> Event ()
enqueueProcessWithStartTime pid p =
  enqueueEventWithStartTime $ runProcess pid p

-- | Enqueue the process that will be then started in the stop time
-- from the event queue.
enqueueProcessWithStopTime :: ProcessId -> Process () -> Event ()
enqueueProcessWithStopTime pid p =
  enqueueEventWithStopTime $ runProcess pid p

-- | Return the current process identifier.
processId :: Process ProcessId
processId = Process return

-- | Create a new process identifier without exception handling.
newProcessId :: Simulation ProcessId
newProcessId =
  do x <- liftIO $ newIORef Nothing
     y <- liftIO $ newIORef False
     c <- liftIO $ newIORef False
     t <- liftIO $ newIORef False
     i <- liftIO $ newIORef False
     z <- liftIO $ newIORef Nothing
     v <- liftIO $ newIORef 0
     return ProcessId { processStarted = y,
                        processCatchFlag     = False,
                        processReactCont     = x, 
                        processCancelRef     = c, 
                        processCancelToken   = t,
                        processInterruptRef  = i,
                        processInterruptCont = z, 
                        processInterruptVersion = v }

-- | Create a new process identifier with capabilities of catching 
-- the 'IOError' exceptions and finalizing the computation. 
-- The corresponded process will be slower than that one
-- which identifier is created with help of 'newProcessId'.
newProcessIdWithCatch :: Simulation ProcessId
newProcessIdWithCatch =
  do x <- liftIO $ newIORef Nothing
     y <- liftIO $ newIORef False
     c <- liftIO $ newIORef False
     t <- liftIO $ newIORef False
     i <- liftIO $ newIORef False
     z <- liftIO $ newIORef Nothing
     v <- liftIO $ newIORef 0
     return ProcessId { processStarted = y,
                        processCatchFlag     = True,
                        processReactCont     = x, 
                        processCancelRef     = c, 
                        processCancelToken   = t,
                        processInterruptRef  = i,
                        processInterruptCont = z, 
                        processInterruptVersion = v }

-- | Test whether the process identifier was created with support
-- of the exception handling.
processIdWithCatch :: ProcessId -> Bool
processIdWithCatch = processCatchFlag

-- | Cancel a process with the specified identifier.
cancelProcess :: ProcessId -> Event ()
cancelProcess pid =
  Event $ \p ->
  do z <- readIORef (processCancelRef pid) 
     unless z $
       do writeIORef (processCancelRef pid) True
          writeIORef (processCancelToken pid) True

-- | Test whether the process with the specified identifier was canceled.
processCanceled :: ProcessId -> Event Bool
processCanceled pid =
  Event $ \p ->
  readIORef (processCancelRef pid)

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

