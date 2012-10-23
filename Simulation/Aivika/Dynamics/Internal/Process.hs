
-- |
-- Module     : Simulation.Aivika.Dynamics.Internal.Process
-- Copyright  : Copyright (c) 2009-2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- A value in the 'Process' monad represents a discontinuous process that 
-- can suspend in any simulation time point and then resume later in the same 
-- or another time point. 
-- 
-- The process of this type behaves like a dynamic process too. So, any value 
-- in the 'Dynamics' monad can be lifted to the Process monad. Moreover, 
-- a value in the Process monad can be run in the Dynamics monad.
--
-- A value of the 'ProcessID' type is just an identifier of such a process.
--
module Simulation.Aivika.Dynamics.Internal.Process
       (ProcessID,
        Process(..),
        processQueue,
        newProcessID,
        holdProcess,
        interruptProcess,
        processInterrupted,
        passivateProcess,
        processPassive,
        reactivateProcess,
        processID,
        cancelProcess,
        processCanceled,
        runProcess,
        runProcessNow) where

import Data.Maybe
import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Cont
import Simulation.Aivika.Dynamics.EventQueue

-- | Represents a process identificator.
data ProcessID = 
  ProcessID { processQueue   :: EventQueue,  -- ^ Return the event queue.
              processStarted :: IORef Bool,
              processReactCont     :: IORef (Maybe (() -> Dynamics ())), 
              processCancelRef     :: IORef Bool, 
              processCancelComp    :: Cont Bool,
              processInterruptRef  :: IORef Bool, 
              processInterruptCont :: IORef (Maybe (() -> Dynamics ())), 
              processInterruptVersion :: IORef Int }

-- | Specifies a discontinuous process that can suspend at any time
-- and then resume later.
newtype Process a = Process (ProcessID -> Cont a)

-- | Hold the process for the specified time period.
holdProcess :: Double -> Process ()
holdProcess dt =
  Process $ \pid ->
  Cont $ \c ->
  Dynamics $ \p ->
  do let x = processInterruptCont pid
     writeIORef x $ Just c
     writeIORef (processInterruptRef pid) False
     v <- readIORef (processInterruptVersion pid)
     let Dynamics m = 
           enqueue (processQueue pid) (pointTime p + dt) $
           Dynamics $ \p ->
           do v' <- readIORef (processInterruptVersion pid)
              when (v == v') $ 
                do writeIORef x Nothing
                   let Dynamics m = c ()
                   m p
     m p

-- | Interrupt a process with the specified ID.
interruptProcess :: ProcessID -> Dynamics ()
interruptProcess pid =
  Dynamics $ \p ->
  do let x = processInterruptCont pid
     a <- readIORef x
     case a of
       Nothing -> return ()
       Just c ->
         do writeIORef x Nothing
            writeIORef (processInterruptRef pid) True
            modifyIORef (processInterruptVersion pid) $ (+) 1
            let Dynamics m = 
                  enqueueCont (processQueue pid) (pointTime p) $ c
            m p
            
-- | Test whether the process with the specified ID was interrupted.
processInterrupted :: ProcessID -> Dynamics Bool
processInterrupted pid =
  Dynamics $ \p ->
  readIORef (processInterruptRef pid)

-- | Passivate the process.
passivateProcess :: Process ()
passivateProcess =
  Process $ \pid ->
  Cont $ \c ->
  Dynamics $ \p ->
  do let x = processReactCont pid
     a <- readIORef x
     case a of
       Nothing -> writeIORef x $ Just c
       Just _  -> error "Cannot passivate the process twice: passivate"

-- | Test whether the process with the specified ID is passivated.
processPassive :: ProcessID -> Dynamics Bool
processPassive pid =
  Dynamics $ \p ->
  do let Dynamics m = queueRun $ processQueue pid
     m p
     let x = processReactCont pid
     a <- readIORef x
     return $ isJust a

-- | Reactivate a process with the specified ID.
reactivateProcess :: ProcessID -> Dynamics ()
reactivateProcess pid =
  Dynamics $ \p ->
  do let Dynamics m = queueRun $ processQueue pid
     m p
     let x = processReactCont pid
     a <- readIORef x
     case a of
       Nothing -> 
         return ()
       Just c ->
         do writeIORef x Nothing
            let Dynamics m  = enqueueCont (processQueue pid) (pointTime p) c
            m p

-- | Start the process with the specified ID at the desired time.
runProcess :: Process () -> ProcessID -> Double -> Dynamics ()
runProcess (Process p) pid t =
  runCont m return
    where m = do y <- liftIO $ readIORef (processStarted pid)
                 if y 
                   then error $
                        "A process with such ID " ++
                        "has been started already: runProc"
                   else liftIO $ writeIORef (processStarted pid) True
                 Cont $ \c -> enqueueCont (processQueue pid) t c
                 p pid

-- | Start the process with the specified ID at the current simulation time.
runProcessNow :: Process () -> ProcessID -> Dynamics ()
runProcessNow process pid =
  Dynamics $ \p ->
  do let Dynamics m = runProcess process pid (pointTime p)
     m p

-- | Return the current process ID.
processID :: Process ProcessID
processID = Process $ \pid -> return pid

-- | Create a new process ID.
newProcessID :: EventQueue -> Simulation ProcessID
newProcessID q =
  do x <- liftIO $ newIORef Nothing
     y <- liftIO $ newIORef False
     c <- liftIO $ newIORef False
     i <- liftIO $ newIORef False
     z <- liftIO $ newIORef Nothing
     v <- liftIO $ newIORef 0
     return ProcessID { processQueue   = q,
                        processStarted = y,
                        processReactCont     = x, 
                        processCancelRef     = c, 
                        processCancelComp    = liftIO $ readIORef c,
                        processInterruptRef  = i,
                        processInterruptCont = z, 
                        processInterruptVersion = v }

-- | Cancel a process with the specified ID.
cancelProcess :: ProcessID -> Dynamics ()
cancelProcess pid =
  Dynamics $ \p ->
  writeIORef (processCancelRef pid) True

-- | Test whether the process with the specified ID is canceled.
processCanceled :: ProcessID -> Dynamics Bool
processCanceled pid =
  Dynamics $ \p ->
  readIORef (processCancelRef pid)

instance Eq ProcessID where
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
  
instance MonadIO Process where
  liftIO = liftIOP
  
returnP :: a -> Process a
{-# INLINE returnP #-}
returnP a = Process (\pid -> return a)

bindP :: Process a -> (a -> Process b) -> Process b
{-# INLINE bindP #-}
bindP (Process m) k = 
  Process $ \pid -> 
  do a <- m pid
     c <- processCancelComp pid
     if c
       then cancelCont
       else do let Process m' = k a
               m' pid

liftSP :: Simulation a -> Process a
{-# INLINE liftSP #-}
liftSP m = Process $ \pid -> liftSimulation m

liftDP :: Dynamics a -> Process a
{-# INLINE liftDP #-}
liftDP m = Process $ \pid -> liftDynamics m

liftIOP :: IO a -> Process a
{-# INLINE liftIOP #-}
liftIOP m = Process $ \pid -> liftIO m
