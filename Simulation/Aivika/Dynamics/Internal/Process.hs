
-- |
-- Module     : Simulation.Aivika.Dynamics.Internal.Process
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- A value in the 'Process' monad represents a discontinuous process that 
-- can suspend and resume at any time. It behaves like a dynamic process too. 
-- Any value in the 'Dynamics' monad can be lifted to the Process monad. 
-- Moreover, a value in the Process monad can be run in the Dynamics monad.
--
-- A value of the 'ProcessID' type is just an identifier of such a process.
--
module Simulation.Aivika.Dynamics.Internal.Process
       (ProcessID,
        Process(..),
        processQueue,
        newProcessID,
        holdProcess,
        passivateProcess,
        processPassive,
        reactivateProcess,
        processID,
        runProcess) where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Cont
import Simulation.Aivika.Dynamics.Lift
import Simulation.Aivika.Dynamics.EventQueue

-- | Represents a process identificator.
data ProcessID = 
  ProcessID { processQueue   :: EventQueue,  -- ^ Return the event queue.
              processStarted :: IORef Bool,
              processCont    :: IORef (Maybe (Dynamics (() -> IO ()))) }

-- | Specifies a discontinuous process that can suspend at any time
-- and then resume later.
newtype Process a = Process (ProcessID -> Cont a)

-- | Hold the process for the specified time period.
holdProcess :: Double -> Process ()
holdProcess dt =
  Process $ \pid ->
  Cont $ \c ->
  Dynamics $ \ps ->
  do let Dynamics m = enqueueCont (processQueue pid) (pointTime ps + dt) c
     m ps

-- | Passivate the process.
passivateProcess :: Process ()
passivateProcess =
  Process $ \pid ->
  Cont $ \c ->
  Dynamics $ \p ->
  do let x = processCont pid
     a <- readIORef x
     case a of
       Nothing -> writeIORef x $ Just c
       Just _  -> error "Cannot passivate the process twice: passivate"

-- | Test whether the process with the specified ID is passivated.
processPassive :: ProcessID -> Process Bool
processPassive pid =
  Process $ \_ ->
  Cont $ \(Dynamics c) ->
  Dynamics $ \p ->
  do cont' <- c p
     let x = processCont pid
     a <- readIORef x
     case a of
       Nothing -> cont' False
       Just _  -> cont' True

-- | Reactivate a process with the specified ID.
reactivateProcess :: ProcessID -> Process ()
reactivateProcess pid =
  Process $ \pid' ->
  Cont $ \c@(Dynamics cont) ->
  Dynamics $ \p ->
  do let x = processCont pid
     a <- readIORef x
     case a of
       Nothing ->
         do cont' <- cont p
            cont' ()
       Just (Dynamics cont2) ->
         do writeIORef x Nothing
            let Dynamics m = enqueueCont (processQueue pid') (pointTime p) c
            m p
            cont2' <- cont2 p
            cont2' ()

-- | Start the process with the specified ID at the desired time.
runProcess :: Process () -> ProcessID -> Double -> Dynamics ()
runProcess (Process p) pid t =
  runCont m r
    where m = do y <- liftIO $ readIORef (processStarted pid)
                 if y 
                   then error $
                        "A process with such ID " ++
                        "has been started already: runProc"
                   else liftIO $ writeIORef (processStarted pid) True
                 Cont $ \c -> enqueueCont (processQueue pid) t c
                 p pid
          r = let f () = return () in return f

-- | Return the current process ID.
processID :: Process ProcessID
processID = Process $ \pid -> return pid

-- | Create a new process ID.
newProcessID :: EventQueue -> Dynamics ProcessID
newProcessID q =
  do x <- liftIO $ newIORef Nothing
     y <- liftIO $ newIORef False
     return ProcessID { processQueue   = q,
                        processStarted = y,
                        processCont    = x }

instance Eq ProcessID where
  x == y = processCont x == processCont y    -- for the references are unique

instance Monad Process where
  return  = returnP
  m >>= k = bindP m k

instance Functor Process where
  fmap = liftM

instance Lift Process where
  liftD = liftP
  
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
     let Process m' = k a
     m' pid

liftP :: Dynamics a -> Process a
{-# INLINE liftP #-}
liftP m = Process $ \pid -> liftD m

liftIOP :: IO a -> Process a
{-# INLINE liftIOP #-}
liftIOP m = Process $ \pid -> liftIO m
