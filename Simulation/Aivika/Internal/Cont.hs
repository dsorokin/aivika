
-- |
-- Module     : Simulation.Aivika.Internal.Cont
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This is an internal implementation module that should never be used directly.
--
-- The 'Cont' monad is a variation of the standard @Cont@ monad 
-- and F# async workflow, where the result of applying 
-- the continuations is the 'Event' computation.
--
module Simulation.Aivika.Internal.Cont
       (ContCancellation(..),
        ContId,
        ContEvent(..),
        Cont(..),
        ContParams,
        newContId,
        contChanged,
        contCancellationInitiated,
        contCancellationInitiate,
        contCancellationInitiating,
        contCancellationBind,
        contCancellationConnect,
        contPreemptionActuated,
        contPreemptionActuate,
        contPreemptionActuating,
        contPreemptionReentering,
        contPreemptionReenter,
        invokeCont,
        runCont,
        rerunCont,
        spawnCont,
        contParallel,
        contParallel_,
        catchCont,
        finallyCont,
        throwCont,
        resumeCont,
        resumeECont,
        contCanceled,
        contFreeze,
        contAwait,
        traceCont) where

import Data.IORef
import Data.Array
import Data.Array.IO.Safe
import Data.Monoid

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import Debug.Trace

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Signal

-- | It defines how the parent and child computations should be cancelled.
data ContCancellation = CancelTogether
                        -- ^ Cancel the both computations together.
                      | CancelChildAfterParent
                        -- ^ Cancel the child if its parent is cancelled.
                      | CancelParentAfterChild
                        -- ^ Cancel the parent if its child is cancelled.
                      | CancelInIsolation
                        -- ^ Cancel the computations in isolation.

-- | It identifies the 'Cont' computation.
data ContId =
  ContId { contCancellationInitiatedRef :: IORef Bool,
           contCancellationActivatedRef :: IORef Bool,
           contPreemptionActuatedRef :: IORef Bool,
           contChangeSource :: SignalSource ContEvent
         }

-- | The event that occurs within the 'Cont' computation.
data ContEvent = ContCancellationInitiating
                 -- ^ Cancel the computation.
               | ContPreemptionActuating
                 -- ^ Preempt the computation.
               | ContPreemptionReentering
                 -- ^ Reenter the computation after if was preempted.
               deriving (Eq, Ord, Show)

-- | Create a computation identifier.
newContId :: Simulation ContId
newContId =
  Simulation $ \r ->
  do r1 <- newIORef False
     r2 <- newIORef False
     r3 <- newIORef False
     s  <- invokeSimulation r newSignalSource
     return ContId { contCancellationInitiatedRef = r1,
                     contCancellationActivatedRef = r2,
                     contPreemptionActuatedRef = r3,
                     contChangeSource = s
                   }

-- | Signal when the computation state changes.
contChanged :: ContId -> Signal ContEvent
contChanged = publishSignal . contChangeSource

-- | Signal when the cancellation is intiating.
contCancellationInitiating :: ContId -> Signal ()
contCancellationInitiating =
  filterSignal_ (ContCancellationInitiating ==) . contChanged

-- | Whether the cancellation was initiated.
contCancellationInitiated :: ContId -> Event Bool
contCancellationInitiated x =
  Event $ \p -> readIORef (contCancellationInitiatedRef x)

-- | Whether the cancellation was activated.
contCancellationActivated :: ContId -> IO Bool
contCancellationActivated =
  readIORef . contCancellationActivatedRef

-- | Deactivate the cancellation.
contCancellationDeactivate :: ContId -> IO ()
contCancellationDeactivate x =
  writeIORef (contCancellationActivatedRef x) False

-- | If the main computation is cancelled then all the nested ones will be cancelled too.
contCancellationBind :: ContId -> [ContId] -> Event DisposableEvent
contCancellationBind x ys =
  Event $ \p ->
  do hs1 <- forM ys $ \y ->
       invokeEvent p $
       handleSignal (contCancellationInitiating x) $ \_ ->
       contCancellationInitiate y
     hs2 <- forM ys $ \y ->
       invokeEvent p $
       handleSignal (contCancellationInitiating y) $ \_ ->
       contCancellationInitiate x
     return $ mconcat hs1 <> mconcat hs2

-- | Connect the parent computation to the child one.
contCancellationConnect :: ContId
                           -- ^ the parent
                           -> ContCancellation
                           -- ^ how to connect
                           -> ContId
                           -- ^ the child
                           -> Event DisposableEvent
                           -- ^ computation of the disposable handler
contCancellationConnect parent cancellation child =
  Event $ \p ->
  do let m1 =
           handleSignal (contCancellationInitiating parent) $ \_ ->
           contCancellationInitiate child
         m2 =
           handleSignal (contCancellationInitiating child) $ \_ ->
           contCancellationInitiate parent
     h1 <- 
       case cancellation of
         CancelTogether -> invokeEvent p m1
         CancelChildAfterParent -> invokeEvent p m1
         CancelParentAfterChild -> return mempty
         CancelInIsolation -> return mempty
     h2 <-
       case cancellation of
         CancelTogether -> invokeEvent p m2
         CancelChildAfterParent -> return mempty
         CancelParentAfterChild -> invokeEvent p m2
         CancelInIsolation -> return mempty
     return $ h1 <> h2

-- | Initiate the cancellation.
contCancellationInitiate :: ContId -> Event ()
contCancellationInitiate x =
  Event $ \p ->
  do f <- readIORef (contCancellationInitiatedRef x)
     unless f $
       do writeIORef (contCancellationInitiatedRef x) True
          writeIORef (contCancellationActivatedRef x) True
          invokeEvent p $
            triggerSignal (contChangeSource x) ContCancellationInitiating

-- | Preempt the computation.
contPreemptionActuate :: ContId -> Event ()
contPreemptionActuate x =
  Event $ \p ->
  do f <- readIORef (contCancellationInitiatedRef x)
     unless f $
       do g <- readIORef (contPreemptionActuatedRef x)
          unless g $
            do writeIORef (contPreemptionActuatedRef x) True
               invokeEvent p $
                 triggerSignal (contChangeSource x) ContPreemptionActuating

-- | Reenter the computation after it was preempted.
contPreemptionReenter :: ContId -> Event ()
contPreemptionReenter x =
  Event $ \p ->
  do f <- readIORef (contCancellationInitiatedRef x)
     unless f $
       do g <- readIORef (contPreemptionActuatedRef x)
          when g $
            do writeIORef (contPreemptionActuatedRef x) False
               invokeEvent p $
                 triggerSignal (contChangeSource x) ContPreemptionReentering

-- | Signal when the computation is preempted.
contPreemptionActuating :: ContId -> Signal ()
contPreemptionActuating =
  filterSignal_ (ContPreemptionActuating ==) . contChanged

-- | Signal when the computation is reentered after it was preempted before.
contPreemptionReentering :: ContId -> Signal ()
contPreemptionReentering =
  filterSignal_ (ContPreemptionReentering ==) . contChanged

-- | Whether the computation was preemtped.
contPreemptionActuated :: ContId -> Event Bool
contPreemptionActuated x =
  Event $ \p -> readIORef (contPreemptionActuatedRef x)

-- | The 'Cont' type is similar to the standard @Cont@ monad 
-- and F# async workflow but only the result of applying
-- the continuations return the 'Event' computation.
newtype Cont a = Cont (ContParams a -> Event ())

-- | The continuation parameters.
data ContParams a = 
  ContParams { contCont :: a -> Event (), 
               contAux  :: ContParamsAux }

-- | The auxiliary continuation parameters.
data ContParamsAux =
  ContParamsAux { contECont :: SomeException -> Event (),
                  contCCont :: () -> Event (),
                  contId :: ContId,
                  contCancelFlag :: IO Bool,
                  contCatchFlag  :: Bool }

instance Monad Cont where
  return  = returnC
  m >>= k = bindC m k

instance ParameterLift Cont where
  liftParameter = liftPC

instance SimulationLift Cont where
  liftSimulation = liftSC

instance DynamicsLift Cont where
  liftDynamics = liftDC

instance EventLift Cont where
  liftEvent = liftEC

instance Functor Cont where
  fmap = liftM

instance Applicative Cont where
  pure = return
  (<*>) = ap

instance MonadIO Cont where
  liftIO = liftIOC 

-- | Invoke the computation.
invokeCont :: ContParams a -> Cont a -> Event ()
{-# INLINE invokeCont #-}
invokeCont p (Cont m) = m p

-- | Cancel the computation.
cancelCont :: Point -> ContParams a -> IO ()
{-# NOINLINE cancelCont #-}
cancelCont p c =
  do contCancellationDeactivate (contId $ contAux c)
     invokeEvent p $ (contCCont $ contAux c) ()

returnC :: a -> Cont a
{-# INLINE returnC #-}
returnC a = 
  Cont $ \c ->
  Event $ \p ->
  do z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ contCont c a
                          
bindC :: Cont a -> (a -> Cont b) -> Cont b
{-# INLINE bindC #-}
bindC (Cont m) k =
  Cont $ \c ->
  Event $ \p ->
  do z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ m $ 
            let cont a = invokeCont c (k a)
            in c { contCont = cont }

-- | Like @return a >>= k@.
callCont :: (a -> Cont b) -> a -> ContParams b -> Event ()
callCont k a c =
  Event $ \p ->
  do z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ invokeCont c (k a)

-- | Exception handling within 'Cont' computations.
catchCont :: Exception e => Cont a -> (e -> Cont a) -> Cont a
catchCont (Cont m) h = 
  Cont $ \c0 ->
  Event $ \p ->
  do let c = c0 { contAux = (contAux c0) { contCatchFlag = True } }
     z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ m $
            let econt e0 =
                  case fromException e0 of
                    Just e  -> callCont h e c
                    Nothing -> (contECont . contAux $ c) e0
            in c { contAux = (contAux c) { contECont = econt } }
               
-- | A computation with finalization part.
finallyCont :: Cont a -> Cont b -> Cont a
finallyCont (Cont m) (Cont m') = 
  Cont $ \c0 ->
  Event $ \p ->
  do let c = c0 { contAux = (contAux c0) { contCatchFlag = True } }
     z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ m $
            let cont a   = 
                  Event $ \p ->
                  invokeEvent p $ m' $
                  let cont b = contCont c a
                  in c { contCont = cont }
                econt e  =
                  Event $ \p ->
                  invokeEvent p $ m' $
                  let cont b = (contECont . contAux $ c) e
                  in c { contCont = cont }
                ccont () = 
                  Event $ \p ->
                  invokeEvent p $ m' $
                  let cont b  = (contCCont . contAux $ c) ()
                      econt e = (contCCont . contAux $ c) ()
                  in c { contCont = cont,
                         contAux  = (contAux c) { contECont = econt } }
            in c { contCont = cont,
                   contAux  = (contAux c) { contECont = econt,
                                            contCCont = ccont } }

-- | Throw the exception with the further exception handling.
--
-- By some reason, an exception raised with help of the standard 'throw' function
-- is not handled properly within 'Cont' computation, altough it will be still handled 
-- if it will be wrapped in the 'IO' monad. Therefore, you should use specialised
-- functions like the stated one that use the 'throw' function but within the 'IO' computation,
-- which allows already handling the exception.
throwCont :: IOException -> Cont a
throwCont = liftIO . throw

-- | Run the 'Cont' computation with the specified cancelation source 
-- and flag indicating whether to catch exceptions from the beginning.
runCont :: Cont a
           -- ^ the computation to run
           -> (a -> Event ())
           -- ^ the main branch 
           -> (SomeException -> Event ())
           -- ^ the branch for handing exceptions
           -> (() -> Event ())
           -- ^ the branch for cancellation
           -> ContId
           -- ^ the computation identifier
           -> Bool
           -- ^ whether to support the exception handling from the beginning
           -> Event ()
runCont (Cont m) cont econt ccont cid catchFlag = 
  m ContParams { contCont = cont,
                 contAux  = 
                   ContParamsAux { contECont = econt,
                                   contCCont = ccont,
                                   contId = cid,
                                   contCancelFlag = contCancellationActivated cid, 
                                   contCatchFlag  = catchFlag } }

-- | Lift the 'Parameter' computation.
liftPC :: Parameter a -> Cont a
liftPC (Parameter m) = 
  Cont $ \c ->
  Event $ \p ->
  if contCatchFlag . contAux $ c
  then liftIOWithCatch (m $ pointRun p) p c
  else liftIOWithoutCatch (m $ pointRun p) p c

-- | Lift the 'Simulation' computation.
liftSC :: Simulation a -> Cont a
liftSC (Simulation m) = 
  Cont $ \c ->
  Event $ \p ->
  if contCatchFlag . contAux $ c
  then liftIOWithCatch (m $ pointRun p) p c
  else liftIOWithoutCatch (m $ pointRun p) p c
     
-- | Lift the 'Dynamics' computation.
liftDC :: Dynamics a -> Cont a
liftDC (Dynamics m) =
  Cont $ \c ->
  Event $ \p ->
  if contCatchFlag . contAux $ c
  then liftIOWithCatch (m p) p c
  else liftIOWithoutCatch (m p) p c
     
-- | Lift the 'Event' computation.
liftEC :: Event a -> Cont a
liftEC (Event m) =
  Cont $ \c ->
  Event $ \p ->
  if contCatchFlag . contAux $ c
  then liftIOWithCatch (m p) p c
  else liftIOWithoutCatch (m p) p c
     
-- | Lift the IO computation.
liftIOC :: IO a -> Cont a
liftIOC m =
  Cont $ \c ->
  Event $ \p ->
  if contCatchFlag . contAux $ c
  then liftIOWithCatch m p c
  else liftIOWithoutCatch m p c
  
liftIOWithoutCatch :: IO a -> Point -> ContParams a -> IO ()
{-# INLINE liftIOWithoutCatch #-}
liftIOWithoutCatch m p c =
  do z <- contCanceled c
     if z
       then cancelCont p c
       else do a <- m
               invokeEvent p $ contCont c a

liftIOWithCatch :: IO a -> Point -> ContParams a -> IO ()
{-# NOINLINE liftIOWithCatch #-}
liftIOWithCatch m p c =
  do z <- contCanceled c
     if z
       then cancelCont p c
       else do aref <- newIORef undefined
               eref <- newIORef Nothing
               catch (m >>= writeIORef aref) 
                 (writeIORef eref . Just)
               e <- readIORef eref
               case e of
                 Nothing -> 
                   do a <- readIORef aref
                      -- tail recursive
                      invokeEvent p $ contCont c a
                 Just e ->
                   -- tail recursive
                   invokeEvent p $ (contECont . contAux) c e

-- | Resume the computation by the specified parameters.
resumeCont :: ContParams a -> a -> Event ()
{-# INLINE resumeCont #-}
resumeCont c a = 
  Event $ \p ->
  do z <- contCanceled c
     if z
       then cancelCont p c
       else invokeEvent p $ contCont c a

-- | Resume the exception handling by the specified parameters.
resumeECont :: ContParams a -> SomeException -> Event ()
{-# INLINE resumeECont #-}
resumeECont c e = 
  Event $ \p ->
  do z <- contCanceled c
     if z
       then cancelCont p c
       else invokeEvent p $ (contECont $ contAux c) e

-- | Test whether the computation is canceled.
contCanceled :: ContParams a -> IO Bool
{-# INLINE contCanceled #-}
contCanceled c = contCancelFlag $ contAux c

-- | Execute the specified computations in parallel within
-- the current computation and return their results. The cancellation
-- of any of the nested computations affects the current computation.
-- The exception raised in any of the nested computations is propogated
-- to the current computation as well (if the exception handling is
-- supported).
--
-- Here word @parallel@ literally means that the computations are
-- actually executed on a single operating system thread but
-- they are processed simultaneously by the event queue.
contParallel :: [(Cont a, ContId)]
                -- ^ the list of:
                -- the nested computation,
                -- the computation identifier
                -> Cont [a]
contParallel xs =
  Cont $ \c ->
  Event $ \p ->
  do let n = length xs
         worker =
           do results   <- newArray_ (1, n) :: IO (IOArray Int a)
              counter   <- newIORef 0
              catchRef  <- newIORef Nothing
              hs <- invokeEvent p $
                    contCancellationBind (contId $ contAux c) $
                    map snd xs
              let propagate =
                    Event $ \p ->
                    do n' <- readIORef counter
                       when (n' == n) $
                         do invokeEvent p $ disposeEvent hs  -- unbind the cancellation sources
                            f1 <- contCanceled c
                            f2 <- readIORef catchRef
                            case (f1, f2) of
                              (False, Nothing) ->
                                do rs <- getElems results
                                   invokeEvent p $ resumeCont c rs
                              (False, Just e) ->
                                invokeEvent p $ resumeECont c e
                              (True, _) ->
                                cancelCont p c
                  cont i a =
                    Event $ \p ->
                    do modifyIORef counter (+ 1)
                       writeArray results i a
                       invokeEvent p propagate
                  econt e =
                    Event $ \p ->
                    do modifyIORef counter (+ 1)
                       r <- readIORef catchRef
                       case r of
                         Nothing -> writeIORef catchRef $ Just e
                         Just e' -> return ()  -- ignore the next error
                       invokeEvent p propagate
                  ccont e =
                    Event $ \p ->
                    do modifyIORef counter (+ 1)
                       -- the main computation was automatically canceled
                       invokeEvent p propagate
              forM_ (zip [1..n] xs) $ \(i, (x, cid)) ->
                invokeEvent p $
                runCont x (cont i) econt ccont cid (contCatchFlag $ contAux c)
     z <- contCanceled c
     if z
       then cancelCont p c
       else if n == 0
            then invokeEvent p $ contCont c []
            else worker

-- | A partial case of 'contParallel' when we are not interested in
-- the results but we are interested in the actions to be peformed by
-- the nested computations.
contParallel_ :: [(Cont a, ContId)]
                 -- ^ the list of:
                 -- the nested computation,
                 -- the computation identifier
                 -> Cont ()
contParallel_ xs =
  Cont $ \c ->
  Event $ \p ->
  do let n = length xs
         worker =
           do counter   <- newIORef 0
              catchRef  <- newIORef Nothing
              hs <- invokeEvent p $
                    contCancellationBind (contId $ contAux c) $
                    map snd xs
              let propagate =
                    Event $ \p ->
                    do n' <- readIORef counter
                       when (n' == n) $
                         do invokeEvent p $ disposeEvent hs  -- unbind the cancellation sources
                            f1 <- contCanceled c
                            f2 <- readIORef catchRef
                            case (f1, f2) of
                              (False, Nothing) ->
                                invokeEvent p $ resumeCont c ()
                              (False, Just e) ->
                                invokeEvent p $ resumeECont c e
                              (True, _) ->
                                cancelCont p c
                  cont i a =
                    Event $ \p ->
                    do modifyIORef counter (+ 1)
                       -- ignore the result
                       invokeEvent p propagate
                  econt e =
                    Event $ \p ->
                    do modifyIORef counter (+ 1)
                       r <- readIORef catchRef
                       case r of
                         Nothing -> writeIORef catchRef $ Just e
                         Just e' -> return ()  -- ignore the next error
                       invokeEvent p propagate
                  ccont e =
                    Event $ \p ->
                    do modifyIORef counter (+ 1)
                       -- the main computation was automatically canceled
                       invokeEvent p propagate
              forM_ (zip [1..n] xs) $ \(i, (x, cid)) ->
                invokeEvent p $
                runCont x (cont i) econt ccont cid (contCatchFlag $ contAux c)
     z <- contCanceled c
     if z
       then cancelCont p c
       else if n == 0
            then invokeEvent p $ contCont c ()
            else worker

-- | Rerun the 'Cont' computation with the specified identifier.
rerunCont :: Cont a -> ContId -> Cont a
rerunCont x cid =
  Cont $ \c ->
  Event $ \p ->
  do let worker =
           do hs <- invokeEvent p $
                    contCancellationBind (contId $ contAux c) [cid]
              let cont a  =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       invokeEvent p $ resumeCont c a
                  econt e =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       invokeEvent p $ resumeECont c e
                  ccont e =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       cancelCont p c
              invokeEvent p $
                runCont x cont econt ccont cid (contCatchFlag $ contAux c)
     z <- contCanceled c
     if z
       then cancelCont p c
       else worker

-- | Run the 'Cont' computation in parallel but connect the computations.
spawnCont :: ContCancellation -> Cont () -> ContId -> Cont ()
spawnCont cancellation x cid =
  Cont $ \c ->
  Event $ \p ->
  do let worker =
           do hs <- invokeEvent p $
                    contCancellationConnect
                    (contId $ contAux c) cancellation cid
              let cont a  =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       -- do nothing and it will finish the computation
                  econt e =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       invokeEvent p $ throwEvent e  -- this is all we can do
                  ccont e =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       -- do nothing and it will finish the computation
              invokeEvent p $
                enqueueEvent (pointTime p) $
                runCont x cont econt ccont cid False
              invokeEvent p $
                resumeCont c ()
     z <- contCanceled c
     if z
       then cancelCont p c
       else worker

-- | Freeze the computation parameters temporarily.
contFreeze :: ContParams a -> Event (Event (Maybe (ContParams a)))
contFreeze c =
  Event $ \p ->
  do rh <- newIORef Nothing
     rc <- newIORef $ Just c
     h <- invokeEvent p $
          handleSignal (contCancellationInitiating $
                        contId $
                        contAux c) $ \a ->
          Event $ \p ->
          do h <- readIORef rh
             case h of
               Nothing ->
                 error "The handler was lost: contFreeze."
               Just h ->
                 do invokeEvent p $ disposeEvent h
                    c <- readIORef rc
                    case c of
                      Nothing -> return ()
                      Just c  ->
                        do writeIORef rc Nothing
                           invokeEvent p $
                             enqueueEvent (pointTime p) $
                             Event $ \p ->
                             do z <- contCanceled c
                                when z $ cancelCont p c
     writeIORef rh (Just h)
     return $
       Event $ \p ->
       do invokeEvent p $ disposeEvent h
          c <- readIORef rc
          writeIORef rc Nothing
          return c
     
-- | Await the signal.
contAwait :: Signal a -> Cont a
contAwait signal =
  Cont $ \c ->
  Event $ \p ->
  do c <- invokeEvent p $ contFreeze c
     r <- newIORef Nothing
     h <- invokeEvent p $
          handleSignal signal $ 
          \a -> Event $ 
                \p -> do x <- readIORef r
                         case x of
                           Nothing ->
                             error "The signal was lost: contAwait."
                           Just x ->
                             do invokeEvent p $ disposeEvent x
                                c <- invokeEvent p c
                                case c of
                                  Nothing -> return ()
                                  Just c  ->
                                    invokeEvent p $ resumeCont c a
     writeIORef r $ Just h          

-- | Show the debug message with the current simulation time.
traceCont :: String -> Cont a -> Cont a
traceCont message (Cont m) =
  Cont $ \c ->
  Event $ \p ->
  do z <- contCanceled c
     if z
       then cancelCont p c
       else trace ("t = " ++ show (pointTime p) ++ ": " ++ message) $
            invokeEvent p $ m c
