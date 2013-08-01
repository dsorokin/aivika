
-- |
-- Module     : Simulation.Aivika.Internal.Cont
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The 'Cont' monad is a variation of the standard Cont monad 
-- and F# async workflow, where the result of applying 
-- the continuations is the 'Event' computation.
--
module Simulation.Aivika.Internal.Cont
       (ContCancellation,
        Cont(..),
        ContParams,
        newContCancellation,
        contCancellationInitiated,
        contCancellationInitiate,
        contCancellationInitiating,
        contCancellationBind,
        invokeCont,
        runCont,
        rerunCont,
        contParallel,
        contParallel_,
        catchCont,
        finallyCont,
        throwCont,
        resumeCont,
        contCanceled) where

import Data.IORef
import Data.Array
import Data.Array.IO.Safe

import qualified Control.Exception as C
import Control.Exception (IOException, throw)

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Signal

-- | It manages the cancellation process.
data ContCancellation =
  ContCancellation { contCancellationInitiatedRef :: IORef Bool,
                     contCancellationActivatedRef :: IORef Bool,
                     contCancellationInitiatingSource :: SignalSource ()
                   }

-- | Create the cancellation token.
newContCancellation :: Simulation ContCancellation
newContCancellation =
  Simulation $ \r ->
  do r1 <- newIORef False
     r2 <- newIORef False
     s  <- invokeSimulation r newSignalSource
     return ContCancellation { contCancellationInitiatedRef = r1,
                               contCancellationActivatedRef = r2,
                               contCancellationInitiatingSource = s
                             }

-- | Signal when the cancellation is intiating.
contCancellationInitiating :: ContCancellation -> Signal ()
contCancellationInitiating =
  publishSignal . contCancellationInitiatingSource

-- | Whether the cancellation was initiated.
contCancellationInitiated :: ContCancellation -> Event Bool
contCancellationInitiated x =
  Event $ \p -> readIORef (contCancellationInitiatedRef x)

-- | Whether the cancellation was activated.
contCancellationActivated :: ContCancellation -> IO Bool
contCancellationActivated =
  readIORef . contCancellationActivatedRef

-- | Deactivate the cancellation.
contCancellationDeactivate :: ContCancellation -> IO ()
contCancellationDeactivate x =
  writeIORef (contCancellationActivatedRef x) False

-- | If the main computation is cancelled then all the nested ones will be cancelled too.
contCancellationBind :: ContCancellation -> [ContCancellation] -> Event (Event ())
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
     return $ do sequence_ hs1
                 sequence_ hs2

-- | Initiate the cancellation.
contCancellationInitiate :: ContCancellation -> Event ()
contCancellationInitiate x =
  Event $ \p ->
  do f <- readIORef (contCancellationInitiatedRef x)
     unless f $
       do writeIORef (contCancellationInitiatedRef x) True
          writeIORef (contCancellationActivatedRef x) True
          invokeEvent p $ triggerSignal (contCancellationInitiatingSource x) ()

-- | The 'Cont' type is similar to the standard Cont monad 
-- and F# async workflow but only the result of applying
-- the continuations return the 'Event' computation.
newtype Cont a = Cont (ContParams a -> Event ())

-- | The continuation parameters.
data ContParams a = 
  ContParams { contCont :: a -> Event (), 
               contAux  :: ContParamsAux }

-- | The auxiliary continuation parameters.
data ContParamsAux =
  ContParamsAux { contECont :: IOException -> Event (),
                  contCCont :: () -> Event (),
                  contCancel     :: ContCancellation,
                  contCancelFlag :: IO Bool,
                  contCatchFlag  :: Bool }

instance Monad Cont where
  return  = returnC
  m >>= k = bindC m k

instance SimulationLift Cont where
  liftSimulation = liftSC

instance DynamicsLift Cont where
  liftDynamics = liftDC

instance EventLift Cont where
  liftEvent = liftEC

instance Functor Cont where
  fmap = liftM

instance MonadIO Cont where
  liftIO = liftIOC 

invokeCont :: ContParams a -> Cont a -> Event ()
{-# INLINE invokeCont #-}
invokeCont p (Cont m) = m p

cancelCont :: Point -> ContParams a -> IO ()
{-# NOINLINE cancelCont #-}
cancelCont p c =
  do contCancellationDeactivate (contCancel $ contAux c)
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
                          
-- bindC :: Cont a -> (a -> Cont b) -> Cont b
-- {-# INLINE bindC #-}
-- bindC m k = 
--   Cont $ \c -> 
--   if (contCatchFlag . contAux $ c) 
--   then bindWithCatch m k c
--   else bindWithoutCatch m k c
  
bindC :: Cont a -> (a -> Cont b) -> Cont b
{-# INLINE bindC #-}
bindC m k = 
  Cont $ bindWithoutCatch m k  -- Another version is not tail recursive!
  
bindWithoutCatch :: Cont a -> (a -> Cont b) -> ContParams b -> Event ()
{-# INLINE bindWithoutCatch #-}
bindWithoutCatch (Cont m) k c = 
  Event $ \p ->
  do z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ m $ 
            let cont a = invokeCont c (k a)
            in c { contCont = cont }

-- It is not tail recursive!
bindWithCatch :: Cont a -> (a -> Cont b) -> ContParams b -> Event ()
{-# NOINLINE bindWithCatch #-}
bindWithCatch (Cont m) k c = 
  Event $ \p ->
  do z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ m $ 
            let cont a = catchEvent 
                         (invokeCont c (k a))
                         (contECont $ contAux c)
            in c { contCont = cont }

-- Like "bindWithoutCatch (return a) k"
callWithoutCatch :: (a -> Cont b) -> a -> ContParams b -> Event ()
callWithoutCatch k a c =
  Event $ \p ->
  do z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ invokeCont c (k a)

-- Like "bindWithCatch (return a) k" but it is not tail recursive!
callWithCatch :: (a -> Cont b) -> a -> ContParams b -> Event ()
callWithCatch k a c =
  Event $ \p ->
  do z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ catchEvent 
            (invokeCont c (k a))
            (contECont $ contAux c)

-- | Exception handling within 'Cont' computations.
catchCont :: Cont a -> (IOException -> Cont a) -> Cont a
catchCont m h = 
  Cont $ \c ->
  catchWithCatch m h (c { contAux = (contAux c) { contCatchFlag = True } })
  
catchWithCatch :: Cont a -> (IOException -> Cont a) -> ContParams a -> Event ()
catchWithCatch (Cont m) h c =
  Event $ \p -> 
  do z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ m $
            -- let econt e = callWithCatch h e c   -- not tail recursive!
            let econt e = callWithoutCatch h e c
            in c { contAux = (contAux c) { contECont = econt } }
               
-- | A computation with finalization part.
finallyCont :: Cont a -> Cont b -> Cont a
finallyCont m m' = 
  Cont $ \c -> 
  finallyWithCatch m m' (c { contAux = (contAux c) { contCatchFlag = True } })
  
finallyWithCatch :: Cont a -> Cont b -> ContParams a -> Event ()               
finallyWithCatch (Cont m) (Cont m') c =
  Event $ \p ->
  do z <- contCanceled c
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
-- By some reasons, the standard 'throw' function per se is not handled 
-- properly within 'Cont' computations, altough it will be still handled 
-- if it will be hidden under the 'liftIO' function. The problem arises 
-- namely with the @throw@ function, not 'IO' computations.
throwCont :: IOException -> Cont a
throwCont e = liftIO $ throw e

-- | Run the 'Cont' computation with the specified cancelation token 
-- and flag indicating whether to catch exceptions.
runCont :: Cont a
           -- ^ the computation to run
           -> (a -> Event ())
           -- ^ the main branch 
           -> (IOError -> Event ())
           -- ^ the branch for handing exceptions
           -> (() -> Event ())
           -- ^ the branch for cancellation
           -> ContCancellation
           -- ^ the cancellation token
           -> Bool
           -- ^ whether to support the exception catching
           -> Event ()
runCont (Cont m) cont econt ccont cancelToken catchFlag = 
  m ContParams { contCont = cont,
                 contAux  = 
                   ContParamsAux { contECont = econt,
                                   contCCont = ccont,
                                   contCancel     = cancelToken,
                                   contCancelFlag = contCancellationActivated cancelToken, 
                                   contCatchFlag  = catchFlag } }

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
               C.catch (m >>= writeIORef aref) 
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
resumeECont :: ContParams a -> IOException -> Event ()
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
contParallel :: [(Cont a, ContCancellation, Bool)]
                -- ^ the list of:
                -- the nested computation,
                -- the cancellation token,
                -- the catch flag
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
                    contCancellationBind (contCancel $ contAux c) $
                    flip map xs $ \(_, token, _) -> token
              let propagate =
                    Event $ \p ->
                    do n' <- readIORef counter
                       when (n' == n) $
                         do invokeEvent p hs  -- unbind the cancellation tokens
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
              forM_ (zip [1..n] xs) $ \(i, (x, cancelToken, catchFlag)) ->
                invokeEvent p $
                runCont x (cont i) econt ccont cancelToken (catchFlag || (contCatchFlag $ contAux c))
     z <- contCanceled c
     if z
       then cancelCont p c
       else if n == 0
            then invokeEvent p $ contCont c []
            else worker

-- | A partial case of 'contParallel' when we are not interested in
-- the results but we are interested in the actions to be peformed by
-- the nested computations.
contParallel_ :: [(Cont a, ContCancellation, Bool)]
                 -- ^ the list of:
                 -- the nested computation,
                 -- the cancellation token,
                 -- the catch flag
                 -> Cont ()
contParallel_ xs =
  Cont $ \c ->
  Event $ \p ->
  do let n = length xs
         worker =
           do counter   <- newIORef 0
              catchRef  <- newIORef Nothing
              hs <- invokeEvent p $
                    contCancellationBind (contCancel $ contAux c) $
                    flip map xs $ \(_, token, _) -> token
              let propagate =
                    Event $ \p ->
                    do n' <- readIORef counter
                       when (n' == n) $
                         do invokeEvent p hs  -- unbind the cancellation tokens
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
              forM_ (zip [1..n] xs) $ \(i, (x, cancelToken, catchFlag)) ->
                invokeEvent p $
                runCont x (cont i) econt ccont cancelToken (catchFlag || (contCatchFlag $ contAux c))
     z <- contCanceled c
     if z
       then cancelCont p c
       else if n == 0
            then invokeEvent p $ contCont c ()
            else worker

-- | Rerun the 'Cont' computation with the specified cancellation token and catch flag.
rerunCont :: Cont a -> ContCancellation -> Bool -> Cont a
rerunCont x cancelToken catchFlag =
  Cont $ \c ->
  Event $ \p ->
  do let worker =
           do hs <- invokeEvent p $
                    contCancellationBind (contCancel $ contAux c) [cancelToken]
              let cont a  =
                    Event $ \p ->
                    do invokeEvent p hs  -- unbind the cancellation token
                       invokeEvent p $ resumeCont c a
                  econt e =
                    Event $ \p ->
                    do invokeEvent p hs  -- unbind the cancellation token
                       invokeEvent p $ resumeECont c e
                  ccont e =
                    Event $ \p ->
                    do invokeEvent p hs  -- unbind the cancellation token
                       cancelCont p c
              invokeEvent p $
                runCont x cont econt ccont cancelToken (catchFlag || (contCatchFlag $ contAux c))
     z <- contCanceled c
     if z
       then cancelCont p c
       else worker
