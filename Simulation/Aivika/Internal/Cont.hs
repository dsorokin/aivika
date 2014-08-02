
-- |
-- Module     : Simulation.Aivika.Internal.Cont
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The 'Cont' monad is a variation of the standard Cont monad 
-- and F# async workflow, where the result of applying 
-- the continuations is the 'Event' computation.
--
module Simulation.Aivika.Internal.Cont
       (ContCancellation(..),
        ContCancellationSource,
        Cont(..),
        ContParams,
        newContCancellationSource,
        contCancellationInitiated,
        contCancellationInitiate,
        contCancellationInitiating,
        contCancellationBind,
        contCancellationConnect,
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
        contAwait) where

import Data.IORef
import Data.Array
import Data.Array.IO.Safe
import Data.Monoid

import qualified Control.Exception as C
import Control.Exception (IOException, throw)

import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Signal

-- | It defines how the parent and child computations should be cancelled.
data ContCancellation = CancelTogether
                        -- ^ Cancel the both computations together.
                      | CancelChildAfterParent
                        -- ^ Cancel the child if its parent is cancelled.
                      | CancelParentAfterChild
                        -- ^ Cancel the parent if its child is cancelled.
                      | CancelInIsolation
                        -- ^ Cancel the computations in isolation.

-- | It manages the cancellation process.
data ContCancellationSource =
  ContCancellationSource { contCancellationInitiatedRef :: IORef Bool,
                           contCancellationActivatedRef :: IORef Bool,
                           contCancellationInitiatingSource :: SignalSource ()
                         }

-- | Create the cancellation source.
newContCancellationSource :: Simulation ContCancellationSource
newContCancellationSource =
  Simulation $ \r ->
  do r1 <- newIORef False
     r2 <- newIORef False
     s  <- invokeSimulation r newSignalSource
     return ContCancellationSource { contCancellationInitiatedRef = r1,
                                     contCancellationActivatedRef = r2,
                                     contCancellationInitiatingSource = s
                                   }

-- | Signal when the cancellation is intiating.
contCancellationInitiating :: ContCancellationSource -> Signal ()
contCancellationInitiating =
  publishSignal . contCancellationInitiatingSource

-- | Whether the cancellation was initiated.
contCancellationInitiated :: ContCancellationSource -> Event Bool
contCancellationInitiated x =
  Event $ \p -> readIORef (contCancellationInitiatedRef x)

-- | Whether the cancellation was activated.
contCancellationActivated :: ContCancellationSource -> IO Bool
contCancellationActivated =
  readIORef . contCancellationActivatedRef

-- | Deactivate the cancellation.
contCancellationDeactivate :: ContCancellationSource -> IO ()
contCancellationDeactivate x =
  writeIORef (contCancellationActivatedRef x) False

-- | If the main computation is cancelled then all the nested ones will be cancelled too.
contCancellationBind :: ContCancellationSource -> [ContCancellationSource] -> Event (Event ())
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

-- | Connect the parent computation to the child one.
contCancellationConnect :: ContCancellationSource
                           -- ^ the parent
                           -> ContCancellation
                           -- ^ how to connect
                           -> ContCancellationSource
                           -- ^ the child
                           -> Event (Event ())
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
         CancelParentAfterChild -> return $ return ()
         CancelInIsolation -> return $ return ()
     h2 <-
       case cancellation of
         CancelTogether -> invokeEvent p m2
         CancelChildAfterParent -> return $ return ()
         CancelParentAfterChild -> invokeEvent p m2
         CancelInIsolation -> return $ return ()
     return $ h1 >> h2

-- | Initiate the cancellation.
contCancellationInitiate :: ContCancellationSource -> Event ()
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
                  contCancelSource :: ContCancellationSource,
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

invokeCont :: ContParams a -> Cont a -> Event ()
{-# INLINE invokeCont #-}
invokeCont p (Cont m) = m p

cancelCont :: Point -> ContParams a -> IO ()
{-# NOINLINE cancelCont #-}
cancelCont p c =
  do contCancellationDeactivate (contCancelSource $ contAux c)
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

-- -- It is not tail recursive!
-- bindWithCatch :: Cont a -> (a -> Cont b) -> ContParams b -> Event ()
-- {-# NOINLINE bindWithCatch #-}
-- bindWithCatch (Cont m) k c = 
--   Event $ \p ->
--   do z <- contCanceled c
--      if z 
--        then cancelCont p c
--        else invokeEvent p $ m $ 
--             let cont a = catchEvent 
--                          (invokeCont c (k a))
--                          (contECont $ contAux c)
--             in c { contCont = cont }

-- Like "bindWithoutCatch (return a) k"
callWithoutCatch :: (a -> Cont b) -> a -> ContParams b -> Event ()
callWithoutCatch k a c =
  Event $ \p ->
  do z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ invokeCont c (k a)

-- -- Like "bindWithCatch (return a) k" but it is not tail recursive!
-- callWithCatch :: (a -> Cont b) -> a -> ContParams b -> Event ()
-- callWithCatch k a c =
--   Event $ \p ->
--   do z <- contCanceled c
--      if z 
--        then cancelCont p c
--        else invokeEvent p $ catchEvent 
--             (invokeCont c (k a))
--             (contECont $ contAux c)

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

-- | Run the 'Cont' computation with the specified cancelation source 
-- and flag indicating whether to catch exceptions from the beginning.
runCont :: Cont a
           -- ^ the computation to run
           -> (a -> Event ())
           -- ^ the main branch 
           -> (IOError -> Event ())
           -- ^ the branch for handing exceptions
           -> (() -> Event ())
           -- ^ the branch for cancellation
           -> ContCancellationSource
           -- ^ the cancellation source
           -> Bool
           -- ^ whether to support the exception handling from the beginning
           -> Event ()
runCont (Cont m) cont econt ccont cancelSource catchFlag = 
  m ContParams { contCont = cont,
                 contAux  = 
                   ContParamsAux { contECont = econt,
                                   contCCont = ccont,
                                   contCancelSource = cancelSource,
                                   contCancelFlag = contCancellationActivated cancelSource, 
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
contParallel :: [(Cont a, ContCancellationSource)]
                -- ^ the list of:
                -- the nested computation,
                -- the cancellation source
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
                    contCancellationBind (contCancelSource $ contAux c) $
                    map snd xs
              let propagate =
                    Event $ \p ->
                    do n' <- readIORef counter
                       when (n' == n) $
                         do invokeEvent p hs  -- unbind the cancellation sources
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
              forM_ (zip [1..n] xs) $ \(i, (x, cancelSource)) ->
                invokeEvent p $
                runCont x (cont i) econt ccont cancelSource (contCatchFlag $ contAux c)
     z <- contCanceled c
     if z
       then cancelCont p c
       else if n == 0
            then invokeEvent p $ contCont c []
            else worker

-- | A partial case of 'contParallel' when we are not interested in
-- the results but we are interested in the actions to be peformed by
-- the nested computations.
contParallel_ :: [(Cont a, ContCancellationSource)]
                 -- ^ the list of:
                 -- the nested computation,
                 -- the cancellation source
                 -> Cont ()
contParallel_ xs =
  Cont $ \c ->
  Event $ \p ->
  do let n = length xs
         worker =
           do counter   <- newIORef 0
              catchRef  <- newIORef Nothing
              hs <- invokeEvent p $
                    contCancellationBind (contCancelSource $ contAux c) $
                    map snd xs
              let propagate =
                    Event $ \p ->
                    do n' <- readIORef counter
                       when (n' == n) $
                         do invokeEvent p hs  -- unbind the cancellation sources
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
              forM_ (zip [1..n] xs) $ \(i, (x, cancelSource)) ->
                invokeEvent p $
                runCont x (cont i) econt ccont cancelSource (contCatchFlag $ contAux c)
     z <- contCanceled c
     if z
       then cancelCont p c
       else if n == 0
            then invokeEvent p $ contCont c ()
            else worker

-- | Rerun the 'Cont' computation with the specified cancellation source.
rerunCont :: Cont a -> ContCancellationSource -> Cont a
rerunCont x cancelSource =
  Cont $ \c ->
  Event $ \p ->
  do let worker =
           do hs <- invokeEvent p $
                    contCancellationBind (contCancelSource $ contAux c) [cancelSource]
              let cont a  =
                    Event $ \p ->
                    do invokeEvent p hs  -- unbind the cancellation source
                       invokeEvent p $ resumeCont c a
                  econt e =
                    Event $ \p ->
                    do invokeEvent p hs  -- unbind the cancellation source
                       invokeEvent p $ resumeECont c e
                  ccont e =
                    Event $ \p ->
                    do invokeEvent p hs  -- unbind the cancellation source
                       cancelCont p c
              invokeEvent p $
                runCont x cont econt ccont cancelSource (contCatchFlag $ contAux c)
     z <- contCanceled c
     if z
       then cancelCont p c
       else worker

-- | Run the 'Cont' computation in parallel but connect the cancellation sources.
spawnCont :: ContCancellation -> Cont () -> ContCancellationSource -> Cont ()
spawnCont cancellation x cancelSource =
  Cont $ \c ->
  Event $ \p ->
  do let worker =
           do hs <- invokeEvent p $
                    contCancellationConnect
                    (contCancelSource $ contAux c) cancellation cancelSource
              let cont a  =
                    Event $ \p ->
                    do invokeEvent p hs  -- unbind the cancellation source
                       -- do nothing and it will finish the computation
                  econt e =
                    Event $ \p ->
                    do invokeEvent p hs  -- unbind the cancellation source
                       invokeEvent p $ throwEvent e  -- this is all we can do
                  ccont e =
                    Event $ \p ->
                    do invokeEvent p hs  -- unbind the cancellation source
                       -- do nothing and it will finish the computation
              invokeEvent p $
                enqueueEvent (pointTime p) $
                runCont x cont econt ccont cancelSource False
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
                        contCancelSource $
                        contAux c) $ \a ->
          Event $ \p ->
          do h <- readIORef rh
             case h of
               Nothing ->
                 error "The handler was lost: contFreeze."
               Just h ->
                 do invokeEvent p h
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
       do invokeEvent p h
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
                             error "The signal was lost: awaitSignal."
                           Just x ->
                             do invokeEvent p x
                                c <- invokeEvent p c
                                case c of
                                  Nothing -> return ()
                                  Just c  ->
                                    invokeEvent p $ resumeCont c a
     writeIORef r $ Just h          
