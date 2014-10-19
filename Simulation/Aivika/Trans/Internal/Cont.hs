
-- |
-- Module     : Simulation.Aivika.Trans.Internal.Cont
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The 'Cont' monad is a variation of the standard Cont monad 
-- and F# async workflow, where the result of applying 
-- the continuations is the 'Event' computation.
--
module Simulation.Aivika.Trans.Internal.Cont
       (ContCancellationSource,
        ContParams,
        ContCancellation(..),
        Cont(..),
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

import Data.Array
import Data.Monoid

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.ProtoArray
import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Signal

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
data ContCancellationSource m =
  ContCancellationSource { contCancellationInitiatedRef :: ProtoRef m Bool,
                           contCancellationActivatedRef :: ProtoRef m Bool,
                           contCancellationInitiatingSource :: SignalSource m ()
                         }

-- | Create the cancellation source.
newContCancellationSource :: Comp m => Simulation m (ContCancellationSource m)
{-# INLINABLE newContCancellationSource #-}
{-# SPECIALISE newContCancellationSource :: Simulation IO (ContCancellationSource IO) #-}
newContCancellationSource =
  Simulation $ \r ->
  do let sn = runSession r
     r1 <- newProtoRef sn False
     r2 <- newProtoRef sn False
     s  <- invokeSimulation r newSignalSource
     return ContCancellationSource { contCancellationInitiatedRef = r1,
                                     contCancellationActivatedRef = r2,
                                     contCancellationInitiatingSource = s
                                   }

-- | Signal when the cancellation is intiating.
contCancellationInitiating :: ContCancellationSource m -> Signal m ()
{-# INLINE contCancellationInitiating #-}
contCancellationInitiating =
  publishSignal . contCancellationInitiatingSource

-- | Whether the cancellation was initiated.
contCancellationInitiated :: Comp m => ContCancellationSource m -> (Event m Bool)
{-# INLINABLE contCancellationInitiated #-}
{-# SPECIALISE contCancellationInitiated :: ContCancellationSource IO -> Event IO Bool #-}
contCancellationInitiated x =
  Event $ \p -> readProtoRef (contCancellationInitiatedRef x)

-- | Whether the cancellation was activated.
contCancellationActivated :: Comp m => ContCancellationSource m -> m Bool
{-# INLINABLE contCancellationActivated #-}
{-# SPECIALISE contCancellationActivated :: ContCancellationSource IO -> IO Bool #-}
contCancellationActivated =
  readProtoRef . contCancellationActivatedRef

-- | Deactivate the cancellation.
contCancellationDeactivate :: Comp m => ContCancellationSource m -> m ()
{-# INLINABLE contCancellationDeactivate #-}
{-# SPECIALISE contCancellationDeactivate :: ContCancellationSource IO -> IO () #-}
contCancellationDeactivate x =
  writeProtoRef (contCancellationActivatedRef x) False

-- | If the main computation is cancelled then all the nested ones will be cancelled too.
contCancellationBind :: Comp m => ContCancellationSource m -> [ContCancellationSource m] -> Event m (DisposableEvent m)
{-# INLINABLE contCancellationBind #-}
{-# SPECIALISE contCancellationBind :: ContCancellationSource IO -> [ContCancellationSource IO] -> Event IO (DisposableEvent IO) #-}
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
contCancellationConnect :: Comp m
                           => ContCancellationSource m
                           -- ^ the parent
                           -> ContCancellation
                           -- ^ how to connect
                           -> ContCancellationSource m
                           -- ^ the child
                           -> Event m (DisposableEvent m)
                           -- ^ computation of the disposable handler
{-# INLINABLE contCancellationConnect #-}
{-# SPECIALISE contCancellationConnect :: ContCancellationSource IO -> ContCancellation -> ContCancellationSource IO -> Event IO (DisposableEvent IO) #-}
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
contCancellationInitiate :: Comp m => ContCancellationSource m -> Event m ()
{-# INLINABLE contCancellationInitiate #-}
{-# SPECIALISE contCancellationInitiate :: ContCancellationSource IO -> Event IO () #-}
contCancellationInitiate x =
  Event $ \p ->
  do f <- readProtoRef (contCancellationInitiatedRef x)
     unless f $
       do writeProtoRef (contCancellationInitiatedRef x) True
          writeProtoRef (contCancellationActivatedRef x) True
          invokeEvent p $ triggerSignal (contCancellationInitiatingSource x) ()

-- | The 'Cont' type is similar to the standard Cont monad 
-- and F# async workflow but only the result of applying
-- the continuations return the 'Event' computation.
newtype Cont m a = Cont (ContParams m a -> Event m ())

-- | The continuation parameters.
data ContParams m a = 
  ContParams { contCont :: a -> Event m (), 
               contAux  :: ContParamsAux m }

-- | The auxiliary continuation parameters.
data ContParamsAux m =
  ContParamsAux { contECont :: SomeException -> Event m (),
                  contCCont :: () -> Event m (),
                  contCancelSource :: ContCancellationSource m,
                  contCancelFlag :: m Bool,
                  contCatchFlag  :: Bool }

instance Comp m => Monad (Cont m) where

  {-# INLINE return #-}
  return a = 
    Cont $ \c ->
    Event $ \p ->
    do z <- contCanceled c
       if z 
         then cancelCont p c
         else invokeEvent p $ contCont c a

  {-# INLINE (>>=) #-}
  (Cont m) >>= k =
    Cont $ \c ->
    Event $ \p ->
    do z <- contCanceled c
       if z 
         then cancelCont p c
         else invokeEvent p $ m $ 
              let cont a = invokeCont c (k a)
              in c { contCont = cont }

instance CompTrans Cont where

  {-# INLINE liftComp #-}
  liftComp m =
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching m p c
    else liftWithoutCatching m p c

instance ParameterLift Cont where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m $ pointRun p) p c
    else liftWithoutCatching (m $ pointRun p) p c

instance SimulationLift Cont where

  {-# INLINE liftSimulation #-}
  liftSimulation (Simulation m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m $ pointRun p) p c
    else liftWithoutCatching (m $ pointRun p) p c

instance DynamicsLift Cont where

  {-# INLINE liftDynamics #-}
  liftDynamics (Dynamics m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m p) p c
    else liftWithoutCatching (m p) p c

instance EventLift Cont where

  {-# INLINE liftEvent #-}
  liftEvent (Event m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m p) p c
    else liftWithoutCatching (m p) p c

instance (Comp m, MonadIO m) => MonadIO (Cont m) where

  {-# INLINE liftIO #-}
  liftIO m =
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (liftIO m) p c
    else liftWithoutCatching (liftIO m) p c

instance Comp m => Functor (Cont m) where

  {-# INLINE fmap #-}
  fmap = liftM

instance Comp m => Applicative (Cont m) where

  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) = ap

-- | Invoke the computation.
invokeCont :: ContParams m a -> Cont m a -> Event m ()
{-# INLINE invokeCont #-}
invokeCont p (Cont m) = m p

-- | Cancel the computation.
cancelCont :: Comp m => Point m -> ContParams m a -> m ()
{-# NOINLINE cancelCont #-}
cancelCont p c =
  do contCancellationDeactivate (contCancelSource $ contAux c)
     invokeEvent p $ (contCCont $ contAux c) ()

-- | Like @return a >>= k@.
callCont :: Comp m => (a -> Cont m b) -> a -> ContParams m b -> Event m ()
{-# INLINE callCont #-}
callCont k a c =
  Event $ \p ->
  do z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ invokeCont c (k a)

-- | Exception handling within 'Cont' computations.
catchCont :: (Comp m, Exception e) => Cont m a -> (e -> Cont m a) -> Cont m a
{-# INLINABLE catchCont #-}
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
finallyCont :: Comp m => Cont m a -> Cont m b -> Cont m a
{-# INLINABLE finallyCont #-}
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
throwCont :: (Comp m, Exception e) => e -> Cont m a
{-# INLINABLE throwCont #-}
throwCont = liftEvent . throwEvent

-- | Run the 'Cont' computation with the specified cancelation source 
-- and flag indicating whether to catch exceptions from the beginning.
runCont :: Comp m
           => Cont m a
           -- ^ the computation to run
           -> (a -> Event m ())
           -- ^ the main branch 
           -> (SomeException -> Event m ())
           -- ^ the branch for handing exceptions
           -> (() -> Event m ())
           -- ^ the branch for cancellation
           -> ContCancellationSource m
           -- ^ the cancellation source
           -> Bool
           -- ^ whether to support the exception handling from the beginning
           -> Event m ()
{-# INLINE runCont #-}
runCont (Cont m) cont econt ccont cancelSource catchFlag = 
  m ContParams { contCont = cont,
                 contAux  = 
                   ContParamsAux { contECont = econt,
                                   contCCont = ccont,
                                   contCancelSource = cancelSource,
                                   contCancelFlag = contCancellationActivated cancelSource, 
                                   contCatchFlag  = catchFlag } }
  
liftWithoutCatching :: Comp m => m a -> Point m -> ContParams m a -> m ()
{-# INLINE liftWithoutCatching #-}
liftWithoutCatching m p c =
  do z <- contCanceled c
     if z
       then cancelCont p c
       else do a <- m
               invokeEvent p $ contCont c a

liftWithCatching :: Comp m => m a -> Point m -> ContParams m a -> m ()
{-# NOINLINE liftWithCatching #-}
liftWithCatching m p c =
  do z <- contCanceled c
     if z
       then cancelCont p c
       else do let s = runSession $ pointRun p
               aref <- newProtoRef s undefined
               eref <- newProtoRef s Nothing
               catchComp
                 (m >>= writeProtoRef aref) 
                 (writeProtoRef eref . Just)
               e <- readProtoRef eref
               case e of
                 Nothing -> 
                   do a <- readProtoRef aref
                      -- tail recursive
                      invokeEvent p $ contCont c a
                 Just e ->
                   -- tail recursive
                   invokeEvent p $ (contECont . contAux) c e

-- | Resume the computation by the specified parameters.
resumeCont :: Comp m => ContParams m a -> a -> Event m ()
{-# INLINE resumeCont #-}
resumeCont c a = 
  Event $ \p ->
  do z <- contCanceled c
     if z
       then cancelCont p c
       else invokeEvent p $ contCont c a

-- | Resume the exception handling by the specified parameters.
resumeECont :: Comp m => ContParams m a -> SomeException -> Event m ()
{-# INLINE resumeECont #-}
resumeECont c e = 
  Event $ \p ->
  do z <- contCanceled c
     if z
       then cancelCont p c
       else invokeEvent p $ (contECont $ contAux c) e

-- | Test whether the computation is canceled.
contCanceled :: ContParams m a -> m Bool
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
contParallel :: Comp m
                => [(Cont m a, ContCancellationSource m)]
                -- ^ the list of pairs:
                -- the nested computation,
                -- the cancellation source
                -> Cont m [a]
{-# INLINABLE contParallel #-}
contParallel xs =
  Cont $ \c ->
  Event $ \p ->
  do let n = length xs
         s = runSession $ pointRun p
         worker =
           do results   <- newProtoArray_ s n
              counter   <- newProtoRef s 0
              catchRef  <- newProtoRef s Nothing
              hs <- invokeEvent p $
                    contCancellationBind (contCancelSource $ contAux c) $
                    map snd xs
              let propagate =
                    Event $ \p ->
                    do n' <- readProtoRef counter
                       when (n' == n) $
                         do invokeEvent p $ disposeEvent hs  -- unbind the cancellation sources
                            f1 <- contCanceled c
                            f2 <- readProtoRef catchRef
                            case (f1, f2) of
                              (False, Nothing) ->
                                do rs <- protoArrayToList results
                                   invokeEvent p $ resumeCont c rs
                              (False, Just e) ->
                                invokeEvent p $ resumeECont c e
                              (True, _) ->
                                cancelCont p c
                  cont i a =
                    Event $ \p ->
                    do modifyProtoRef counter (+ 1)
                       writeProtoArray results i a
                       invokeEvent p propagate
                  econt e =
                    Event $ \p ->
                    do modifyProtoRef counter (+ 1)
                       r <- readProtoRef catchRef
                       case r of
                         Nothing -> writeProtoRef catchRef $ Just e
                         Just e' -> return ()  -- ignore the next error
                       invokeEvent p propagate
                  ccont e =
                    Event $ \p ->
                    do modifyProtoRef counter (+ 1)
                       -- the main computation was automatically canceled
                       invokeEvent p propagate
              forM_ (zip [0..n-1] xs) $ \(i, (x, cancelSource)) ->
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
contParallel_ :: Comp m
                 => [(Cont m a, ContCancellationSource m)]
                 -- ^ the list of pairs:
                 -- the nested computation,
                 -- the cancellation source
                 -> Cont m ()
{-# INLINABLE contParallel_ #-}
contParallel_ xs =
  Cont $ \c ->
  Event $ \p ->
  do let n = length xs
         s = runSession $ pointRun p
         worker =
           do counter   <- newProtoRef s 0
              catchRef  <- newProtoRef s Nothing
              hs <- invokeEvent p $
                    contCancellationBind (contCancelSource $ contAux c) $
                    map snd xs
              let propagate =
                    Event $ \p ->
                    do n' <- readProtoRef counter
                       when (n' == n) $
                         do invokeEvent p $ disposeEvent hs  -- unbind the cancellation sources
                            f1 <- contCanceled c
                            f2 <- readProtoRef catchRef
                            case (f1, f2) of
                              (False, Nothing) ->
                                invokeEvent p $ resumeCont c ()
                              (False, Just e) ->
                                invokeEvent p $ resumeECont c e
                              (True, _) ->
                                cancelCont p c
                  cont i a =
                    Event $ \p ->
                    do modifyProtoRef counter (+ 1)
                       -- ignore the result
                       invokeEvent p propagate
                  econt e =
                    Event $ \p ->
                    do modifyProtoRef counter (+ 1)
                       r <- readProtoRef catchRef
                       case r of
                         Nothing -> writeProtoRef catchRef $ Just e
                         Just e' -> return ()  -- ignore the next error
                       invokeEvent p propagate
                  ccont e =
                    Event $ \p ->
                    do modifyProtoRef counter (+ 1)
                       -- the main computation was automatically canceled
                       invokeEvent p propagate
              forM_ (zip [0..n-1] xs) $ \(i, (x, cancelSource)) ->
                invokeEvent p $
                runCont x (cont i) econt ccont cancelSource (contCatchFlag $ contAux c)
     z <- contCanceled c
     if z
       then cancelCont p c
       else if n == 0
            then invokeEvent p $ contCont c ()
            else worker

-- | Rerun the 'Cont' computation with the specified cancellation source.
rerunCont :: Comp m => Cont m a -> ContCancellationSource m -> Cont m a
{-# INLINABLE rerunCont #-}
rerunCont x cancelSource =
  Cont $ \c ->
  Event $ \p ->
  do let worker =
           do hs <- invokeEvent p $
                    contCancellationBind (contCancelSource $ contAux c) [cancelSource]
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
                runCont x cont econt ccont cancelSource (contCatchFlag $ contAux c)
     z <- contCanceled c
     if z
       then cancelCont p c
       else worker

-- | Run the 'Cont' computation in parallel but connect the cancellation sources.
spawnCont :: Comp m => ContCancellation -> Cont m () -> ContCancellationSource m -> Cont m ()
{-# INLINABLE spawnCont #-}
spawnCont cancellation x cancelSource =
  Cont $ \c ->
  Event $ \p ->
  do let worker =
           do hs <- invokeEvent p $
                    contCancellationConnect
                    (contCancelSource $ contAux c) cancellation cancelSource
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
                runCont x cont econt ccont cancelSource False
              invokeEvent p $
                resumeCont c ()
     z <- contCanceled c
     if z
       then cancelCont p c
       else worker

-- | Freeze the computation parameters temporarily.
contFreeze :: Comp m => ContParams m a -> Event m (Event m (Maybe (ContParams m a)))
{-# INLINABLE contFreeze #-}
contFreeze c =
  Event $ \p ->
  do let s = runSession $ pointRun p
     rh <- newProtoRef s Nothing
     rc <- newProtoRef s $ Just c
     h <- invokeEvent p $
          handleSignal (contCancellationInitiating $
                        contCancelSource $
                        contAux c) $ \a ->
          Event $ \p ->
          do h <- readProtoRef rh
             case h of
               Nothing ->
                 error "The handler was lost: contFreeze."
               Just h ->
                 do invokeEvent p $ disposeEvent h
                    c <- readProtoRef rc
                    case c of
                      Nothing -> return ()
                      Just c  ->
                        do writeProtoRef rc Nothing
                           invokeEvent p $
                             enqueueEvent (pointTime p) $
                             Event $ \p ->
                             do z <- contCanceled c
                                when z $ cancelCont p c
     writeProtoRef rh (Just h)
     return $
       Event $ \p ->
       do invokeEvent p $ disposeEvent h
          c <- readProtoRef rc
          writeProtoRef rc Nothing
          return c
     
-- | Await the signal.
contAwait :: Comp m => Signal m a -> Cont m a
{-# INLINABLE contAwait #-}
contAwait signal =
  Cont $ \c ->
  Event $ \p ->
  do let s = runSession $ pointRun p
     c <- invokeEvent p $ contFreeze c
     r <- newProtoRef s Nothing
     h <- invokeEvent p $
          handleSignal signal $ 
          \a -> Event $ 
                \p -> do x <- readProtoRef r
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
     writeProtoRef r $ Just h          
