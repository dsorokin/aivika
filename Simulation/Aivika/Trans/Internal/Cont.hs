
-- |
-- Module     : Simulation.Aivika.Trans.Internal.Cont
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The 'ContT' monad is a variation of the standard Cont monad 
-- and F# async workflow, where the result of applying 
-- the continuations is the 'EventT' computation.
--
module Simulation.Aivika.Trans.Internal.Cont
       (ContTCancellationSource,
        ContT(..),
        ContTParams,
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

import Control.Exception (IOException, throw)

import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.ProtoArray
import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.MonadSim
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
data ContTCancellationSource m =
  ContCancellationSource { contCancellationInitiatedRef :: ProtoRefT m Bool,
                           contCancellationActivatedRef :: ProtoRefT m Bool,
                           contCancellationInitiatingSource :: SignalTSource m ()
                         }

-- | Create the cancellation source.
newContCancellationSource :: MonadSim m => SimulationT m (ContTCancellationSource m)
{-# INLINE newContCancellationSource #-}
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
contCancellationInitiating :: ContTCancellationSource m -> SignalT m ()
{-# INLINE contCancellationInitiating #-}
contCancellationInitiating =
  publishSignal . contCancellationInitiatingSource

-- | Whether the cancellation was initiated.
contCancellationInitiated :: MonadSim m => ContTCancellationSource m -> (EventT m Bool)
{-# INLINE contCancellationInitiated #-}
contCancellationInitiated x =
  Event $ \p -> readProtoRef (contCancellationInitiatedRef x)

-- | Whether the cancellation was activated.
contCancellationActivated :: MonadSim m => ContTCancellationSource m -> m Bool
{-# INLINE contCancellationActivated #-}
contCancellationActivated =
  readProtoRef . contCancellationActivatedRef

-- | Deactivate the cancellation.
contCancellationDeactivate :: MonadSim m => ContTCancellationSource m -> m ()
{-# INLINE contCancellationDeactivate #-}
contCancellationDeactivate x =
  writeProtoRef (contCancellationActivatedRef x) False

-- | If the main computation is cancelled then all the nested ones will be cancelled too.
contCancellationBind :: MonadSim m => ContTCancellationSource m -> [ContTCancellationSource m] -> EventT m (DisposableEventT m)
{-# INLINABLE contCancellationBind #-}
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
contCancellationConnect :: MonadSim m
                           => ContTCancellationSource m
                           -- ^ the parent
                           -> ContCancellation
                           -- ^ how to connect
                           -> ContTCancellationSource m
                           -- ^ the child
                           -> EventT m (DisposableEventT m)
                           -- ^ computation of the disposable handler
{-# INLINABLE contCancellationConnect #-}
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
contCancellationInitiate :: MonadSim m => ContTCancellationSource m -> EventT m ()
{-# INLINE contCancellationInitiate #-}
contCancellationInitiate x =
  Event $ \p ->
  do f <- readProtoRef (contCancellationInitiatedRef x)
     unless f $
       do writeProtoRef (contCancellationInitiatedRef x) True
          writeProtoRef (contCancellationActivatedRef x) True
          invokeEvent p $ triggerSignal (contCancellationInitiatingSource x) ()

-- | The 'ContT' type is similar to the standard Cont monad 
-- and F# async workflow but only the result of applying
-- the continuations return the 'EventT' computation.
newtype ContT m a = Cont (ContTParams m a -> EventT m ())

-- | A convenient type synonym.
type Cont a = ContT IO a

-- | The continuation parameters.
data ContTParams m a = 
  ContParams { contCont :: a -> EventT m (), 
               contAux  :: ContTParamsAux m }

-- | The auxiliary continuation parameters.
data ContTParamsAux m =
  ContParamsAux { contECont :: IOException -> EventT m (),
                  contCCont :: () -> EventT m (),
                  contCancelSource :: ContTCancellationSource m,
                  contCancelFlag :: m Bool,
                  contCatchFlag  :: Bool }

instance MonadSim m => Monad (ContT m) where

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

instance MonadSimTrans ContT where

  {-# INLINE liftComp #-}
  liftComp m =
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching m p c
    else liftWithoutCatching m p c

instance ParameterLift ContT where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m $ pointRun p) p c
    else liftWithoutCatching (m $ pointRun p) p c

instance SimulationLift ContT where

  {-# INLINE liftSimulation #-}
  liftSimulation (Simulation m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m $ pointRun p) p c
    else liftWithoutCatching (m $ pointRun p) p c

instance DynamicsLift ContT where

  {-# INLINE liftDynamics #-}
  liftDynamics (Dynamics m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m p) p c
    else liftWithoutCatching (m p) p c

instance EventLift ContT where

  {-# INLINE liftEvent #-}
  liftEvent (Event m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m p) p c
    else liftWithoutCatching (m p) p c

instance (MonadSim m, MonadIO m) => MonadIO (ContT m) where

  {-# INLINE liftIO #-}
  liftIO m =
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (liftIO m) p c
    else liftWithoutCatching (liftIO m) p c

instance MonadSim m => Functor (ContT m) where

  {-# INLINE fmap #-}
  fmap = liftM

instance MonadSim m => Applicative (ContT m) where

  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) = ap

-- | Invoke the computation.
invokeCont :: ContTParams m a -> ContT m a -> EventT m ()
{-# INLINE invokeCont #-}
invokeCont p (Cont m) = m p

-- | Cancel the computation.
cancelCont :: MonadSim m => PointT m -> ContTParams m a -> m ()
{-# NOINLINE cancelCont #-}
cancelCont p c =
  do contCancellationDeactivate (contCancelSource $ contAux c)
     invokeEvent p $ (contCCont $ contAux c) ()

-- | Like @return a >>= k@.
callCont :: MonadSim m => (a -> ContT m b) -> a -> ContTParams m b -> EventT m ()
{-# INLINE callCont #-}
callCont k a c =
  Event $ \p ->
  do z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ invokeCont c (k a)

-- | Exception handling within 'ContT' computations.
catchCont :: MonadSim m => ContT m a -> (IOException -> ContT m a) -> ContT m a
{-# INLINABLE catchCont #-}
catchCont (Cont m) h = 
  Cont $ \c0 ->
  Event $ \p -> 
  do let c = c0 { contAux = (contAux c) { contCatchFlag = True } }
     z <- contCanceled c
     if z 
       then cancelCont p c
       else invokeEvent p $ m $
            let econt e = callCont h e c
            in c { contAux = (contAux c) { contECont = econt } }
               
-- | A computation with finalization part.
finallyCont :: MonadSim m => ContT m a -> ContT m b -> ContT m a
{-# INLINABLE finallyCont #-}
finallyCont (Cont m) (Cont m') = 
  Cont $ \c0 -> 
  Event $ \p ->
  do let c = c0 { contAux = (contAux c) { contCatchFlag = True } }
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
-- By some reasons, the standard 'throw' function per se is not handled 
-- properly within 'ContT' computations, altough it will be still handled 
-- if it will be hidden under the 'liftIO' function. The problem arises 
-- namely with the @throw@ function, not 'IO' computations.
throwCont :: (MonadSim m, MonadIO m) => IOException -> ContT m a
{-# INLINABLE throwCont #-}
throwCont e = liftIO $ throw e

-- | Run the 'Cont' computation with the specified cancelation source 
-- and flag indicating whether to catch exceptions from the beginning.
runCont :: MonadSim m
           => ContT m a
           -- ^ the computation to run
           -> (a -> EventT m ())
           -- ^ the main branch 
           -> (IOError -> EventT m ())
           -- ^ the branch for handing exceptions
           -> (() -> EventT m ())
           -- ^ the branch for cancellation
           -> ContTCancellationSource m
           -- ^ the cancellation source
           -> Bool
           -- ^ whether to support the exception handling from the beginning
           -> EventT m ()
{-# INLINE runCont #-}
runCont (Cont m) cont econt ccont cancelSource catchFlag = 
  m ContParams { contCont = cont,
                 contAux  = 
                   ContParamsAux { contECont = econt,
                                   contCCont = ccont,
                                   contCancelSource = cancelSource,
                                   contCancelFlag = contCancellationActivated cancelSource, 
                                   contCatchFlag  = catchFlag } }
  
liftWithoutCatching :: MonadSim m => m a -> PointT m -> ContTParams m a -> m ()
{-# INLINE liftWithoutCatching #-}
liftWithoutCatching m p c =
  do z <- contCanceled c
     if z
       then cancelCont p c
       else do a <- m
               invokeEvent p $ contCont c a

liftWithCatching :: MonadSim m => m a -> PointT m -> ContTParams m a -> m ()
{-# NOINLINE liftWithCatching #-}
liftWithCatching m p c =
  do z <- contCanceled c
     if z
       then cancelCont p c
       else do let s = runSession $ pointRun p
               aref <- newProtoRef s undefined
               eref <- newProtoRef s Nothing
               catchComputation
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
resumeCont :: MonadSim m => ContTParams m a -> a -> EventT m ()
{-# INLINE resumeCont #-}
resumeCont c a = 
  Event $ \p ->
  do z <- contCanceled c
     if z
       then cancelCont p c
       else invokeEvent p $ contCont c a

-- | Resume the exception handling by the specified parameters.
resumeECont :: MonadSim m => ContTParams m a -> IOException -> EventT m ()
{-# INLINE resumeECont #-}
resumeECont c e = 
  Event $ \p ->
  do z <- contCanceled c
     if z
       then cancelCont p c
       else invokeEvent p $ (contECont $ contAux c) e

-- | Test whether the computation is canceled.
contCanceled :: ContTParams m a -> m Bool
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
contParallel :: MonadSim m
                => [(ContT m a, ContTCancellationSource m)]
                -- ^ the list of pairs:
                -- the nested computation,
                -- the cancellation source
                -> ContT m [a]
{-# INLINABLE contParallel #-}
contParallel xs =
  Cont $ \c ->
  Event $ \p ->
  do let n = length xs
         s = runSession $ pointRun p
         worker =
           do results   <- newProtoArray_ s (1, n)
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
                                do rs <- protoArrayElems results
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
contParallel_ :: MonadSim m
                 => [(ContT m a, ContTCancellationSource m)]
                 -- ^ the list of pairs:
                 -- the nested computation,
                 -- the cancellation source
                 -> ContT m ()
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
              forM_ (zip [1..n] xs) $ \(i, (x, cancelSource)) ->
                invokeEvent p $
                runCont x (cont i) econt ccont cancelSource (contCatchFlag $ contAux c)
     z <- contCanceled c
     if z
       then cancelCont p c
       else if n == 0
            then invokeEvent p $ contCont c ()
            else worker

-- | Rerun the 'ContT' computation with the specified cancellation source.
rerunCont :: MonadSim m => ContT m a -> ContTCancellationSource m -> ContT m a
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

-- | Run the 'ContT' computation in parallel but connect the cancellation sources.
spawnCont :: MonadEnq m => ContCancellation -> ContT m () -> ContTCancellationSource m -> ContT m ()
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
contFreeze :: MonadEnq m => ContTParams m a -> EventT m (EventT m (Maybe (ContTParams m a)))
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
contAwait :: MonadEnq m => SignalT m a -> ContT m a
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
