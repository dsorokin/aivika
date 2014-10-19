
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Event
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'Event' monad transformer which is very similar to the 'Dynamics'
-- monad transformer but only now the computation is strongly synchronized with the event queue.
--
module Simulation.Aivika.Trans.Internal.Event
       (-- * Event Monad
        EventLift(..),
        runEventInStartTime,
        runEventInStopTime,
        -- * Event Queue
        enqueueEventWithCancellation,
        enqueueEventWithTimes,
        enqueueEventWithPoints,
        enqueueEventWithIntegTimes,
        yieldEvent,
        -- * Cancelling Event
        EventCancellation,
        cancelEvent,
        eventCancelled,
        eventFinished,
        -- * Error Handling
        catchEvent,
        finallyEvent,
        throwEvent,
        -- * Memoization
        memoEvent,
        memoEventInTime,
        -- * Disposable
        DisposableEvent(..)) where

import Data.Monoid

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics

instance Monad m => Monad (Event m) where

  {-# SPECIALISE INLINE return :: a -> Event IO a #-}
  return a = Event $ \p -> return a

  {-# SPECIALISE INLINE (>>=) :: Event IO a -> (a -> Event IO b) -> Event IO b #-}
  (Event m) >>= k =
    Event $ \p -> 
    do a <- m p
       let Event m' = k a
       m' p

instance Functor m => Functor (Event m) where
  
  {-# SPECIALISE INLINE fmap :: (a -> b) -> Event IO a -> Event IO b #-}
  fmap f (Event x) = Event $ \p -> fmap f $ x p

instance Applicative m => Applicative (Event m) where
  
  {-# SPECIALISE INLINE pure :: a -> Event IO a #-}
  pure = Event . const . pure
  
  {-# SPECIALISE INLINE (<*>) :: Event IO (a -> b) -> Event IO a -> Event IO b #-}
  (Event x) <*> (Event y) = Event $ \p -> x p <*> y p

instance MonadTrans Event where

  {-# INLINE lift #-}
  lift = Event . const

instance MonadIO m => MonadIO (Event m) where
  
  {-# INLINE liftIO #-}
  liftIO = Event . const . liftIO

instance CompTrans Event where

  {-# INLINE liftComp #-}
  liftComp = Event . const

-- | A type class to lift the 'Event' computations into other computations.
class EventLift t where
  
  -- | Lift the specified 'Event' computation into another computation.
  liftEvent :: Comp m => Event m a -> t m a

instance EventLift Event where
  
  {-# INLINE liftEvent #-}
  liftEvent = id

instance DynamicsLift Event where
  
  {-# INLINE liftDynamics #-}
  liftDynamics (Dynamics x) = Event x

instance SimulationLift Event where

  {-# INLINE liftSimulation #-}
  liftSimulation (Simulation x) = Event $ x . pointRun 

instance ParameterLift Event where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter x) = Event $ x . pointRun

-- | Exception handling within 'Event' computations.
catchEvent :: (Comp m, Exception e) => Event m a -> (e -> Event m a) -> Event m a
{-# INLINABLE catchEvent #-}
{-# SPECIALISE catchEvent :: Exception e => Event IO a -> (e -> Event IO a) -> Event IO a #-}
catchEvent (Event m) h =
  Event $ \p -> 
  catchComp (m p) $ \e ->
  let Event m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyEvent :: Comp m => Event m a -> Event m b -> Event m a
{-# INLINABLE finallyEvent #-}
{-# SPECIALISE finallyEvent :: Event IO a -> Event IO b -> Event IO a #-}
finallyEvent (Event m) (Event m') =
  Event $ \p ->
  finallyComp (m p) (m' p)

-- | Like the standard 'throw' function.
throwEvent :: (Comp m, Exception e) => e -> Event m a
{-# INLINABLE throwEvent #-}
{-# SPECIALISE throwEvent :: Exception e => e -> Event IO a #-}
throwEvent = throw

instance MonadFix m => MonadFix (Event m) where

  {-# SPECIALISE INLINE mfix :: (a -> Event IO a) -> Event IO a #-}
  mfix f = 
    Event $ \p ->
    do { rec { a <- invokeEvent p (f a) }; return a }

-- | Run the 'Event' computation in the start time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStartTime :: Comp m => Event m a -> Simulation m a
{-# INLINABLE runEventInStartTime #-}
{-# SPECIALISE runEventInStartTime :: Event IO a -> Simulation IO a #-}
runEventInStartTime = runDynamicsInStartTime . runEvent

-- | Run the 'Event' computation in the stop time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStopTime :: Comp m => Event m a -> Simulation m a
{-# INLINABLE runEventInStopTime #-}
{-# SPECIALISE runEventInStopTime :: Event IO a -> Simulation IO a #-}
runEventInStopTime = runDynamicsInStopTime . runEvent

-- | Actuate the event handler in the specified time points.
enqueueEventWithTimes :: Comp m => [Double] -> Event m () -> Event m ()
{-# INLINABLE enqueueEventWithTimes #-}
{-# SPECIALISE enqueueEventWithTimes :: [Double] -> Event IO () -> Event IO () #-}
enqueueEventWithTimes ts e = loop ts
  where loop []       = return ()
        loop (t : ts) = enqueueEvent t $ e >> loop ts
       
-- | Actuate the event handler in the specified time points.
enqueueEventWithPoints :: Comp m => [Point m] -> Event m () -> Event m ()
{-# INLINABLE enqueueEventWithPoints #-}
{-# SPECIALISE enqueueEventWithPoints :: [Point IO] -> Event IO () -> Event IO () #-}
enqueueEventWithPoints xs (Event e) = loop xs
  where loop []       = return ()
        loop (x : xs) = enqueueEvent (pointTime x) $ 
                        Event $ \p ->
                        do e x    -- N.B. we substitute the time point!
                           invokeEvent p $ loop xs
                           
-- | Actuate the event handler in the integration time points.
enqueueEventWithIntegTimes :: Comp m => Event m () -> Event m ()
{-# INLINABLE enqueueEventWithIntegTimes #-}
{-# SPECIALISE enqueueEventWithIntegTimes :: Event IO () -> Event IO () #-}
enqueueEventWithIntegTimes e =
  Event $ \p ->
  let points = integPoints $ pointRun p
  in invokeEvent p $ enqueueEventWithPoints points e

-- | It allows cancelling the event.
data EventCancellation m =
  EventCancellation { cancelEvent :: Event m (),
                      -- ^ Cancel the event.
                      eventCancelled :: Event m Bool,
                      -- ^ Test whether the event was cancelled.
                      eventFinished :: Event m Bool
                      -- ^ Test whether the event was processed and finished.
                    }

-- | Enqueue the event with an ability to cancel it.
enqueueEventWithCancellation :: Comp m => Double -> Event m () -> Event m (EventCancellation m)
{-# INLINABLE enqueueEventWithCancellation #-}
{-# SPECIALISE enqueueEventWithCancellation :: Double -> Event IO () -> Event IO (EventCancellation IO) #-}
enqueueEventWithCancellation t e =
  Event $ \p ->
  do let s = runSession $ pointRun p
     cancelledRef <- newProtoRef s False
     cancellableRef <- newProtoRef s True
     finishedRef <- newProtoRef s False
     let cancel =
           Event $ \p ->
           do x <- readProtoRef cancellableRef
              when x $
                writeProtoRef cancelledRef True
         cancelled =
           Event $ \p -> readProtoRef cancelledRef
         finished =
           Event $ \p -> readProtoRef finishedRef
     invokeEvent p $
       enqueueEvent t $
       Event $ \p ->
       do writeProtoRef cancellableRef False
          x <- readProtoRef cancelledRef
          unless x $
            do invokeEvent p e
               writeProtoRef finishedRef True
     return EventCancellation { cancelEvent   = cancel,
                                eventCancelled = cancelled,
                                eventFinished = finished }

-- | Memoize the 'Event' computation, always returning the same value
-- within a simulation run.
memoEvent :: Comp m => Event m a -> Simulation m (Event m a)
{-# INLINABLE memoEvent #-}
{-# SPECIALISE memoEvent :: Event IO a -> Simulation IO (Event IO a) #-}
memoEvent m =
  Simulation $ \r ->
  do let s = runSession r
     ref <- newProtoRef s Nothing
     return $ Event $ \p ->
       do x <- readProtoRef ref
          case x of
            Just v -> return v
            Nothing ->
              do v <- invokeEvent p m
                 writeProtoRef ref (Just v)
                 return v

-- | Memoize the 'Event' computation, always returning the same value
-- in the same modeling time. After the time changes, the value is
-- recalculated by demand.
--
-- It is possible to implement this function efficiently, for the 'Event'
-- computation is always synchronized with the event queue which time
-- flows in one direction only. This synchronization is a key difference
-- between the 'Event' and 'Dynamics' computations.
memoEventInTime :: Comp m => Event m a -> Simulation m (Event m a)
{-# INLINABLE memoEventInTime #-}
{-# SPECIALISE memoEventInTime :: Event IO a -> Simulation IO (Event IO a) #-}
memoEventInTime m =
  Simulation $ \r ->
  do let s = runSession r
     ref <- newProtoRef s Nothing
     return $ Event $ \p ->
       do x <- readProtoRef ref
          case x of
            Just (t, v) | t == pointTime p ->
              return v
            _ ->
              do v <- invokeEvent p m
                 writeProtoRef ref (Just (pointTime p, v))
                 return v

-- | Enqueue the event which must be actuated with the current modeling time but later.
yieldEvent :: Comp m => Event m () -> Event m ()
{-# INLINABLE yieldEvent #-}
{-# SPECIALISE yieldEvent :: Event IO () -> Event IO () #-}
yieldEvent m =
  Event $ \p ->
  invokeEvent p $
  enqueueEvent (pointTime p) m

-- | Defines a computation disposing some entity.
newtype DisposableEvent m =
  DisposableEvent { disposeEvent :: Event m ()
                    -- ^ Dispose something within the 'Event' computation.
                  }

instance Monad m => Monoid (DisposableEvent m) where

  {-# SPECIALISE INLINE mempty :: DisposableEvent IO #-}
  mempty = DisposableEvent $ return ()

  {-# SPECIALISE INLINE mappend :: DisposableEvent IO -> DisposableEvent IO -> DisposableEvent IO #-}
  mappend (DisposableEvent x) (DisposableEvent y) = DisposableEvent $ x >> y
