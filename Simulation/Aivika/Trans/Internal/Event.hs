
{-# LANGUAGE RecursiveDo, TypeFamilies #-}

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
        Event(..),
        EventLift(..),
        EventProcessing(..),
        invokeEvent,
        runEventInStartTime,
        runEventInStopTime,
        -- * Event Queue
        EventQueueable(..),
        EventQueueing(..),
        MonadEnq,
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

import qualified Simulation.Aivika.PriorityQueue as PQ

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.MonadSim
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics

-- | A value in the 'Event' monad transformer represents a polymorphic time varying
-- function which is strongly synchronized with the event queue.
newtype Event m a = Event (Point m -> m a)

instance Monad m => Monad (Event m) where

  {-# INLINE return #-}
  return a = Event $ \p -> return a

  {-# INLINE (>>=) #-}
  (Event m) >>= k =
    Event $ \p -> 
    do a <- m p
       let Event m' = k a
       m' p

instance Functor m => Functor (Event m) where
  
  {-# INLINE fmap #-}
  fmap f (Event x) = Event $ \p -> fmap f $ x p

instance Applicative m => Applicative (Event m) where
  
  {-# INLINE pure #-}
  pure = Event . const . pure
  
  {-# INLINE (<*>) #-}
  (Event x) <*> (Event y) = Event $ \p -> x p <*> y p

instance MonadTrans Event where

  {-# INLINE lift #-}
  lift = Event . const

instance MonadIO m => MonadIO (Event m) where
  
  {-# INLINE liftIO #-}
  liftIO = Event . const . liftIO

-- | A type class to lift the 'Event' computations into other computations.
class EventLift t where
  
  -- | Lift the specified 'Event' computation into another computation.
  liftEvent :: MonadSim m => Event m a -> t m a

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
catchEvent :: MonadSim m => Event m a -> (IOException -> Event m a) -> Event m a
{-# INLINABLE catchEvent #-}
catchEvent (Event m) h =
  Event $ \p -> 
  catchComp (m p) $ \e ->
  let Event m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyEvent :: MonadSim m => Event m a -> Event m b -> Event m a
{-# INLINABLE finallyEvent #-}
finallyEvent (Event m) (Event m') =
  Event $ \p ->
  finallyComp (m p) (m' p)

-- | Like the standard 'throw' function.
throwEvent :: MonadSim m => IOException -> Event m a
{-# INLINABLE throwEvent #-}
throwEvent = throw

-- | Invoke the 'Event' computation.
invokeEvent :: Point m -> Event m a -> m a
{-# INLINE invokeEvent #-}
invokeEvent p (Event m) = m p

instance MonadFix m => MonadFix (Event m) where

  {-# INLINE mfix #-}
  mfix f = 
    Event $ \p ->
    do { rec { a <- invokeEvent p (f a) }; return a }

-- | Defines how the events are processed.
data EventProcessing = CurrentEvents
                       -- ^ either process all earlier and then current events,
                       -- or raise an error if the current simulation time is less
                       -- than the actual time of the event queue (safe within
                       -- the 'Event' computation as this is protected by the type system)
                     | EarlierEvents
                       -- ^ either process all earlier events not affecting
                       -- the events at the current simulation time,
                       -- or raise an error if the current simulation time is less
                       -- than the actual time of the event queue (safe within
                       -- the 'Event' computation as this is protected by the type system)
                     | CurrentEventsOrFromPast
                       -- ^ either process all earlier and then current events,
                       -- or do nothing if the current simulation time is less
                       -- than the actual time of the event queue
                       -- (do not use unless the documentation states the opposite)
                     | EarlierEventsOrFromPast
                       -- ^ either process all earlier events,
                       -- or do nothing if the current simulation time is less
                       -- than the actual time of the event queue
                       -- (do not use unless the documentation states the opposite)
                     deriving (Eq, Ord, Show)

-- | A type class of monads that allow enqueueing the events.
class EventQueueing m where

  -- | Enqueue the event which must be actuated at the specified time.
  --
  -- The events are processed when calling the 'runEvent' function. So,
  -- if you want to insist on their immediate execution then you can apply
  -- something like
  --
  -- @
  --   liftDynamics $ runEvent IncludingCurrentEvents $ return ()
  -- @
  --
  -- although this is generally not good idea.  
  enqueueEvent :: Double -> Event m () -> Event m ()

  -- | Run the 'EventT' computation in the current simulation time
  -- within the 'DynamicsT' computation involving all pending
  -- 'CurrentEvents' in the processing too.
  runEvent :: Event m a -> Dynamics m a
  {-# INLINE runEvent #-}
  runEvent = runEventWith CurrentEvents

  -- | Run the 'EventT' computation in the current simulation time
  -- within the 'DynamicsT' computation specifying what pending events 
  -- should be involved in the processing.
  runEventWith :: EventProcessing -> Event m a -> Dynamics m a

  -- | Return the number of pending events that should
  -- be yet actuated.
  eventQueueCount :: Event m Int

-- | Such a simulation monad that allows enqueueing events.
class (MonadSim m, EventQueueing m) => MonadEnq m

-- | Run the 'Event' computation in the start time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStartTime :: MonadEnq m => Event m a -> Simulation m a
{-# INLINE runEventInStartTime #-}
runEventInStartTime = runDynamicsInStartTime . runEvent

-- | Run the 'Event' computation in the stop time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStopTime :: MonadEnq m => Event m a -> Simulation m a
{-# INLINE runEventInStopTime #-}
runEventInStopTime = runDynamicsInStopTime . runEvent

-- | Actuate the event handler in the specified time points.
enqueueEventWithTimes :: MonadEnq m => [Double] -> Event m () -> Event m ()
{-# INLINE enqueueEventWithTimes #-}
enqueueEventWithTimes ts e = loop ts
  where loop []       = return ()
        loop (t : ts) = enqueueEvent t $ e >> loop ts
       
-- | Actuate the event handler in the specified time points.
enqueueEventWithPoints :: MonadEnq m => [Point m] -> Event m () -> Event m ()
{-# INLINE enqueueEventWithPoints #-}
enqueueEventWithPoints xs (Event e) = loop xs
  where loop []       = return ()
        loop (x : xs) = enqueueEvent (pointTime x) $ 
                        Event $ \p ->
                        do e x    -- N.B. we substitute the time point!
                           invokeEvent p $ loop xs
                           
-- | Actuate the event handler in the integration time points.
enqueueEventWithIntegTimes :: MonadEnq m => Event m () -> Event m ()
{-# INLINE enqueueEventWithIntegTimes #-}
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
enqueueEventWithCancellation :: MonadEnq m => Double -> Event m () -> Event m (EventCancellation m)
{-# INLINE enqueueEventWithCancellation #-}
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
memoEvent :: MonadSim m => Event m a -> Simulation m (Event m a)
{-# INLINE memoEvent #-}
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
memoEventInTime :: MonadSim m => Event m a -> Simulation m (Event m a)
{-# INLINE memoEventInTime #-}
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
yieldEvent :: MonadEnq m => Event m () -> Event m ()
{-# INLINE yieldEvent #-}
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

  {-# INLINE mempty #-}
  mempty = DisposableEvent $ return ()

  {-# INLINE mappend #-}
  mappend (DisposableEvent x) (DisposableEvent y) = DisposableEvent $ x >> y
