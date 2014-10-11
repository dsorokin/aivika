
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Event
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'EventT' monad transformer which is very similar to the 'DynamicsT'
-- monad transformer but only now the computation is strongly synchronized with the event queue.
--
module Simulation.Aivika.Trans.Internal.Event
       (-- * Event Monad
        EventT(..),
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
        EventTCancellation,
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
        DisposableEventT(..),
        DisposableEvent(..)) where

import Data.IORef
import Data.Monoid

import Control.Exception

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import qualified Simulation.Aivika.Trans.PriorityQueue as PQ

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.MonadSim
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics

-- | A value in the 'EventT' monad transformer represents a polymorphic time varying
-- function which is strongly synchronized with the event queue.
newtype EventT m a = Event (PointT m -> m a)

-- | A convenient type synonym.
type Event a = EventT IO a

instance Monad m => Monad (EventT m) where

  {-# INLINE return #-}
  return a = Event $ \p -> return a

  {-# INLINE (>>=) #-}
  (Event m) >>= k =
    Event $ \p -> 
    do a <- m p
       let Event m' = k a
       m' p

instance Functor m => Functor (EventT m) where
  
  {-# INLINE fmap #-}
  fmap f (Event x) = Event $ \p -> fmap f $ x p

instance Applicative m => Applicative (EventT m) where
  
  {-# INLINE pure #-}
  pure = Event . const . pure
  
  {-# INLINE (<*>) #-}
  (Event x) <*> (Event y) = Event $ \p -> x p <*> y p

instance MonadTrans EventT where

  {-# INLINE lift #-}
  lift = Event . const

instance MonadIO m => MonadIO (EventT m) where
  
  {-# INLINE liftIO #-}
  liftIO = Event . const . liftIO

-- | A type class to lift the 'EventT' computations into other computations.
class EventLift t where
  
  -- | Lift the specified 'EventT' computation into another computation.
  liftEvent :: Monad m => EventT m a -> t m a

instance EventLift EventT where
  
  {-# INLINE liftEvent #-}
  liftEvent = id

instance DynamicsLift EventT where
  
  {-# INLINE liftDynamics #-}
  liftDynamics (Dynamics x) = Event x

instance SimulationLift EventT where

  {-# INLINE liftSimulation #-}
  liftSimulation (Simulation x) = Event $ x . pointRun 

instance ParameterLift EventT where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter x) = Event $ x . pointRun

-- | Exception handling within 'EventT' computations.
catchEvent :: MonadSim m => EventT m a -> (IOException -> EventT m a) -> EventT m a
{-# INLINABLE catchEvent #-}
catchEvent (Event m) h =
  Event $ \p -> 
  catchComputation (m p) $ \e ->
  let Event m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyEvent :: MonadSim m => EventT m a -> EventT m b -> EventT m a
{-# INLINABLE finallyEvent #-}
finallyEvent (Event m) (Event m') =
  Event $ \p ->
  finallyComputation (m p) (m' p)

-- | Like the standard 'throw' function.
throwEvent :: MonadSim m => IOException -> EventT m a
{-# INLINABLE throwEvent #-}
throwEvent = throw

-- | Invoke the 'EventT' computation.
invokeEvent :: PointT m -> EventT m a -> m a
{-# INLINE invokeEvent #-}
invokeEvent p (Event m) = m p

instance MonadFix m => MonadFix (EventT m) where

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
  enqueueEvent :: Double -> EventT m () -> EventT m ()

  -- | Run the 'EventT' computation in the current simulation time
  -- within the 'DynamicsT' computation involving all pending
  -- 'CurrentEvents' in the processing too.
  runEvent :: EventT m a -> DynamicsT m a
  {-# INLINE runEvent #-}
  runEvent = runEventWith CurrentEvents

  -- | Run the 'EventT' computation in the current simulation time
  -- within the 'DynamicsT' computation specifying what pending events 
  -- should be involved in the processing.
  runEventWith :: EventProcessing -> EventT m a -> DynamicsT m a

  -- | Return the number of pending events that should
  -- be yet actuated.
  eventQueueCount :: EventT m Int

instance EventQueueing IO where
  
  {-# INLINE enqueueEvent #-}
  enqueueEvent t (Event m) =
    Event $ \p ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in PQ.enqueue pq t m

  {-# INLINE runEventWith #-}
  runEventWith processing (Event e) =
    Dynamics $ \p ->
    do invokeDynamics p $ processEvents processing
       e p

  {-# INLINE eventQueueCount #-}
  eventQueueCount =
    Event $ PQ.queueCount . queuePQ . runEventQueue . pointRun

-- | Such a simulation monad that allows enqueueing events.
class (MonadSim m, EventQueueing m) => MonadEnq m

instance MonadEnq IO

-- | Process the pending events.
processPendingEventsCore :: Bool -> Dynamics ()
processPendingEventsCore includingCurrentEvents = Dynamics r where
  r p =
    do let q = runEventQueue $ pointRun p
           f = queueBusy q
       f' <- readIORef f
       unless f' $
         do writeIORef f True
            call q p
            writeIORef f False
  call q p =
    do let pq = queuePQ q
           r  = pointRun p
       f <- PQ.queueNull pq
       unless f $
         do (t2, c2) <- PQ.queueFront pq
            let t = queueTime q
            t' <- readIORef t
            when (t2 < t') $ 
              error "The time value is too small: processPendingEventsCore"
            when ((t2 < pointTime p) ||
                  (includingCurrentEvents && (t2 == pointTime p))) $
              do writeIORef t t2
                 PQ.dequeue pq
                 let sc = pointSpecs p
                     t0 = spcStartTime sc
                     dt = spcDT sc
                     n2 = fromIntegral $ floor ((t2 - t0) / dt)
                 c2 $ p { pointTime = t2,
                          pointIteration = n2,
                          pointPhase = -1 }
                 call q p

-- | Process the pending events synchronously, i.e. without past.
processPendingEvents :: Bool -> Dynamics ()
processPendingEvents includingCurrentEvents = Dynamics r where
  r p =
    do let q = runEventQueue $ pointRun p
           t = queueTime q
       t' <- readIORef t
       if pointTime p < t'
         then error $
              "The current time is less than " ++
              "the time in the queue: processPendingEvents"
         else invokeDynamics p m
  m = processPendingEventsCore includingCurrentEvents

-- | A memoized value.
processEventsIncludingCurrent = processPendingEvents True

-- | A memoized value.
processEventsIncludingEarlier = processPendingEvents False

-- | A memoized value.
processEventsIncludingCurrentCore = processPendingEventsCore True

-- | A memoized value.
processEventsIncludingEarlierCore = processPendingEventsCore True

-- | Process the events.
processEvents :: EventProcessing -> Dynamics ()
processEvents CurrentEvents = processEventsIncludingCurrent
processEvents EarlierEvents = processEventsIncludingEarlier
processEvents CurrentEventsOrFromPast = processEventsIncludingCurrentCore
processEvents EarlierEventsOrFromPast = processEventsIncludingEarlierCore

-- | Run the 'EventT' computation in the start time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStartTime :: MonadEnq m => EventT m a -> SimulationT m a
{-# INLINE runEventInStartTime #-}
runEventInStartTime = runDynamicsInStartTime . runEvent

-- | Run the 'EventT' computation in the stop time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStopTime :: MonadEnq m => EventT m a -> SimulationT m a
{-# INLINE runEventInStopTime #-}
runEventInStopTime = runDynamicsInStopTime . runEvent

-- | Actuate the event handler in the specified time points.
enqueueEventWithTimes :: MonadEnq m => [Double] -> EventT m () -> EventT m ()
{-# INLINE enqueueEventWithTimes #-}
enqueueEventWithTimes ts e = loop ts
  where loop []       = return ()
        loop (t : ts) = enqueueEvent t $ e >> loop ts
       
-- | Actuate the event handler in the specified time points.
enqueueEventWithPoints :: MonadEnq m => [PointT m] -> EventT m () -> EventT m ()
{-# INLINE enqueueEventWithPoints #-}
enqueueEventWithPoints xs (Event e) = loop xs
  where loop []       = return ()
        loop (x : xs) = enqueueEvent (pointTime x) $ 
                        Event $ \p ->
                        do e x    -- N.B. we substitute the time point!
                           invokeEvent p $ loop xs
                           
-- | Actuate the event handler in the integration time points.
enqueueEventWithIntegTimes :: MonadEnq m => EventT m () -> EventT m ()
{-# INLINE enqueueEventWithIntegTimes #-}
enqueueEventWithIntegTimes e =
  Event $ \p ->
  let points = integPoints $ pointRun p
  in invokeEvent p $ enqueueEventWithPoints points e

-- | It allows cancelling the event.
data EventTCancellation m =
  EventCancellation { cancelEvent   :: EventT m (),
                      -- ^ Cancel the event.
                      eventCancelled :: EventT m Bool,
                      -- ^ Test whether the event was cancelled.
                      eventFinished :: EventT m Bool
                      -- ^ Test whether the event was processed and finished.
                    }

-- | A convenient type synonym.
type EventCancellation = EventTCancellation IO

-- | Enqueue the event with an ability to cancel it.
enqueueEventWithCancellation :: MonadEnq m => Double -> EventT m () -> EventT m (EventTCancellation m)
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

-- | Memoize the 'EventT' computation, always returning the same value
-- within a simulation run.
memoEvent :: MonadSim m => EventT m a -> SimulationT m (EventT m a)
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

-- | Memoize the 'EventT' computation, always returning the same value
-- in the same modeling time. After the time changes, the value is
-- recalculated by demand.
--
-- It is possible to implement this function efficiently, for the 'EventT'
-- computation is always synchronized with the event queue which time
-- flows in one direction only. This synchronization is a key difference
-- between the 'EventT' and 'DynamicsT' computations.
memoEventInTime :: MonadSim m => EventT m a -> SimulationT m (EventT m a)
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
yieldEvent :: MonadEnq m => EventT m () -> EventT m ()
{-# INLINE yieldEvent #-}
yieldEvent m =
  Event $ \p ->
  invokeEvent p $
  enqueueEvent (pointTime p) m

-- | Defines a computation disposing some entity.
newtype DisposableEventT m =
  DisposableEvent { disposeEvent :: EventT m ()
                    -- ^ Dispose something within the 'EventT' computation.
                  }

-- | A convenient type synonym.
type DisposableEvent = DisposableEventT IO

instance Monad m => Monoid (DisposableEventT m) where

  {-# INLINE mempty #-}
  mempty = DisposableEvent $ return ()

  {-# INLINE mappend #-}
  mappend (DisposableEvent x) (DisposableEvent y) = DisposableEvent $ x >> y
