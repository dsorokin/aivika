
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Internal.Event
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This is an internal implementation module that should never be used directly.
--
-- The module defines the 'Event' monad which is very similar to the 'Dynamics'
-- monad but only now the computation is strongly synchronized with the event queue.
--
-- The @Dynamics@ computation is defined in all time points simultaneously, while
-- the @Event@ computation can be described in every time point differently and can change
-- in discrete steps. Therefore, the former is destined for differential and difference
-- equations of System Dynamics, while the latter is destined for discrete event simulation,
-- being its core actually.
--
module Simulation.Aivika.Internal.Event
       (-- * Event Monad
        Event(..),
        EventLift(..),
        EventProcessing(..),
        invokeEvent,
        runEvent,
        runEventWith,
        runEventInStartTime,
        runEventInStopTime,
        -- * Event Queue
        enqueueEvent,
        enqueueEventWithCancellation,
        enqueueEventWithTimes,
        enqueueEventWithPoints,
        enqueueEventWithIntegTimes,
        enqueueEventWithStopTime,
        yieldEvent,
        eventQueueCount,
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
        DisposableEvent(..),
        -- * Retrying Computation
        retryEvent,
        -- * Debugging
        traceEvent) where

import Data.IORef
import Data.Monoid

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Debug.Trace (trace)

import qualified Simulation.Aivika.PriorityQueue as PQ

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics

-- | A value in the 'Event' monad represents a polymorphic time varying function
-- which is strongly synchronized with the event queue.
newtype Event a = Event (Point -> IO a)

instance Monad Event where
  return  = returnE
  m >>= k = bindE m k

returnE :: a -> Event a
{-# INLINE returnE #-}
returnE a = Event (\p -> return a)

bindE :: Event a -> (a -> Event b) -> Event b
{-# INLINE bindE #-}
bindE (Event m) k = 
  Event $ \p -> 
  do a <- m p
     let Event m' = k a
     m' p

instance Functor Event where
  fmap = liftME

instance Applicative Event where
  pure = return
  (<*>) = ap

liftME :: (a -> b) -> Event a -> Event b
{-# INLINE liftME #-}
liftME f (Event x) =
  Event $ \p -> do { a <- x p; return $ f a }

instance MonadIO Event where
  liftIO m = Event $ const m

instance ParameterLift Event where
  liftParameter = liftPS

instance SimulationLift Event where
  liftSimulation = liftES

instance DynamicsLift Event where
  liftDynamics = liftDS
    
liftPS :: Parameter a -> Event a
{-# INLINE liftPS #-}
liftPS (Parameter m) =
  Event $ \p -> m $ pointRun p
    
liftES :: Simulation a -> Event a
{-# INLINE liftES #-}
liftES (Simulation m) =
  Event $ \p -> m $ pointRun p

liftDS :: Dynamics a -> Event a
{-# INLINE liftDS #-}
liftDS (Dynamics m) =
  Event m

-- | A type class to lift the 'Event' computation to other computations.
class EventLift m where
  
  -- | Lift the specified 'Event' computation to another computation.
  liftEvent :: Event a -> m a

instance EventLift Event where
  liftEvent = id
  
-- | Exception handling within 'Event' computations.
catchEvent :: Exception e => Event a -> (e -> Event a) -> Event a
catchEvent (Event m) h =
  Event $ \p -> 
  catch (m p) $ \e ->
  let Event m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyEvent :: Event a -> Event b -> Event a
finallyEvent (Event m) (Event m') =
  Event $ \p ->
  finally (m p) (m' p)

-- | Like the standard 'throw' function.
throwEvent :: Exception e => e -> Event a
throwEvent = throw

-- | Invoke the 'Event' computation.
invokeEvent :: Point -> Event a -> IO a
{-# INLINE invokeEvent #-}
invokeEvent p (Event m) = m p

instance MonadFix Event where
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

-- | Enqueue the event which must be actuated at the specified time.
enqueueEvent :: Double -> Event () -> Event ()
enqueueEvent t (Event m) =
  Event $ \p ->
  let pq = queuePQ $ runEventQueue $ pointRun p
  in PQ.enqueue pq t m

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

-- | Run the 'Event' computation in the current simulation time
-- within the 'Dynamics' computation involving all pending
-- 'CurrentEvents' in the processing too.
runEvent :: Event a -> Dynamics a
runEvent = runEventWith CurrentEvents

-- | Run the 'Event' computation in the current simulation time
-- within the 'Dynamics' computation specifying what pending events 
-- should be involved in the processing.
runEventWith :: EventProcessing -> Event a -> Dynamics a
runEventWith processing (Event e) =
  Dynamics $ \p ->
  do invokeDynamics p $ processEvents processing
     e p

-- | Run the 'Event' computation in the start time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStartTime :: Event a -> Simulation a
runEventInStartTime = runDynamicsInStartTime . runEvent

-- | Run the 'Event' computation in the stop time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStopTime :: Event a -> Simulation a
runEventInStopTime = runDynamicsInStopTime . runEvent

-- | Return the number of pending events that should
-- be yet actuated.
eventQueueCount :: Event Int
eventQueueCount =
  Event $ PQ.queueCount . queuePQ . runEventQueue . pointRun

-- | Actuate the event handler in the specified time points.
enqueueEventWithTimes :: [Double] -> Event () -> Event ()
enqueueEventWithTimes ts e = loop ts
  where loop []       = return ()
        loop (t : ts) = enqueueEvent t $ e >> loop ts
       
-- | Actuate the event handler in the specified time points.
enqueueEventWithPoints :: [Point] -> Event () -> Event ()
enqueueEventWithPoints xs (Event e) = loop xs
  where loop []       = return ()
        loop (x : xs) = enqueueEvent (pointTime x) $ 
                        Event $ \p ->
                        do e x    -- N.B. we substitute the time point!
                           invokeEvent p $ loop xs
                           
-- | Actuate the event handler in the integration time points.
enqueueEventWithIntegTimes :: Event () -> Event ()
enqueueEventWithIntegTimes e =
  Event $ \p ->
  let points = integPointsStartingFrom p
  in invokeEvent p $ enqueueEventWithPoints points e

-- | Actuate the event handler in the final time point.
enqueueEventWithStopTime :: Event () -> Event ()
enqueueEventWithStopTime e =
  Event $ \p ->
  let p0 = integStopPoint $ pointRun p
  in invokeEvent p $ enqueueEventWithPoints [p0] e

-- | It allows cancelling the event.
data EventCancellation =
  EventCancellation { cancelEvent   :: Event (),
                      -- ^ Cancel the event.
                      eventCancelled :: Event Bool,
                      -- ^ Test whether the event was cancelled.
                      eventFinished :: Event Bool
                      -- ^ Test whether the event was processed and finished.
                    }

-- | Enqueue the event with an ability to cancel it.
enqueueEventWithCancellation :: Double -> Event () -> Event EventCancellation
enqueueEventWithCancellation t e =
  Event $ \p ->
  do cancelledRef <- newIORef False
     cancellableRef <- newIORef True
     finishedRef <- newIORef False
     let cancel =
           Event $ \p ->
           do x <- readIORef cancellableRef
              when x $
                writeIORef cancelledRef True
         cancelled =
           Event $ \p -> readIORef cancelledRef
         finished =
           Event $ \p -> readIORef finishedRef
     invokeEvent p $
       enqueueEvent t $
       Event $ \p ->
       do writeIORef cancellableRef False
          x <- readIORef cancelledRef
          unless x $
            do invokeEvent p e
               writeIORef finishedRef True
     return EventCancellation { cancelEvent   = cancel,
                                eventCancelled = cancelled,
                                eventFinished = finished }

-- | Memoize the 'Event' computation, always returning the same value
-- within a simulation run.
memoEvent :: Event a -> Simulation (Event a)
memoEvent m =
  do ref <- liftIO $ newIORef Nothing
     return $ Event $ \p ->
       do x <- readIORef ref
          case x of
            Just v -> return v
            Nothing ->
              do v <- invokeEvent p m
                 writeIORef ref (Just v)
                 return v

-- | Memoize the 'Event' computation, always returning the same value
-- in the same modeling time. After the time changes, the value is
-- recalculated by demand.
--
-- It is possible to implement this function efficiently, for the 'Event'
-- computation is always synchronized with the event queue which time
-- flows in one direction only. This synchronization is a key difference
-- between the 'Event' and 'Dynamics' computations.
memoEventInTime :: Event a -> Simulation (Event a)
memoEventInTime m =
  do ref <- liftIO $ newIORef Nothing
     return $ Event $ \p ->
       do x <- readIORef ref
          case x of
            Just (t, v) | t == pointTime p ->
              return v
            _ ->
              do v <- invokeEvent p m
                 writeIORef ref (Just (pointTime p, v))
                 return v

-- | Enqueue the event which must be actuated with the current modeling time but later.
yieldEvent :: Event () -> Event ()
yieldEvent m =
  Event $ \p ->
  invokeEvent p $
  enqueueEvent (pointTime p) m

-- | Defines a computation disposing some entity.
newtype DisposableEvent =
  DisposableEvent { disposeEvent :: Event ()
                    -- ^ Dispose something within the 'Event' computation.
                  }

instance Monoid DisposableEvent where

  mempty = DisposableEvent $ return ()
  mappend (DisposableEvent x) (DisposableEvent y) = DisposableEvent $ x >> y

-- | Retry the current computation as possible, using the specified argument
-- as a 'SimulationRetry' exception message in case of failure.
retryEvent :: String -> Event a
retryEvent message = throwEvent $ SimulationRetry message

-- | Show the debug message with the current simulation time.
traceEvent :: String -> Event a -> Event a
traceEvent message m =
  Event $ \p ->
  trace ("t = " ++ show (pointTime p) ++ ": " ++ message) $
  invokeEvent p m
