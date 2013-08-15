
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Internal.Event
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : OtherLicense
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines the 'Event' monad which is very similar to the 'Dynamics'
-- monad but only now the computation is strongly synchronized with the event queue.
--
module Simulation.Aivika.Internal.Event
       (-- * Event Monad
        Event(..),
        EventLift(..),
        EventProcessing(..),
        EventCancellation(..),
        invokeEvent,
        runEvent,
        runEventInStartTime,
        runEventInStopTime,
        -- * Event Queue
        enqueueEvent,
        enqueueEventWithCancellation,
        enqueueEventWithTimes,
        enqueueEventWithPoints,
        enqueueEventWithIntegTimes,
        enqueueEventWithStartTime,
        enqueueEventWithStopTime,
        enqueueEventWithCurrentTime,
        eventQueueCount,
        -- * Error Handling
        catchEvent,
        finallyEvent,
        throwEvent) where

import Data.IORef

import qualified Control.Exception as C
import Control.Exception (IOException, throw, finally)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix

import qualified Simulation.Aivika.PriorityQueue as PQ

import Simulation.Aivika.Internal.Specs
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

liftME :: (a -> b) -> Event a -> Event b
{-# INLINE liftME #-}
liftME f (Event x) =
  Event $ \p -> do { a <- x p; return $ f a }

instance MonadIO Event where
  liftIO m = Event $ const m

instance SimulationLift Event where
  liftSimulation = liftES

instance DynamicsLift Event where
  liftDynamics = liftDS
    
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
catchEvent :: Event a -> (IOException -> Event a) -> Event a
catchEvent (Event m) h =
  Event $ \p -> 
  C.catch (m p) $ \e ->
  let Event m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyEvent :: Event a -> Event b -> Event a
finallyEvent (Event m) (Event m') =
  Event $ \p ->
  C.finally (m p) (m' p)

-- | Like the standard 'throw' function.
throwEvent :: IOException -> Event a
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
data EventProcessing = IncludingCurrentEvents
                       -- ^ either process all earlier and then current events,
                       -- or raise an error if the current simulation time is less
                       -- than the actual time of the event queue
                     | IncludingEarlierEvents
                       -- ^ either process all earlier events not affecting
                       -- the events at the current simulation time,
                       -- or raise an error if the current simulation time is less
                       -- than the actual time of the event queue
                     | IncludingCurrentEventsOrFromPast
                       -- ^ either process all earlier and then current events,
                       -- or do nothing if the current simulation time is less
                       -- than the actual time of the event queue
                       -- (do not use unless the documentation states the opposite)
                     | IncludingEarlierEventsOrFromPast
                       -- ^ either process all earlier events,
                       -- or do nothing if the current simulation time is less
                       -- than the actual time of the event queue
                       -- (do not use unless the documentation states the opposite)
                     deriving (Eq, Ord, Show)

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
processEvents IncludingCurrentEvents = processEventsIncludingCurrent
processEvents IncludingEarlierEvents = processEventsIncludingEarlier
processEvents IncludingCurrentEventsOrFromPast = processEventsIncludingCurrentCore
processEvents IncludingEarlierEventsOrFromPast = processEventsIncludingEarlierCore

-- | Run the 'Event' computation in the current simulation time
-- within the 'Dynamics' computation.
runEvent :: EventProcessing -> Event a -> Dynamics a
runEvent processing (Event e) =
  Dynamics $ \p ->
  do invokeDynamics p $ processEvents processing
     e p

-- | Run the 'Event' computation in the start time.
runEventInStartTime :: EventProcessing -> Event a -> Simulation a
runEventInStartTime processing e =
  runDynamicsInStartTime $ runEvent processing e

-- | Run the 'Event' computation in the stop time.
runEventInStopTime :: EventProcessing -> Event a -> Simulation a
runEventInStopTime processing e =
  runDynamicsInStopTime $ runEvent processing e

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
  let points = integPoints $ pointRun p
  in invokeEvent p $ enqueueEventWithPoints points e

-- | Actuate the event handler in the start time.
enqueueEventWithStartTime :: Event () -> Event ()
enqueueEventWithStartTime e =
  Event $ \p ->
  let point = integStartPoint $ pointRun p
  in invokeEvent p $ enqueueEventWithPoints [point] e

-- | Actuate the event handler in the stop time.
enqueueEventWithStopTime :: Event () -> Event ()
enqueueEventWithStopTime e =
  Event $ \p ->
  let point = integStopPoint $ pointRun p
  in invokeEvent p $ enqueueEventWithPoints [point] e

-- | Actuate the event handler in the current time but 
-- through the event queue, which allows continuing the 
-- current tasks and then calling the handler after the 
-- tasks are finished. The simulation time will be the same.
enqueueEventWithCurrentTime :: Event () -> Event ()
enqueueEventWithCurrentTime e =
  Event $ \p ->
  invokeEvent p $ enqueueEventWithPoints [p] e

-- | It allows cancelling the event.
data EventCancellation =
  EventCancellation { cancelEvent   :: Event (),
                      -- ^ Cancel the event.
                      eventCanceled :: Event Bool,
                      -- ^ Test whether the event was canceled.
                      eventFinished :: Event Bool
                      -- ^ Test whether the event was processed and finished.
                    }

-- | Enqueue the event with an ability to cancel it.
enqueueEventWithCancellation :: Double -> Event () -> Event EventCancellation
enqueueEventWithCancellation t e =
  Event $ \p ->
  do canceledRef <- newIORef False
     cancellableRef <- newIORef True
     finishedRef <- newIORef False
     let cancel =
           Event $ \p ->
           do x <- readIORef cancellableRef
              when x $
                writeIORef canceledRef True
         canceled =
           Event $ \p -> readIORef canceledRef
         finished =
           Event $ \p -> readIORef finishedRef
     invokeEvent p $
       enqueueEvent t $
       Event $ \p ->
       do writeIORef cancellableRef False
          x <- readIORef canceledRef
          unless x $
            do invokeEvent p e
               writeIORef finishedRef True
     return EventCancellation { cancelEvent   = cancel,
                                eventCanceled = canceled,
                                eventFinished = finished }
