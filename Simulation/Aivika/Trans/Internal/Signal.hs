
-- |
-- Module     : Simulation.Aivika.Trans.Internal.Signal
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the signal which we can subscribe handlers to. 
-- These handlers can be disposed. The signal is triggered in the 
-- current time point actuating the corresponded computations from 
-- the handlers. 
--

module Simulation.Aivika.Trans.Internal.Signal
       (-- * Handling and Triggering Signal
        SignalT(..),
        Signal(..),
        handleSignal_,
        SignalTSource,
        SignalSource,
        newSignalSource,
        publishSignal,
        triggerSignal,
        -- * Useful Combinators
        mapSignal,
        mapSignalM,
        apSignal,
        filterSignal,
        filterSignalM,
        emptySignal,
        merge2Signals,
        merge3Signals,
        merge4Signals,
        merge5Signals,
        -- * Signal Arriving
        arrivalSignal,
        -- * Creating Signal in Time Points
        newSignalInTimes,
        newSignalInIntegTimes,
        newSignalInStartTime,
        newSignalInStopTime,
        -- * Signal History
        SignalTHistory,
        SignalHistory,
        signalHistorySignal,
        newSignalHistory,
        newSignalHistoryStartingWith,
        readSignalHistory,
        -- * Signalable Computations
        SignalableT(..),
        Signalable(..),
        signalableChanged,
        emptySignalable,
        appendSignalable) where

import Data.Monoid
import Data.List
import Data.Array

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import qualified Simulation.Aivika.Trans.ProtoVector as V
import qualified Simulation.Aivika.Trans.ProtoVector.Unboxed as UV
import Simulation.Aivika.Trans.MonadSim
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Arrival

-- | The signal source that can publish its signal.
data SignalTSource m a =
  SignalSource { publishSignal :: SignalT m a,
                                  -- ^ Publish the signal.
                 triggerSignal :: a -> EventT m ()
                                  -- ^ Trigger the signal actuating 
                                  -- all its handlers at the current 
                                  -- simulation time point.
               }
  
-- | The signal that can have disposable handlers.  
data SignalT m a =
  Signal { handleSignal :: (a -> EventT m ()) -> EventT m (DisposableEventT m)
           -- ^ Subscribe the handler to the specified 
           -- signal and return a nested computation
           -- within a disposable object that, being applied,
           -- unsubscribes the handler from this signal.
         }

-- | The queue of signal handlers.
data SignalTHandlerQueue m a =
  SignalHandlerQueue { queueList :: ProtoRefT m [SignalTHandler m a] }
  
-- | It contains the information about the disposable queue handler.
data SignalTHandler m a =
  SignalHandler { handlerComp   :: a -> EventT m (),
                  handlerMarker :: SessionTMarker m }

-- | A convenient type synonym.
type SignalSource a = SignalTSource IO a

-- | A convenient type synonym.
type Signal a = SignalT IO a

instance Sessionning m => Eq (SignalTHandler m a) where

  {-# INLINE (==) #-}
  x == y = (handlerMarker x) == (handlerMarker y)

-- | Subscribe the handler to the specified signal forever.
-- To subscribe the disposable handlers, use function 'handleSignal'.
handleSignal_ :: Monad m => SignalT m a -> (a -> EventT m ()) -> EventT m ()
{-# INLINE handleSignal_ #-}
handleSignal_ signal h = 
  do x <- handleSignal signal h
     return ()
     
-- | Create a new signal source.
newSignalSource :: MonadSim m => SimulationT m (SignalTSource m a)
{-# INLINE newSignalSource #-}
newSignalSource =
  Simulation $ \r ->
  do let s = runSession r
     list <- newProtoRef s []
     let queue  = SignalHandlerQueue { queueList = list }
         signal = Signal { handleSignal = handle }
         source = SignalSource { publishSignal = signal, 
                                 triggerSignal = trigger }
         handle h =
           Event $ \p ->
           do m <- newSessionMarker s
              x <- enqueueSignalHandler queue h m
              return $
                DisposableEvent $
                Event $ \p -> dequeueSignalHandler queue x
         trigger a =
           Event $ \p -> triggerSignalHandlers queue a p
     return source

-- | Trigger all next signal handlers.
triggerSignalHandlers :: MonadSim m => SignalTHandlerQueue m a -> a -> PointT m -> m ()
{-# INLINE triggerSignalHandlers #-}
triggerSignalHandlers q a p =
  do hs <- readProtoRef (queueList q)
     forM_ hs $ \h ->
       invokeEvent p $ handlerComp h a
            
-- | Enqueue the handler and return its representative in the queue.            
enqueueSignalHandler :: MonadSim m => SignalTHandlerQueue m a -> (a -> EventT m ()) -> SessionTMarker m -> m (SignalTHandler m a)
{-# INLINE enqueueSignalHandler #-}
enqueueSignalHandler q h m = 
  do let handler = SignalHandler { handlerComp   = h,
                                   handlerMarker = m }
     modifyProtoRef (queueList q) (handler :)
     return handler

-- | Dequeue the handler representative.
dequeueSignalHandler :: MonadSim m => SignalTHandlerQueue m a -> SignalTHandler m a -> m ()
{-# INLINE dequeueSignalHandler #-}
dequeueSignalHandler q h = 
  modifyProtoRef (queueList q) (delete h)

instance Monad m => Functor (SignalT m) where

  {-# INLINE fmap #-}
  fmap = mapSignal
  
instance Monad m => Monoid (SignalT m a) where 

  {-# INLINE mempty #-}
  mempty = emptySignal

  {-# INLINE mappend #-}
  mappend = merge2Signals

  {-# INLINABLE mconcat #-}
  mconcat [] = emptySignal
  mconcat [x1] = x1
  mconcat [x1, x2] = merge2Signals x1 x2
  mconcat [x1, x2, x3] = merge3Signals x1 x2 x3
  mconcat [x1, x2, x3, x4] = merge4Signals x1 x2 x3 x4
  mconcat [x1, x2, x3, x4, x5] = merge5Signals x1 x2 x3 x4 x5
  mconcat (x1 : x2 : x3 : x4 : x5 : xs) = 
    mconcat $ merge5Signals x1 x2 x3 x4 x5 : xs
  
-- | Map the signal according the specified function.
mapSignal :: Monad m => (a -> b) -> SignalT m a -> SignalT m b
{-# INLINE mapSignal #-}
mapSignal f m =
  Signal { handleSignal = \h -> 
            handleSignal m $ h . f }

-- | Filter only those signal values that satisfy to 
-- the specified predicate.
filterSignal :: Monad m => (a -> Bool) -> SignalT m a -> SignalT m a
{-# INLINE filterSignal #-}
filterSignal p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            when (p a) $ h a }
  
-- | Filter only those signal values that satisfy to 
-- the specified predicate.
filterSignalM :: Monad m => (a -> EventT m Bool) -> SignalT m a -> SignalT m a
{-# INLINE filterSignalM #-}
filterSignalM p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            do x <- p a
               when x $ h a }
  
-- | Merge two signals.
merge2Signals :: Monad m => SignalT m a -> SignalT m a -> SignalT m a
{-# INLINE merge2Signals #-}
merge2Signals m1 m2 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               return $ x1 <> x2 }

-- | Merge three signals.
merge3Signals :: Monad m => SignalT m a -> SignalT m a -> SignalT m a -> SignalT m a
{-# INLINE merge3Signals #-}
merge3Signals m1 m2 m3 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               return $ x1 <> x2 <> x3 }

-- | Merge four signals.
merge4Signals :: Monad m
                 => SignalT m a -> SignalT m a -> SignalT m a
                 -> SignalT m a -> SignalT m a
{-# INLINE merge4Signals #-}
merge4Signals m1 m2 m3 m4 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               x4 <- handleSignal m4 h
               return $ x1 <> x2 <> x3 <> x4 }
           
-- | Merge five signals.
merge5Signals :: Monad m
                 => SignalT m a -> SignalT m a -> SignalT m a
                 -> SignalT m a -> SignalT m a -> SignalT m a
{-# INLINE merge5Signals #-}
merge5Signals m1 m2 m3 m4 m5 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               x4 <- handleSignal m4 h
               x5 <- handleSignal m5 h
               return $ x1 <> x2 <> x3 <> x4 <> x5 }

-- | Compose the signal.
mapSignalM :: Monad m => (a -> EventT m b) -> SignalT m a -> SignalT m b
{-# INLINE mapSignalM #-}
mapSignalM f m =
  Signal { handleSignal = \h ->
            handleSignal m (f >=> h) }
  
-- | Transform the signal.
apSignal :: Monad m => EventT m (a -> b) -> SignalT m a -> SignalT m b
{-# INLINE apSignal #-}
apSignal f m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a -> do { x <- f; h (x a) } }

-- | An empty signal which is never triggered.
emptySignal :: Monad m => SignalT m a
{-# INLINE emptySignal #-}
emptySignal =
  Signal { handleSignal = \h -> return mempty }
                                    
-- | Represents the history of the signal values.
data SignalTHistory m a =
  SignalHistory { signalHistorySignal :: SignalT m a,  
                  -- ^ The signal for which the history is created.
                  signalHistoryTimes  :: UV.ProtoVectorT m Double,
                  signalHistoryValues :: V.ProtoVectorT m a }

-- | A convenient type synonym.
type SignalHistory a = SignalTHistory IO a

-- | Create a history of the signal values.
newSignalHistory :: MonadSim m => SignalT m a -> EventT m (SignalTHistory m a)
{-# INLINE newSignalHistory #-}
newSignalHistory =
  newSignalHistoryStartingWith Nothing

-- | Create a history of the signal values starting with
-- the optional initial value.
newSignalHistoryStartingWith :: MonadSim m => Maybe a -> SignalT m a -> EventT m (SignalTHistory m a)
{-# INLINE newSignalHistoryStartingWith #-}
newSignalHistoryStartingWith init signal =
  Event $ \p ->
  do let s = runSession $ pointRun p
     ts <- UV.newProtoVector s
     xs <- V.newProtoVector s
     case init of
       Nothing -> return ()
       Just a ->
         do UV.appendProtoVector ts (pointTime p)
            V.appendProtoVector xs a
     invokeEvent p $
       handleSignal_ signal $ \a ->
       Event $ \p ->
       do UV.appendProtoVector ts (pointTime p)
          V.appendProtoVector xs a
     return SignalHistory { signalHistorySignal = signal,
                            signalHistoryTimes  = ts,
                            signalHistoryValues = xs }
       
-- | Read the history of signal values.
readSignalHistory :: MonadSim m => SignalTHistory m a -> EventT m (Array Int Double, Array Int a)
{-# INLINE readSignalHistory #-}
readSignalHistory history =
  Event $ \p ->
  do xs <- UV.freezeProtoVector (signalHistoryTimes history)
     ys <- V.freezeProtoVector (signalHistoryValues history)
     return (xs, ys)     
     
-- | Trigger the signal with the current time.
triggerSignalWithCurrentTime :: MonadSim m => SignalTSource m Double -> EventT m ()
{-# INLINE triggerSignalWithCurrentTime #-}
triggerSignalWithCurrentTime s =
  Event $ \p -> invokeEvent p $ triggerSignal s (pointTime p)

-- | Return a signal that is triggered in the specified time points.
newSignalInTimes :: MonadEnq m => [Double] -> EventT m (SignalT m Double)
{-# INLINE newSignalInTimes #-}
newSignalInTimes xs =
  do s <- liftSimulation newSignalSource
     enqueueEventWithTimes xs $ triggerSignalWithCurrentTime s
     return $ publishSignal s
       
-- | Return a signal that is triggered in the integration time points.
-- It should be called with help of 'runEventInStartTime'.
newSignalInIntegTimes :: MonadEnq m => EventT m (SignalT m Double)
{-# INLINE newSignalInIntegTimes #-}
newSignalInIntegTimes =
  do s <- liftSimulation newSignalSource
     enqueueEventWithIntegTimes $ triggerSignalWithCurrentTime s
     return $ publishSignal s
     
-- | Return a signal that is triggered in the start time.
-- It should be called with help of 'runEventInStartTime'.
newSignalInStartTime :: MonadEnq m => EventT m (SignalT m Double)
{-# INLINE newSignalInStartTime #-}
newSignalInStartTime =
  do s <- liftSimulation newSignalSource
     t <- liftParameter starttime
     enqueueEvent t $ triggerSignalWithCurrentTime s
     return $ publishSignal s

-- | Return a signal that is triggered in the final time.
newSignalInStopTime :: MonadEnq m => EventT m (SignalT m Double)
{-# INLINE newSignalInStopTime #-}
newSignalInStopTime =
  do s <- liftSimulation newSignalSource
     t <- liftParameter stoptime
     enqueueEvent t $ triggerSignalWithCurrentTime s
     return $ publishSignal s

-- | Describes a computation that also signals when changing its value.
data SignalableT m a =
  Signalable { readSignalable :: EventT m a,
               -- ^ Return a computation of the value.
               signalableChanged_ :: SignalT m ()
               -- ^ Return a signal notifying that the value has changed
               -- but without providing the information about the changed value.
             }

-- | A convenient type synonym.
type Signalable a = SignalableT IO a

-- | Return a signal notifying that the value has changed.
signalableChanged :: Monad m => SignalableT m a -> SignalT m a
{-# INLINE signalableChanged #-}
signalableChanged x = mapSignalM (const $ readSignalable x) $ signalableChanged_ x

instance Functor m => Functor (SignalableT m) where

  {-# INLINE fmap #-}
  fmap f x = x { readSignalable = fmap f (readSignalable x) }

instance (Monad m, Monoid a) => Monoid (SignalableT m a) where

  {-# INLINE mempty #-}
  mempty = emptySignalable

  {-# INLINE mappend #-}
  mappend = appendSignalable

-- | Return an identity.
emptySignalable :: (Monad m, Monoid a) => SignalableT m a
{-# INLINE emptySignalable #-}
emptySignalable =
  Signalable { readSignalable = return mempty,
               signalableChanged_ = mempty }

-- | An associative operation.
appendSignalable :: (Monad m, Monoid a) => SignalableT m a -> SignalableT m a -> SignalableT m a
{-# INLINE appendSignalable #-}
appendSignalable m1 m2 =
  Signalable { readSignalable = liftM2 (<>) (readSignalable m1) (readSignalable m2),
               signalableChanged_ = (signalableChanged_ m1) <> (signalableChanged_ m2) }

-- | Transform a signal so that the resulting signal returns a sequence of arrivals
-- saving the information about the time points at which the original signal was received.
arrivalSignal :: MonadSim m => SignalT m a -> SignalT m (Arrival a)
{-# INLINE arrivalSignal #-}
arrivalSignal m = 
  Signal { handleSignal = \h ->
             Event $ \p ->
             do let s = runSession $ pointRun p
                r <- newProtoRef s Nothing
                invokeEvent p $
                  handleSignal m $ \a ->
                  Event $ \p ->
                  do t0 <- readProtoRef r
                     let t = pointTime p
                     writeProtoRef r (Just t)
                     invokeEvent p $
                       h Arrival { arrivalValue = a,
                                   arrivalTime  = t,
                                   arrivalDelay =
                                     case t0 of
                                       Nothing -> Nothing
                                       Just t0 -> Just (t - t0) }
         }
