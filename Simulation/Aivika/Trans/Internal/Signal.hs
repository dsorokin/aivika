
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
        Signal(..),
        handleSignal_,
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
        SignalHistory,
        signalHistorySignal,
        newSignalHistory,
        newSignalHistoryStartingWith,
        readSignalHistory,
        -- * Signalable Computations
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
import qualified Simulation.Aivika.Trans.Vector as V
import qualified Simulation.Aivika.Trans.Vector.Unboxed as UV
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Arrival

-- | The signal source that can publish its signal.
data SignalSource m a =
  SignalSource { publishSignal :: Signal m a,
                                  -- ^ Publish the signal.
                 triggerSignal :: a -> Event m ()
                                  -- ^ Trigger the signal actuating 
                                  -- all its handlers at the current 
                                  -- simulation time point.
               }
  
-- | The signal that can have disposable handlers.  
data Signal m a =
  Signal { handleSignal :: (a -> Event m ()) -> Event m (DisposableEvent m)
           -- ^ Subscribe the handler to the specified 
           -- signal and return a nested computation
           -- within a disposable object that, being applied,
           -- unsubscribes the handler from this signal.
         }

-- | The queue of signal handlers.
data SignalHandlerQueue m a =
  SignalHandlerQueue { queueList :: ProtoRef m [SignalHandler m a] }
  
-- | It contains the information about the disposable queue handler.
data SignalHandler m a =
  SignalHandler { handlerComp   :: a -> Event m (),
                  handlerMarker :: SessionMarker m }

instance Sessionning m => Eq (SignalHandler m a) where

  {-# SPECIALISE INLINE (==) :: SignalHandler IO a -> SignalHandler IO a -> Bool #-}
  x == y = (handlerMarker x) == (handlerMarker y)

-- | Subscribe the handler to the specified signal forever.
-- To subscribe the disposable handlers, use function 'handleSignal'.
handleSignal_ :: Comp m => Signal m a -> (a -> Event m ()) -> Event m ()
{-# INLINABLE handleSignal_ #-}
{-# SPECIALISE handleSignal_ :: Signal IO a -> (a -> Event IO ()) -> Event IO () #-}
handleSignal_ signal h = 
  do x <- handleSignal signal h
     return ()
     
-- | Create a new signal source.
newSignalSource :: Comp m => Simulation m (SignalSource m a)
{-# INLINABLE newSignalSource #-}
{-# SPECIALISE newSignalSource :: Simulation IO (SignalSource IO a) #-}
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
triggerSignalHandlers :: Comp m => SignalHandlerQueue m a -> a -> Point m -> m ()
{-# INLINABLE triggerSignalHandlers #-}
{-# SPECIALISE triggerSignalHandlers :: SignalHandlerQueue IO a -> a -> Point IO -> IO () #-}
triggerSignalHandlers q a p =
  do hs <- readProtoRef (queueList q)
     forM_ hs $ \h ->
       invokeEvent p $ handlerComp h a
            
-- | Enqueue the handler and return its representative in the queue.            
enqueueSignalHandler :: Comp m => SignalHandlerQueue m a -> (a -> Event m ()) -> SessionMarker m -> m (SignalHandler m a)
{-# INLINABLE enqueueSignalHandler #-}
{-# SPECIALISE enqueueSignalHandler :: SignalHandlerQueue IO a -> (a -> Event IO ()) -> SessionMarker IO -> IO (SignalHandler IO a) #-}
enqueueSignalHandler q h m = 
  do let handler = SignalHandler { handlerComp   = h,
                                   handlerMarker = m }
     modifyProtoRef (queueList q) (handler :)
     return handler

-- | Dequeue the handler representative.
dequeueSignalHandler :: Comp m => SignalHandlerQueue m a -> SignalHandler m a -> m ()
{-# INLINABLE dequeueSignalHandler #-}
{-# SPECIALISE dequeueSignalHandler :: SignalHandlerQueue IO a -> SignalHandler IO a -> IO () #-}
dequeueSignalHandler q h = 
  modifyProtoRef (queueList q) (delete h)

instance Comp m => Functor (Signal m) where

  {-# SPECIALISE INLINE fmap :: (a -> b) -> Signal IO a -> Signal IO b #-}
  fmap = mapSignal
  
instance Comp m => Monoid (Signal m a) where 

  {-# SPECIALISE INLINE mempty :: Signal IO a #-}
  mempty = emptySignal

  {-# SPECIALISE INLINE mappend :: Signal IO a -> Signal IO a -> Signal IO a #-}
  mappend = merge2Signals

  {-# SPECIALISE INLINE mconcat :: [Signal IO a] -> Signal IO a #-}
  mconcat [] = emptySignal
  mconcat [x1] = x1
  mconcat [x1, x2] = merge2Signals x1 x2
  mconcat [x1, x2, x3] = merge3Signals x1 x2 x3
  mconcat [x1, x2, x3, x4] = merge4Signals x1 x2 x3 x4
  mconcat [x1, x2, x3, x4, x5] = merge5Signals x1 x2 x3 x4 x5
  mconcat (x1 : x2 : x3 : x4 : x5 : xs) = 
    mconcat $ merge5Signals x1 x2 x3 x4 x5 : xs
  
-- | Map the signal according the specified function.
mapSignal :: Comp m => (a -> b) -> Signal m a -> Signal m b
{-# INLINABLE mapSignal #-}
{-# SPECIALISE mapSignal :: (a -> b) -> Signal IO a -> Signal IO b #-}
mapSignal f m =
  Signal { handleSignal = \h -> 
            handleSignal m $ h . f }

-- | Filter only those signal values that satisfy to 
-- the specified predicate.
filterSignal :: Comp m => (a -> Bool) -> Signal m a -> Signal m a
{-# INLINABLE filterSignal #-}
{-# SPECIALISE filterSignal :: (a -> Bool) -> Signal IO a -> Signal IO a #-}
filterSignal p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            when (p a) $ h a }
  
-- | Filter only those signal values that satisfy to 
-- the specified predicate.
filterSignalM :: Comp m => (a -> Event m Bool) -> Signal m a -> Signal m a
{-# INLINABLE filterSignalM #-}
{-# SPECIALISE filterSignalM :: (a -> Event IO Bool) -> Signal IO a -> Signal IO a #-}
filterSignalM p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            do x <- p a
               when x $ h a }
  
-- | Merge two signals.
merge2Signals :: Comp m => Signal m a -> Signal m a -> Signal m a
{-# INLINABLE merge2Signals #-}
{-# SPECIALISE merge2Signals :: Signal IO a -> Signal IO a -> Signal IO a #-}
merge2Signals m1 m2 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               return $ x1 <> x2 }

-- | Merge three signals.
merge3Signals :: Comp m => Signal m a -> Signal m a -> Signal m a -> Signal m a
{-# INLINABLE merge3Signals #-}
{-# SPECIALISE merge3Signals :: Signal IO a -> Signal IO a -> Signal IO a -> Signal IO a #-}
merge3Signals m1 m2 m3 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               return $ x1 <> x2 <> x3 }

-- | Merge four signals.
merge4Signals :: Comp m
                 => Signal m a -> Signal m a -> Signal m a
                 -> Signal m a -> Signal m a
{-# INLINABLE merge4Signals #-}
{-# SPECIALISE merge4Signals :: Signal IO a -> Signal IO a -> Signal IO a -> Signal IO a -> Signal IO a #-}
merge4Signals m1 m2 m3 m4 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               x4 <- handleSignal m4 h
               return $ x1 <> x2 <> x3 <> x4 }
           
-- | Merge five signals.
merge5Signals :: Comp m
                 => Signal m a -> Signal m a -> Signal m a
                 -> Signal m a -> Signal m a -> Signal m a
{-# INLINABLE merge5Signals #-}
{-# SPECIALISE merge5Signals :: Signal IO a -> Signal IO a -> Signal IO a -> Signal IO a -> Signal IO a -> Signal IO a #-}
merge5Signals m1 m2 m3 m4 m5 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               x4 <- handleSignal m4 h
               x5 <- handleSignal m5 h
               return $ x1 <> x2 <> x3 <> x4 <> x5 }

-- | Compose the signal.
mapSignalM :: Comp m => (a -> Event m b) -> Signal m a -> Signal m b
{-# INLINABLE mapSignalM #-}
{-# SPECIALISE mapSignalM :: (a -> Event IO b) -> Signal IO a -> Signal IO b #-}
mapSignalM f m =
  Signal { handleSignal = \h ->
            handleSignal m (f >=> h) }
  
-- | Transform the signal.
apSignal :: Comp m => Event m (a -> b) -> Signal m a -> Signal m b
{-# INLINABLE apSignal #-}
{-# SPECIALISE apSignal :: Event IO (a -> b) -> Signal IO a -> Signal IO b #-}
apSignal f m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a -> do { x <- f; h (x a) } }

-- | An empty signal which is never triggered.
emptySignal :: Comp m => Signal m a
{-# INLINABLE emptySignal #-}
{-# SPECIALISE emptySignal :: Signal IO a #-}
emptySignal =
  Signal { handleSignal = \h -> return mempty }
                                    
-- | Represents the history of the signal values.
data SignalHistory m a =
  SignalHistory { signalHistorySignal :: Signal m a,  
                  -- ^ The signal for which the history is created.
                  signalHistoryTimes  :: UV.Vector m Double,
                  signalHistoryValues :: V.Vector m a }

-- | Create a history of the signal values.
newSignalHistory :: Comp m => Signal m a -> Event m (SignalHistory m a)
{-# INLINABLE newSignalHistory #-}
{-# SPECIALISE newSignalHistory :: Signal IO a -> Event IO (SignalHistory IO a) #-}
newSignalHistory =
  newSignalHistoryStartingWith Nothing

-- | Create a history of the signal values starting with
-- the optional initial value.
newSignalHistoryStartingWith :: Comp m => Maybe a -> Signal m a -> Event m (SignalHistory m a)
{-# INLINABLE newSignalHistoryStartingWith #-}
{-# SPECIALISE newSignalHistoryStartingWith :: Maybe a -> Signal IO a -> Event IO (SignalHistory IO a) #-}
newSignalHistoryStartingWith init signal =
  Event $ \p ->
  do let s = runSession $ pointRun p
     ts <- UV.newVector s
     xs <- V.newVector s
     case init of
       Nothing -> return ()
       Just a ->
         do UV.appendVector ts (pointTime p)
            V.appendVector xs a
     invokeEvent p $
       handleSignal_ signal $ \a ->
       Event $ \p ->
       do UV.appendVector ts (pointTime p)
          V.appendVector xs a
     return SignalHistory { signalHistorySignal = signal,
                            signalHistoryTimes  = ts,
                            signalHistoryValues = xs }
       
-- | Read the history of signal values.
readSignalHistory :: Comp m => SignalHistory m a -> Event m (Array Int Double, Array Int a)
{-# INLINABLE readSignalHistory #-}
{-# SPECIALISE readSignalHistory :: SignalHistory IO a -> Event IO (Array Int Double, Array Int a) #-}
readSignalHistory history =
  Event $ \p ->
  do xs <- UV.freezeVector (signalHistoryTimes history)
     ys <- V.freezeVector (signalHistoryValues history)
     return (xs, ys)     
     
-- | Trigger the signal with the current time.
triggerSignalWithCurrentTime :: Comp m => SignalSource m Double -> Event m ()
{-# INLINABLE triggerSignalWithCurrentTime #-}
{-# SPECIALISE triggerSignalWithCurrentTime :: SignalSource IO Double -> Event IO () #-}
triggerSignalWithCurrentTime s =
  Event $ \p -> invokeEvent p $ triggerSignal s (pointTime p)

-- | Return a signal that is triggered in the specified time points.
newSignalInTimes :: Comp m => [Double] -> Event m (Signal m Double)
{-# INLINABLE newSignalInTimes #-}
{-# SPECIALISE newSignalInTimes :: [Double] -> Event IO (Signal IO Double) #-}
newSignalInTimes xs =
  do s <- liftSimulation newSignalSource
     enqueueEventWithTimes xs $ triggerSignalWithCurrentTime s
     return $ publishSignal s
       
-- | Return a signal that is triggered in the integration time points.
-- It should be called with help of 'runEventInStartTime'.
newSignalInIntegTimes :: Comp m => Event m (Signal m Double)
{-# INLINABLE newSignalInIntegTimes #-}
{-# SPECIALISE newSignalInIntegTimes :: Event IO (Signal IO Double) #-}
newSignalInIntegTimes =
  do s <- liftSimulation newSignalSource
     enqueueEventWithIntegTimes $ triggerSignalWithCurrentTime s
     return $ publishSignal s
     
-- | Return a signal that is triggered in the start time.
-- It should be called with help of 'runEventInStartTime'.
newSignalInStartTime :: Comp m => Event m (Signal m Double)
{-# INLINABLE newSignalInStartTime #-}
{-# SPECIALISE newSignalInStartTime :: Event IO (Signal IO Double) #-}
newSignalInStartTime =
  do s <- liftSimulation newSignalSource
     t <- liftParameter starttime
     enqueueEvent t $ triggerSignalWithCurrentTime s
     return $ publishSignal s

-- | Return a signal that is triggered in the final time.
newSignalInStopTime :: Comp m => Event m (Signal m Double)
{-# INLINABLE newSignalInStopTime #-}
{-# SPECIALISE newSignalInStopTime :: Event IO (Signal IO Double) #-}
newSignalInStopTime =
  do s <- liftSimulation newSignalSource
     t <- liftParameter stoptime
     enqueueEvent t $ triggerSignalWithCurrentTime s
     return $ publishSignal s

-- | Describes a computation that also signals when changing its value.
data Signalable m a =
  Signalable { readSignalable :: Event m a,
               -- ^ Return a computation of the value.
               signalableChanged_ :: Signal m ()
               -- ^ Return a signal notifying that the value has changed
               -- but without providing the information about the changed value.
             }

-- | Return a signal notifying that the value has changed.
signalableChanged :: Comp m => Signalable m a -> Signal m a
{-# INLINABLE signalableChanged #-}
{-# SPECIALISE signalableChanged :: Signalable IO a -> Signal IO a #-}
signalableChanged x = mapSignalM (const $ readSignalable x) $ signalableChanged_ x

instance Functor m => Functor (Signalable m) where

  {-# SPECIALISE INLINE fmap :: (a -> b) -> Signalable IO a -> Signalable IO b #-}
  fmap f x = x { readSignalable = fmap f (readSignalable x) }

instance (Comp m, Monoid a) => Monoid (Signalable m a) where

  {-# SPECIALISE INLINE mempty :: Monoid a => Signalable IO a #-}
  mempty = emptySignalable

  {-# SPECIALISE INLINE mappend :: Monoid a => Signalable IO a -> Signalable IO a -> Signalable IO a #-}
  mappend = appendSignalable

-- | Return an identity.
emptySignalable :: (Comp m, Monoid a) => Signalable m a
{-# INLINABLE emptySignalable #-}
{-# SPECIALISE emptySignalable :: Monoid a => Signalable IO a #-}
emptySignalable =
  Signalable { readSignalable = return mempty,
               signalableChanged_ = mempty }

-- | An associative operation.
appendSignalable :: (Comp m, Monoid a) => Signalable m a -> Signalable m a -> Signalable m a
{-# INLINABLE appendSignalable #-}
{-# SPECIALISE appendSignalable :: Monoid a => Signalable IO a -> Signalable IO a -> Signalable IO a #-}
appendSignalable m1 m2 =
  Signalable { readSignalable = liftM2 (<>) (readSignalable m1) (readSignalable m2),
               signalableChanged_ = (signalableChanged_ m1) <> (signalableChanged_ m2) }

-- | Transform a signal so that the resulting signal returns a sequence of arrivals
-- saving the information about the time points at which the original signal was received.
arrivalSignal :: Comp m => Signal m a -> Signal m (Arrival a)
{-# INLINABLE arrivalSignal #-}
{-# SPECIALISE arrivalSignal :: Signal IO a -> Signal IO (Arrival a) #-}
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
                                       Just t0 -> Just (t - t0) } }
