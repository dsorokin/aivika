
-- |
-- Module     : Simulation.Aivika.Internal.Signal
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines the signal which we can subscribe handlers to. 
-- These handlers can be disposed. The signal is triggered in the 
-- current time point actuating the corresponded computations from 
-- the handlers. 
--

module Simulation.Aivika.Internal.Signal
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

import Data.IORef
import Data.Monoid
import Data.List
import Data.Array

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Arrival

import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.Vector.Unboxed as UV

-- | The signal source that can publish its signal.
data SignalSource a =
  SignalSource { publishSignal :: Signal a,
                                  -- ^ Publish the signal.
                 triggerSignal :: a -> Event ()
                                  -- ^ Trigger the signal actuating 
                                  -- all its handlers at the current 
                                  -- simulation time point.
               }
  
-- | The signal that can have disposable handlers.  
data Signal a =
  Signal { handleSignal :: (a -> Event ()) -> Event (Event ())
           -- ^ Subscribe the handler to the specified 
           -- signal and return a nested computation 
           -- that, being applied, unsubscribes the 
           -- handler from this signal.
         }
  
-- | The queue of signal handlers.
data SignalHandlerQueue a =
  SignalHandlerQueue { queueList :: IORef [SignalHandler a] }
  
-- | It contains the information about the disposable queue handler.
data SignalHandler a =
  SignalHandler { handlerComp :: a -> Event (),
                  handlerRef  :: IORef () }

instance Eq (SignalHandler a) where
  x == y = (handlerRef x) == (handlerRef y)

-- | Subscribe the handler to the specified signal.
-- To subscribe the disposable handlers, use function 'handleSignal'.
handleSignal_ :: Signal a -> (a -> Event ()) -> Event ()
handleSignal_ signal h = 
  do x <- handleSignal signal h
     return ()
     
-- | Create a new signal source.
newSignalSource :: Simulation (SignalSource a)
newSignalSource =
  Simulation $ \r ->
  do list <- newIORef []
     let queue  = SignalHandlerQueue { queueList = list }
         signal = Signal { handleSignal = handle }
         source = SignalSource { publishSignal = signal, 
                                 triggerSignal = trigger }
         handle h =
           Event $ \p ->
           do x <- enqueueSignalHandler queue h
              return $
                Event $ \p -> dequeueSignalHandler queue x
         trigger a =
           Event $ \p -> triggerSignalHandlers queue a p
     return source

-- | Trigger all next signal handlers.
triggerSignalHandlers :: SignalHandlerQueue a -> a -> Point -> IO ()
{-# INLINE triggerSignalHandlers #-}
triggerSignalHandlers q a p =
  do hs <- readIORef (queueList q)
     forM_ hs $ \h ->
       invokeEvent p $ handlerComp h a
            
-- | Enqueue the handler and return its representative in the queue.            
enqueueSignalHandler :: SignalHandlerQueue a -> (a -> Event ()) -> IO (SignalHandler a)
{-# INLINE enqueueSignalHandler #-}
enqueueSignalHandler q h = 
  do r <- newIORef ()
     let handler = SignalHandler { handlerComp = h,
                                   handlerRef  = r }
     modifyIORef (queueList q) (handler :)
     return handler

-- | Dequeue the handler representative.
dequeueSignalHandler :: SignalHandlerQueue a -> SignalHandler a -> IO ()
{-# INLINE dequeueSignalHandler #-}
dequeueSignalHandler q h = 
  modifyIORef (queueList q) (delete h)

instance Functor Signal where
  fmap = mapSignal
  
instance Monoid (Signal a) where 
  
  mempty = emptySignal
  
  mappend = merge2Signals
  
  mconcat [] = emptySignal
  mconcat [x1] = x1
  mconcat [x1, x2] = merge2Signals x1 x2
  mconcat [x1, x2, x3] = merge3Signals x1 x2 x3
  mconcat [x1, x2, x3, x4] = merge4Signals x1 x2 x3 x4
  mconcat [x1, x2, x3, x4, x5] = merge5Signals x1 x2 x3 x4 x5
  mconcat (x1 : x2 : x3 : x4 : x5 : xs) = 
    mconcat $ merge5Signals x1 x2 x3 x4 x5 : xs
  
-- | Map the signal according the specified function.
mapSignal :: (a -> b) -> Signal a -> Signal b
mapSignal f m =
  Signal { handleSignal = \h -> 
            handleSignal m $ h . f }

-- | Filter only those signal values that satisfy to 
-- the specified predicate.
filterSignal :: (a -> Bool) -> Signal a -> Signal a
filterSignal p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            when (p a) $ h a }
  
-- | Filter only those signal values that satisfy to 
-- the specified predicate.
filterSignalM :: (a -> Event Bool) -> Signal a -> Signal a
filterSignalM p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            do x <- p a
               when x $ h a }
  
-- | Merge two signals.
merge2Signals :: Signal a -> Signal a -> Signal a
merge2Signals m1 m2 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               return $ do { x1; x2 } }

-- | Merge three signals.
merge3Signals :: Signal a -> Signal a -> Signal a -> Signal a
merge3Signals m1 m2 m3 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               return $ do { x1; x2; x3 } }

-- | Merge four signals.
merge4Signals :: Signal a -> Signal a -> Signal a -> 
                 Signal a -> Signal a
merge4Signals m1 m2 m3 m4 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               x4 <- handleSignal m4 h
               return $ do { x1; x2; x3; x4 } }
           
-- | Merge five signals.
merge5Signals :: Signal a -> Signal a -> Signal a -> 
                 Signal a -> Signal a -> Signal a
merge5Signals m1 m2 m3 m4 m5 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               x4 <- handleSignal m4 h
               x5 <- handleSignal m5 h
               return $ do { x1; x2; x3; x4; x5 } }

-- | Compose the signal.
mapSignalM :: (a -> Event b) -> Signal a -> Signal b
mapSignalM f m =
  Signal { handleSignal = \h ->
            handleSignal m (f >=> h) }
  
-- | Transform the signal.
apSignal :: Event (a -> b) -> Signal a -> Signal b
apSignal f m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a -> do { x <- f; h (x a) } }

-- | An empty signal which is never triggered.
emptySignal :: Signal a
emptySignal =
  Signal { handleSignal = \h -> return $ return () }
                                    
-- | Represents the history of the signal values.
data SignalHistory a =
  SignalHistory { signalHistorySignal :: Signal a,  
                  -- ^ The signal for which the history is created.
                  signalHistoryTimes  :: UV.Vector Double,
                  signalHistoryValues :: V.Vector a }

-- | Create a history of the signal values.
newSignalHistory :: Signal a -> Event (SignalHistory a)
newSignalHistory =
  newSignalHistoryStartingWith Nothing

-- | Create a history of the signal values starting with
-- the optional initial value.
newSignalHistoryStartingWith :: Maybe a -> Signal a -> Event (SignalHistory a)
newSignalHistoryStartingWith init signal =
  Event $ \p ->
  do ts <- UV.newVector
     xs <- V.newVector
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
readSignalHistory :: SignalHistory a -> Event (Array Int Double, Array Int a)
readSignalHistory history =
  do xs <- liftIO $ UV.freezeVector (signalHistoryTimes history)
     ys <- liftIO $ V.freezeVector (signalHistoryValues history)
     return (xs, ys)     
     
-- | Trigger the signal with the current time.
triggerSignalWithCurrentTime :: SignalSource Double -> Event ()
triggerSignalWithCurrentTime s =
  Event $ \p -> invokeEvent p $ triggerSignal s (pointTime p)

-- | Return a signal that is triggered in the specified time points.
newSignalInTimes :: [Double] -> Event (Signal Double)
newSignalInTimes xs =
  do s <- liftSimulation newSignalSource
     enqueueEventWithTimes xs $ triggerSignalWithCurrentTime s
     return $ publishSignal s
       
-- | Return a signal that is triggered in the integration time points.
-- It should be called with help of 'runEventInStartTime'.
newSignalInIntegTimes :: Event (Signal Double)
newSignalInIntegTimes =
  do s <- liftSimulation newSignalSource
     enqueueEventWithIntegTimes $ triggerSignalWithCurrentTime s
     return $ publishSignal s
     
-- | Return a signal that is triggered in the start time.
-- It should be called with help of 'runEventInStartTime'.
newSignalInStartTime :: Event (Signal Double)
newSignalInStartTime =
  do s <- liftSimulation newSignalSource
     t <- liftParameter starttime
     enqueueEvent t $ triggerSignalWithCurrentTime s
     return $ publishSignal s

-- | Return a signal that is triggered in the final time.
newSignalInStopTime :: Event (Signal Double)
newSignalInStopTime =
  do s <- liftSimulation newSignalSource
     t <- liftParameter stoptime
     enqueueEvent t $ triggerSignalWithCurrentTime s
     return $ publishSignal s

-- | Describes a computation that also signals when changing its value.
data Signalable a =
  Signalable { readSignalable :: Event a,
               -- ^ Return a computation of the value.
               signalableChanged_ :: Signal ()
               -- ^ Return a signal notifying that the value has changed
               -- but without providing the information about the changed value.
             }

-- | Return a signal notifying that the value has changed.
signalableChanged :: Signalable a -> Signal a
signalableChanged x = mapSignalM (const $ readSignalable x) $ signalableChanged_ x

instance Functor Signalable where
  fmap f x = x { readSignalable = fmap f (readSignalable x) }

instance Monoid a => Monoid (Signalable a) where

  mempty = emptySignalable
  mappend = appendSignalable

-- | Return an identity.
emptySignalable :: Monoid a => Signalable a
emptySignalable =
  Signalable { readSignalable = return mempty,
               signalableChanged_ = mempty }

-- | An associative operation.
appendSignalable :: Monoid a => Signalable a -> Signalable a -> Signalable a
appendSignalable m1 m2 =
  Signalable { readSignalable = liftM2 (<>) (readSignalable m1) (readSignalable m2),
               signalableChanged_ = (signalableChanged_ m1) <> (signalableChanged_ m2) }

-- | Transform a signal so that the resulting signal returns a sequence of arrivals
-- saving the information about the time points at which the original signal was received.
arrivalSignal :: Signal a -> Signal (Arrival a)
arrivalSignal m = 
  Signal { handleSignal = \h ->
             Event $ \p ->
             do r <- newIORef Nothing
                invokeEvent p $
                  handleSignal m $ \a ->
                  Event $ \p ->
                  do t0 <- readIORef r
                     let t = pointTime p
                     writeIORef r (Just t)
                     invokeEvent p $
                       h Arrival { arrivalValue = a,
                                   arrivalTime  = t,
                                   arrivalDelay =
                                     case t0 of
                                       Nothing -> Nothing
                                       Just t0 -> Just (t - t0) }
         }
