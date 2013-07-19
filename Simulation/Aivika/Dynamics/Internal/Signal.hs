
-- |
-- Module     : Simulation.Aivika.Dynamics.Internal.Signal
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

module Simulation.Aivika.Dynamics.Internal.Signal
       (Signal,
        SignalSource,
        newSignalSourceWithUpdate,
        newSignalSourceUnsafe,
        publishSignal,
        triggerSignal,
        handleSignal,
        handleSignal_,
        updateSignal,
        mapSignal,
        mapSignalM,
        apSignal,
        filterSignal,
        filterSignalM,
        emptySignal,
        merge2Signals,
        merge3Signals,
        merge4Signals,
        merge5Signals) where

import Data.IORef
import Data.Monoid

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.Internal.Simulation

-- | The signal source that can publish its signal.
data SignalSource a =
  SignalSource { publishSignal :: Signal a,
                                  -- ^ Publish the signal.
                 triggerSignal :: a -> Dynamics ()
                                  -- ^ Trigger the signal actuating 
                                  -- all its handlers at the current 
                                  -- simulation time point.
               }
  
-- | The signal that can have disposable handlers.  
data Signal a =
  Signal { handleSignal :: (a -> Dynamics ()) -> 
                           Dynamics (Dynamics ()),
           -- ^ Subscribe the handler to the specified 
           -- signal and return a nested computation 
           -- that, being applied, unsubscribes the 
           -- handler from this signal.
           --
           -- If the signal is bound up with the event queue
           -- then the signal in the current time is not lost
           -- by the handler any more. Changed in version 0.6.1.
           updateSignal :: Dynamics ()
           -- ^ Update the signal to its actual state.
         }
  
-- | The queue of signal handlers.
data SignalHandlerQueue a =
  SignalHandlerQueue { queueStart :: IORef (Maybe (SignalHandler a)),
                       queueEnd   :: IORef (Maybe (SignalHandler a)) }
  
-- | It contains the information about the disposable queue handler.
data SignalHandler a =
  SignalHandler { handlerComp :: a -> Dynamics (),
                  handlerPrev :: IORef (Maybe (SignalHandler a)),
                  handlerNext :: IORef (Maybe (SignalHandler a)) }

-- | Subscribe the handler to the specified signal.
-- To subscribe the disposable handlers, use function 'handleSignal'.
--
-- If the signal is bound up with the event queue then the signal in
-- the current time is not lost by the handler any more.
-- Changed in version 0.6.1.
handleSignal_ :: Signal a -> (a -> Dynamics ()) -> Dynamics ()
handleSignal_ signal h = 
  do x <- handleSignal signal h
     return ()
     
-- | Create a new signal source with the specified update computation.
newSignalSourceWithUpdate :: Dynamics () -> Simulation (SignalSource a)
newSignalSourceWithUpdate update =
  Simulation $ \r ->
  do start <- newIORef Nothing
     end <- newIORef Nothing
     let queue  = SignalHandlerQueue { queueStart = start,
                                       queueEnd   = end }
         signal = Signal { handleSignal = handle, 
                           updateSignal = update }
         source = SignalSource { publishSignal = signal, 
                                 triggerSignal = trigger }
         handle h =
           Dynamics $ \p ->
           do invokeDynamics p update
              x <- enqueueSignalHandler queue h
              return $ 
                Dynamics $ \p ->
                do invokeDynamics p update
                   dequeueSignalHandler queue x
         trigger a =
           Dynamics $ \p ->
           do invokeDynamics p update 
              let h = queueStart queue
              triggerSignalHandlers h a p
     return source
     
-- | Create a new signal source that has no update computation.
newSignalSourceUnsafe :: Simulation (SignalSource a)
newSignalSourceUnsafe =
  Simulation $ \r ->
  do start <- newIORef Nothing
     end <- newIORef Nothing
     let queue  = SignalHandlerQueue { queueStart = start,
                                       queueEnd   = end }
         signal = Signal { handleSignal = handle, 
                           updateSignal = update }
         source = SignalSource { publishSignal = signal, 
                                 triggerSignal = trigger }
         handle h =
           Dynamics $ \p ->
           do x <- enqueueSignalHandler queue h
              return $ liftIO $ dequeueSignalHandler queue x
         trigger a =
           Dynamics $ \p ->
           let h = queueStart queue
           in triggerSignalHandlers h a p
         update = return ()
     return source

-- | Trigger all next signal handlers.
triggerSignalHandlers :: IORef (Maybe (SignalHandler a)) -> a -> Point -> IO ()
{-# INLINE triggerSignalHandlers #-}
triggerSignalHandlers r a p =
  do x <- readIORef r
     case x of
       Nothing -> return ()
       Just h ->
         do let Dynamics m = handlerComp h a
            m p
            triggerSignalHandlers (handlerNext h) a p
            
-- | Enqueue the handler and return its representative in the queue.            
enqueueSignalHandler :: SignalHandlerQueue a -> (a -> Dynamics ()) -> IO (SignalHandler a)
enqueueSignalHandler q h = 
  do tail <- readIORef (queueEnd q)
     case tail of
       Nothing ->
         do prev <- newIORef Nothing
            next <- newIORef Nothing
            let handler = SignalHandler { handlerComp = h,
                                          handlerPrev = prev,
                                          handlerNext = next }
            writeIORef (queueStart q) (Just handler)
            writeIORef (queueEnd q) (Just handler)
            return handler
       Just x ->
         do prev <- newIORef tail
            next <- newIORef Nothing
            let handler = SignalHandler { handlerComp = h,
                                          handlerPrev = prev,
                                          handlerNext = next }
            writeIORef (handlerNext x) (Just handler)
            writeIORef (queueEnd q) (Just handler)
            return handler

-- | Dequeue the handler representative.
dequeueSignalHandler :: SignalHandlerQueue a -> SignalHandler a -> IO ()
dequeueSignalHandler q h = 
  do prev <- readIORef (handlerPrev h)
     case prev of
       Nothing ->
         do next <- readIORef (handlerNext h)
            case next of
              Nothing ->
                do writeIORef (queueStart q) Nothing
                   writeIORef (queueEnd q) Nothing
              Just y ->
                do writeIORef (handlerPrev y) Nothing
                   writeIORef (handlerNext h) Nothing
                   writeIORef (queueStart q) next
       Just x ->
         do next <- readIORef (handlerNext h)
            case next of
              Nothing ->
                do writeIORef (handlerPrev h) Nothing
                   writeIORef (handlerNext x) Nothing
                   writeIORef (queueEnd q) prev
              Just y ->
                do writeIORef (handlerPrev h) Nothing
                   writeIORef (handlerNext h) Nothing
                   writeIORef (handlerPrev y) prev
                   writeIORef (handlerNext x) next

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
            handleSignal m $ h . f, 
           updateSignal = 
             updateSignal m }

-- | Filter only those signal values that satisfy to 
-- the specified predicate.
filterSignal :: (a -> Bool) -> Signal a -> Signal a
filterSignal p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            when (p a) $ h a, 
           updateSignal =
             updateSignal m }
  
-- | Filter only those signal values that satisfy to 
-- the specified predicate.
filterSignalM :: (a -> Dynamics Bool) -> Signal a -> Signal a
filterSignalM p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            do x <- p a
               when x $ h a, 
           updateSignal =
             updateSignal m }
  
-- | Merge two signals.
merge2Signals :: Signal a -> Signal a -> Signal a
merge2Signals m1 m2 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               return $ do { x1; x2 }, 
           updateSignal =
             do updateSignal m1
                updateSignal m2 }

-- | Merge three signals.
merge3Signals :: Signal a -> Signal a -> Signal a -> Signal a
merge3Signals m1 m2 m3 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               return $ do { x1; x2; x3 },
           updateSignal =
             do updateSignal m1
                updateSignal m2 
                updateSignal m3 }

-- | Merge four signals.
merge4Signals :: Signal a -> Signal a -> Signal a -> 
                 Signal a -> Signal a
merge4Signals m1 m2 m3 m4 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               x4 <- handleSignal m4 h
               return $ do { x1; x2; x3; x4 },
           updateSignal =
             do updateSignal m1
                updateSignal m2 
                updateSignal m3 
                updateSignal m4 }
           
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
               return $ do { x1; x2; x3; x4; x5 },
           updateSignal =
             do updateSignal m1
                updateSignal m2 
                updateSignal m3 
                updateSignal m4
                updateSignal m5 }

-- | Compose the signal.
mapSignalM :: (a -> Dynamics b) -> Signal a -> Signal b
mapSignalM f m =
  Signal { handleSignal = \h ->
            handleSignal m (f >=> h),
           updateSignal = 
             updateSignal m }
  
-- | Transform the signal.
apSignal :: Dynamics (a -> b) -> Signal a -> Signal b
apSignal f m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a -> do { x <- f; h (x a) },
           updateSignal =
             updateSignal m }

-- | An empty signal which is never triggered.
emptySignal :: Signal a
emptySignal =
  Signal { handleSignal = \h -> return $ return (),
           updateSignal = return () }
  
invokeDynamics :: Point -> Dynamics a -> IO a
{-# INLINE invokeDynamics #-}
invokeDynamics p (Dynamics m) = m p
