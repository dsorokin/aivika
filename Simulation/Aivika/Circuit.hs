
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Circuit
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- CAUTION: EXPERIMENTAL AND NOT TESTED YET
--
-- It represents a circuit synchronized with the event queue.
-- The circuit has an efficient implementation of the 'Arrow'
-- related type classes. Also it allows creating the recursive
-- links with help of the proc-notation.
--
module Simulation.Aivika.Circuit
       (-- * Memoized Event
        EventMemo,
        runEventMemo,
        newEventMemo,
         -- * Circuit
        Circuit(..)) where

import qualified Control.Category as C
import Control.Arrow

import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event

-- | Represents an 'Event' computation memoized in the modeling time.
newtype EventMemo a =
  EventMemo { runEventMemo :: Event a
              -- ^ Run the memoized 'Event' computation.
            }

-- | Create a new computation memoized in the modeling time.
newEventMemo :: Event a -> Simulation (EventMemo a)
newEventMemo m =
  do x <- memoEventInTime m
     return (EventMemo x)

-- | Represents a circuit synchronized with the event queue.
-- The circuit has an efficient implementtion of the 'Arrow'
-- related type classes. Also it allows creating the recursive
-- links with help of the proc-notation.
--
newtype Circuit a b =
  Circuit { runCircuit :: EventMemo a -> Simulation (EventMemo b)
            -- ^ Run the circuit.
          }

instance C.Category Circuit where

  id = Circuit return

  Circuit x . Circuit y =
    Circuit $ \a -> y a >>= x

instance Arrow Circuit where

  arr f =
    Circuit $ \(EventMemo a) ->
    return (EventMemo $ fmap f a)

  first (Circuit f) =
    Circuit $ \(EventMemo bd) ->
    do EventMemo c <-
         f $
         EventMemo $
         Event $ \p ->
         do (b', d') <- invokeEvent p bd
            return b'
       return $
         EventMemo $
         Event $ \p ->
         do (b', d') <- invokeEvent p bd
            c' <- invokeEvent p c
            return (c', d')

  second (Circuit f) =
    Circuit $ \(EventMemo db) ->
    do EventMemo c <-
         f $
         EventMemo $
         Event $ \p ->
         do (d', b') <- invokeEvent p db
            return b'
       return $
         EventMemo $
         Event $ \p ->
         do (d', b') <- invokeEvent p db
            c' <- invokeEvent p c
            return (d', c')

  Circuit f *** Circuit g =
    Circuit $ \(EventMemo bb') ->
    do EventMemo c <-
         f $
         EventMemo $
         Event $ \p ->
         do (b, b') <- invokeEvent p bb'
            return b
       EventMemo c' <-
         g $
         EventMemo $
         Event $ \p ->
         do (b, b') <- invokeEvent p bb'
            return b'
       return $
         EventMemo $
         Event $ \p ->
         do ca  <- invokeEvent p c
            ca' <- invokeEvent p c'
            return (ca, ca')

  Circuit f &&& Circuit g =
    Circuit $ \b ->
    do EventMemo c  <- f b
       EventMemo c' <- g b
       return $
         EventMemo $
         Event $ \p ->
         do ca  <- invokeEvent p c
            ca' <- invokeEvent p c'
            return (ca, ca')

instance ArrowLoop Circuit where

  loop (Circuit f) =
    Circuit $ \(EventMemo b) ->
    mdo let bd =
              EventMemo $
              Event $ \p ->
              do b' <- invokeEvent p b
                 d' <- invokeEvent p d
                 return (b', d')
            c = Event $ \p ->
              do ~(c', d') <- invokeEvent p cd
                 return c'
            d = Event $ \p ->
              do ~(c', d') <- invokeEvent p cd
                 return d'
        EventMemo cd <- f bd
        return $ EventMemo c

instance ArrowChoice Circuit where

  left (Circuit f) =
    Circuit $ \(EventMemo bd) ->
    do EventMemo c <-
         f $
         EventMemo $
         Event $ \p ->
         do Left b <- invokeEvent p bd
            return b
       return $
         EventMemo $
         Event $ \p ->
         do bd <- invokeEvent p bd
            case bd of
              Left b ->
                do d <- invokeEvent p c
                   return $ Left d
              Right d ->
                return $ Right d
  
  right (Circuit f) =
    Circuit $ \(EventMemo db) ->
    do EventMemo c <-
         f $
         EventMemo $
         Event $ \p ->
         do Right b <- invokeEvent p db
            return b
       return $
         EventMemo $
         Event $ \p ->
         do db <- invokeEvent p db
            case db of
              Right b ->
                do d <- invokeEvent p c
                   return $ Right d
              Left d ->
                return $ Left d

  (Circuit f) +++ (Circuit g) =
    Circuit $ \(EventMemo bb') ->
    do EventMemo c <-
         f $
         EventMemo $
         Event $ \p ->
         do Left b <- invokeEvent p bb'
            return b
       EventMemo c' <-
         g $
         EventMemo $
         Event $ \p ->
         do Right b' <- invokeEvent p bb'
            return b'
       return $
         EventMemo $
         Event $ \p ->
         do bb' <- invokeEvent p bb'
            case bb' of
              Left b ->
                do c <- invokeEvent p c
                   return $ Left c
              Right b' ->
                do c' <- invokeEvent p c'
                   return $ Right c'

  (Circuit f) ||| (Circuit g) =
    Circuit $ \(EventMemo bc) ->
    do EventMemo db <-
         f $
         EventMemo $
         Event $ \p ->
         do Left b <- invokeEvent p bc
            return b
       EventMemo dc <-
         g $
         EventMemo $
         Event $ \p ->
         do Right c <- invokeEvent p bc
            return c
       return $
         EventMemo $
         Event $ \p ->
         do bc <- invokeEvent p bc
            case bc of
              Left b  -> invokeEvent p db
              Right c -> invokeEvent p dc
