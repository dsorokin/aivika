
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
       (-- * Circuit Type
        Circuit(..)) where

import qualified Control.Category as C
import Control.Arrow

import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event

-- | Represents a circuit synchronized with the event queue.
-- The circuit has an efficient implementtion of the 'Arrow'
-- related type classes. Also it allows creating the recursive
-- links with help of the proc-notation.
--
newtype Circuit a b =
  Circuit { runCircuit :: Event a -> Simulation (Event b)
            -- ^ Run the circuit.
          }

instance C.Category Circuit where

  id = Circuit return

  Circuit x . Circuit y =
    Circuit $ \a -> y a >>= x

instance Arrow Circuit where

  arr f =
    Circuit $ \a ->
    return (fmap f a)

  first (Circuit f) =
    Circuit $ \bd ->
    do memoizedBD <- memoEventInTime bd
       c <- f $ Event $ \p ->
         do (b', d') <- invokeEvent p memoizedBD
            return b'
       return $
         Event $ \p ->
         do (b', d') <- invokeEvent p memoizedBD
            c' <- invokeEvent p c
            return (c', d')

  second (Circuit f) =
    Circuit $ \db ->
    do memoizedDB <- memoEventInTime db
       c <- f $ Event $ \p ->
         do (d', b') <- invokeEvent p memoizedDB
            return b'
       return $
         Event $ \p ->
         do (d', b') <- invokeEvent p memoizedDB
            c' <- invokeEvent p c
            return (d', c')

  Circuit f *** Circuit g =
    Circuit $ \bb' ->
    do memoizedBB' <- memoEventInTime bb'
       c  <- f $ Event $ \p ->
         do (b, b') <- invokeEvent p memoizedBB'
            return b
       c' <- g $ Event $ \p ->
         do (b, b') <- invokeEvent p memoizedBB'
            return b'
       return $
         Event $ \p ->
         do ca  <- invokeEvent p c
            ca' <- invokeEvent p c'
            return (ca, ca')

  Circuit f &&& Circuit g =
    Circuit $ \b ->
    do memoizedB <- memoEventInTime b
       c  <- f memoizedB
       c' <- g memoizedB
       return $
         Event $ \p ->
         do ca  <- invokeEvent p c
            ca' <- invokeEvent p c'
            return (ca, ca')

instance ArrowLoop Circuit where

  loop (Circuit f) =
    Circuit $ \b ->
    mdo memoizedB  <- memoEventInTime b
        memoizedD  <- memoEventInTime d  -- pro forma
        memoizedCD <- memoEventInTime cd
        let bd = Event $ \p ->
              do b' <- invokeEvent p memoizedB
                 d' <- invokeEvent p memoizedD
                 return (b', d')
            c  = Event $ \p ->
              do ~(c', d') <- invokeEvent p memoizedCD
                 return c'
            d  = Event $ \p ->
              do ~(c', d') <- invokeEvent p memoizedCD
                 return d'
        cd <- f bd
        return c

instance ArrowChoice Circuit where

  left (Circuit f) =
    Circuit $ \bd ->
    do memoizedBD <- memoEventInTime bd
       c <- f $ Event $ \p ->
         do Left b <- invokeEvent p memoizedBD
            return b
       return $ Event $ \p ->
         do bd <- invokeEvent p memoizedBD
            case bd of
              Left b ->
                do d <- invokeEvent p c
                   return $ Left d
              Right d ->
                return $ Right d
  
  right (Circuit f) =
    Circuit $ \db ->
    do memoizedDB <- memoEventInTime db
       c <- f $ Event $ \p ->
         do Right b <- invokeEvent p memoizedDB
            return b
       return $ Event $ \p ->
         do db <- invokeEvent p memoizedDB
            case db of
              Right b ->
                do d <- invokeEvent p c
                   return $ Right d
              Left d ->
                return $ Left d

  (Circuit f) +++ (Circuit g) =
    Circuit $ \bb' ->
    do memoizedBB' <- memoEventInTime bb'
       c  <- f $ Event $ \p ->
         do Left b <- invokeEvent p memoizedBB'
            return b
       c' <- g $ Event $ \p ->
         do Right b' <- invokeEvent p memoizedBB'
            return b'
       return $ Event $ \p ->
         do bb' <- invokeEvent p memoizedBB'
            case bb' of
              Left b ->
                do c <- invokeEvent p c
                   return $ Left c
              Right b' ->
                do c' <- invokeEvent p c'
                   return $ Right c'

  (Circuit f) ||| (Circuit g) =
    Circuit $ \bc ->
    do memoizedBC <- memoEventInTime bc
       db <- f $ Event $ \p ->
         do Left b <- invokeEvent p memoizedBC
            return b
       dc <- g $ Event $ \p ->
         do Right c <- invokeEvent p memoizedBC
            return c
       return $ Event $ \p ->
         do bc <- invokeEvent p memoizedBC
            case bc of
              Left b  -> invokeEvent p db
              Right c -> invokeEvent p dc
