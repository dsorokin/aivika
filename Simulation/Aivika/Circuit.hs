
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Circuit
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- It represents a circuit synchronized with the event queue.
-- Also it allows creating the recursive links with help of
-- the proc-notation.
--
-- The implementation is based on the <http://en.wikibooks.org/wiki/Haskell/Arrow_tutorial Arrow Tutorial>.
--
module Simulation.Aivika.Circuit
       (-- * Circuit Arrow
        Circuit(..),
        -- * Circuit Primitives
        arrCircuit,
        accumCircuit,
        -- * Arrival Circuit
        arrivalCircuit,
        -- * Delaying Circuit
        delayCircuit,
        -- * Time Circuit
        timeCircuit,
        -- * Converting to Signals and Processors
        circuitSignaling,
        circuitProcessor) where

import qualified Control.Category as C
import Control.Arrow
import Control.Monad.Fix

import Data.IORef

import Simulation.Aivika.Internal.Arrival
import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Signal
import Simulation.Aivika.Stream
import Simulation.Aivika.Processor

-- | Represents a circuit synchronized with the event queue.
-- Besides, it allows creating the recursive links with help of
-- the proc-notation.
--
newtype Circuit a b =
  Circuit { runCircuit :: a -> Event (Circuit a b, b)
            -- ^ Run the circuit.
          }

instance C.Category Circuit where

  id = Circuit $ \a -> return (C.id, a)

  (.) = dot
    where 
      (Circuit g) `dot` (Circuit f) =
        Circuit $ \a ->
        Event $ \p ->
        do (cir1, b) <- invokeEvent p (f a)
           (cir2, c) <- invokeEvent p (g b)
           return (cir2 `dot` cir1, c)

instance Arrow Circuit where

  arr f = Circuit $ \a -> return (arr f, f a)

  first (Circuit f) =
    Circuit $ \(b, d) ->
    Event $ \p ->
    do (cir, c) <- invokeEvent p (f b)
       return (first cir, (c, d))

  second (Circuit f) =
    Circuit $ \(d, b) ->
    Event $ \p ->
    do (cir, c) <- invokeEvent p (f b)
       return (second cir, (d, c))

  (Circuit f) *** (Circuit g) =
    Circuit $ \(b, b') ->
    Event $ \p ->
    do (cir1, c) <- invokeEvent p (f b)
       (cir2, c') <- invokeEvent p (g b')
       return (cir1 *** cir2, (c, c'))
       
  (Circuit f) &&& (Circuit g) =
    Circuit $ \b ->
    Event $ \p ->
    do (cir1, c) <- invokeEvent p (f b)
       (cir2, c') <- invokeEvent p (g b)
       return (cir1 &&& cir2, (c, c'))

instance ArrowLoop Circuit where

  loop (Circuit f) =
    Circuit $ \b ->
    Event $ \p ->
    do rec (cir, (c, d)) <- invokeEvent p (f (b, d))
       return (loop cir, c)

instance ArrowChoice Circuit where

  left x@(Circuit f) =
    Circuit $ \ebd ->
    Event $ \p ->
    case ebd of
      Left b ->
        do (cir, c) <- invokeEvent p (f b)
           return (left cir, Left c)
      Right d ->
        return (left x, Right d)

  right x@(Circuit f) =
    Circuit $ \edb ->
    Event $ \p ->
    case edb of
      Right b ->
        do (cir, c) <- invokeEvent p (f b)
           return (right cir, Right c)
      Left d ->
        return (right x, Left d)

  x@(Circuit f) +++ y@(Circuit g) =
    Circuit $ \ebb' ->
    Event $ \p ->
    case ebb' of
      Left b ->
        do (cir1, c) <- invokeEvent p (f b)
           return (cir1 +++ y, Left c)
      Right b' ->
        do (cir2, c') <- invokeEvent p (g b')
           return (x +++ cir2, Right c')

  x@(Circuit f) ||| y@(Circuit g) =
    Circuit $ \ebc ->
    Event $ \p ->
    case ebc of
      Left b ->
        do (cir1, d) <- invokeEvent p (f b)
           return (cir1 ||| y, d)
      Right b' ->
        do (cir2, d) <- invokeEvent p (g b')
           return (x ||| cir2, d)

-- | Get a signal transform by the specified circuit.
circuitSignaling :: Circuit a b -> Signal a -> Signal b
circuitSignaling (Circuit cir) sa =
  Signal { handleSignal = \f ->
            Event $ \p ->
            do r <- newIORef cir
               invokeEvent p $
                 handleSignal sa $ \a ->
                 Event $ \p ->
                 do cir <- readIORef r
                    (Circuit cir', b) <- invokeEvent p (cir a)
                    writeIORef r cir'
                    invokeEvent p (f b) }

-- | Transform the circuit to a processor.
circuitProcessor :: Circuit a b -> Processor a b
circuitProcessor (Circuit cir) = Processor $ \sa ->
  Cons $
  do (a, xs) <- runStream sa
     (cir', b) <- liftEvent (cir a)
     let f = runProcessor (circuitProcessor cir')
     return (b, f xs)

-- | Lift the 'Event' function to a curcuit.
arrCircuit :: (a -> Event b) -> Circuit a b
arrCircuit f =
  let x =
        Circuit $ \a ->
        Event $ \p ->
        do b <- invokeEvent p (f a)
           return (x, b)
  in x

-- | Accumulator that outputs a value determined by the supplied function.
accumCircuit :: (acc -> a -> Event (acc, b)) -> acc -> Circuit a b
accumCircuit f acc =
  Circuit $ \a ->
  Event $ \p ->
  do (acc', b) <- invokeEvent p (f acc a)
     return (accumCircuit f acc', b) 

-- | A circuit that adds the information about the time points at which 
-- the values were received.
arrivalCircuit :: Circuit a (Arrival a)
arrivalCircuit =
  let loop t0 =
        Circuit $ \a ->
        Event $ \p ->
        let t = pointTime p
            b = Arrival { arrivalValue = a,
                          arrivalTime  = t,
                          arrivalDelay = 
                            case t0 of
                              Nothing -> 0
                              Just t0 -> t - t0 }
        in return (loop $ Just t, b)
  in loop Nothing

-- | Delay the input by one step using the specified initial value.
delayCircuit :: a -> Circuit a a
delayCircuit a0 =
  Circuit $ \a ->
  return (delayCircuit a, a0)

-- | A circuit that returns the current modeling time.
timeCircuit :: Circuit a Double
timeCircuit =
  Circuit $ \a ->
  Event $ \p ->
  return (timeCircuit, pointTime p)