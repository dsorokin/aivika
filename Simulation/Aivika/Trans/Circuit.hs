
{-# LANGUAGE RecursiveDo, Arrows #-}

-- |
-- Module     : Simulation.Aivika.Trans.Circuit
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It represents a circuit synchronized with the event queue.
-- Also it allows creating the recursive links with help of
-- the proc-notation.
--
-- The implementation is based on the <http://en.wikibooks.org/wiki/Haskell/Arrow_tutorial Arrow Tutorial>.
--
module Simulation.Aivika.Trans.Circuit
       (-- * The Circuit Arrow
        Circuit(..),
        -- * Circuit Primitives
        arrCircuit,
        accumCircuit,
        -- * The Arrival Circuit
        arrivalCircuit,
        -- * Delaying the Circuit
        delayCircuit,
        -- * The Time Circuit
        timeCircuit,
        -- * Conditional Computation
        (<?<),
        (>?>),
        filterCircuit,
        filterCircuitM,
        neverCircuit,
        -- * Converting to Signals and Processors
        circuitSignaling,
        circuitProcessor,
        -- * Integrals and Difference Equations
        integCircuit,
        sumCircuit,
        -- * The Circuit Transform
        circuitTransform) where

import qualified Control.Category as C
import Control.Arrow
import Control.Monad.Fix

import Data.IORef

import Simulation.Aivika.Trans.Internal.Arrival
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Dynamics.Memo
import Simulation.Aivika.Trans.Transform
import Simulation.Aivika.Trans.SystemDynamics
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Stream
import Simulation.Aivika.Trans.Processor

-- | Represents a circuit synchronized with the event queue.
-- Besides, it allows creating the recursive links with help of
-- the proc-notation.
--
newtype Circuit a b =
  Circuit { runCircuit :: a -> Event (b, Circuit a b)
            -- ^ Run the circuit.
          }

instance C.Category Circuit where

  id = Circuit $ \a -> return (a, C.id)

  (.) = dot
    where 
      (Circuit g) `dot` (Circuit f) =
        Circuit $ \a ->
        Event $ \p ->
        do (b, cir1) <- invokeEvent p (f a)
           (c, cir2) <- invokeEvent p (g b)
           return (c, cir2 `dot` cir1)

instance Arrow Circuit where

  arr f = Circuit $ \a -> return (f a, arr f)

  first (Circuit f) =
    Circuit $ \(b, d) ->
    Event $ \p ->
    do (c, cir) <- invokeEvent p (f b)
       return ((c, d), first cir)

  second (Circuit f) =
    Circuit $ \(d, b) ->
    Event $ \p ->
    do (c, cir) <- invokeEvent p (f b)
       return ((d, c), second cir)

  (Circuit f) *** (Circuit g) =
    Circuit $ \(b, b') ->
    Event $ \p ->
    do (c, cir1) <- invokeEvent p (f b)
       (c', cir2) <- invokeEvent p (g b')
       return ((c, c'), cir1 *** cir2)
       
  (Circuit f) &&& (Circuit g) =
    Circuit $ \b ->
    Event $ \p ->
    do (c, cir1) <- invokeEvent p (f b)
       (c', cir2) <- invokeEvent p (g b)
       return ((c, c'), cir1 &&& cir2)

instance ArrowLoop Circuit where

  loop (Circuit f) =
    Circuit $ \b ->
    Event $ \p ->
    do rec ((c, d), cir) <- invokeEvent p (f (b, d))
       return (c, loop cir)

instance ArrowChoice Circuit where

  left x@(Circuit f) =
    Circuit $ \ebd ->
    Event $ \p ->
    case ebd of
      Left b ->
        do (c, cir) <- invokeEvent p (f b)
           return (Left c, left cir)
      Right d ->
        return (Right d, left x)

  right x@(Circuit f) =
    Circuit $ \edb ->
    Event $ \p ->
    case edb of
      Right b ->
        do (c, cir) <- invokeEvent p (f b)
           return (Right c, right cir)
      Left d ->
        return (Left d, right x)

  x@(Circuit f) +++ y@(Circuit g) =
    Circuit $ \ebb' ->
    Event $ \p ->
    case ebb' of
      Left b ->
        do (c, cir1) <- invokeEvent p (f b)
           return (Left c, cir1 +++ y)
      Right b' ->
        do (c', cir2) <- invokeEvent p (g b')
           return (Right c', x +++ cir2)

  x@(Circuit f) ||| y@(Circuit g) =
    Circuit $ \ebc ->
    Event $ \p ->
    case ebc of
      Left b ->
        do (d, cir1) <- invokeEvent p (f b)
           return (d, cir1 ||| y)
      Right b' ->
        do (d, cir2) <- invokeEvent p (g b')
           return (d, x ||| cir2)

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
                    (b, Circuit cir') <- invokeEvent p (cir a)
                    writeIORef r cir'
                    invokeEvent p (f b) }

-- | Transform the circuit to a processor.
circuitProcessor :: Circuit a b -> Processor a b
circuitProcessor (Circuit cir) = Processor $ \sa ->
  Cons $
  do (a, xs) <- runStream sa
     (b, cir') <- liftEvent (cir a)
     let f = runProcessor (circuitProcessor cir')
     return (b, f xs)

-- | Create a simple circuit by the specified handling function
-- that runs the computation for each input value to get an output.
arrCircuit :: (a -> Event b) -> Circuit a b
arrCircuit f =
  let x =
        Circuit $ \a ->
        Event $ \p ->
        do b <- invokeEvent p (f a)
           return (b, x)
  in x

-- | Accumulator that outputs a value determined by the supplied function.
accumCircuit :: (acc -> a -> Event (acc, b)) -> acc -> Circuit a b
accumCircuit f acc =
  Circuit $ \a ->
  Event $ \p ->
  do (acc', b) <- invokeEvent p (f acc a)
     return (b, accumCircuit f acc') 

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
                              Nothing -> Nothing
                              Just t0 -> Just (t - t0) }
        in return (b, loop $ Just t)
  in loop Nothing

-- | Delay the input by one step using the specified initial value.
delayCircuit :: a -> Circuit a a
delayCircuit a0 =
  Circuit $ \a ->
  return (a0, delayCircuit a)

-- | A circuit that returns the current modeling time.
timeCircuit :: Circuit a Double
timeCircuit =
  Circuit $ \a ->
  Event $ \p ->
  return (pointTime p, timeCircuit)

-- | Like '>>>' but processes only the represented events.
(>?>) :: Circuit a (Maybe b)
         -- ^ whether there is an event
         -> Circuit b c
         -- ^ process the event if it presents
         -> Circuit a (Maybe c)
         -- ^ the resulting circuit that processes only the represented events
whether >?> process =
  Circuit $ \a ->
  Event $ \p ->
  do (b, whether') <- invokeEvent p (runCircuit whether a)
     case b of
       Nothing ->
         return (Nothing, whether' >?> process)
       Just b  ->
         do (c, process') <- invokeEvent p (runCircuit process b)
            return (Just c, whether' >?> process')

-- | Like '<<<' but processes only the represented events.
(<?<) :: Circuit b c
         -- ^ process the event if it presents
         -> Circuit a (Maybe b)
         -- ^ whether there is an event
         -> Circuit a (Maybe c)
         -- ^ the resulting circuit that processes only the represented events
(<?<) = flip (>?>)

-- | Filter the circuit, calculating only those parts of the circuit that satisfy
-- the specified predicate.
filterCircuit :: (a -> Bool) -> Circuit a b -> Circuit a (Maybe b)
filterCircuit pred = filterCircuitM (return . pred)

-- | Filter the circuit within the 'Event' computation, calculating only those parts
-- of the circuit that satisfy the specified predicate.
filterCircuitM :: (a -> Event Bool) -> Circuit a b -> Circuit a (Maybe b)
filterCircuitM pred cir =
  Circuit $ \a ->
  Event $ \p ->
  do x <- invokeEvent p (pred a)
     if x
       then do (b, cir') <- invokeEvent p (runCircuit cir a)
               return (Just b, filterCircuitM pred cir')
       else return (Nothing, filterCircuitM pred cir)

-- | The source of events that never occur.
neverCircuit :: Circuit a (Maybe b)
neverCircuit =
  Circuit $ \a -> return (Nothing, neverCircuit)

-- | An approximation of the integral using Euler's method.
--
-- This function can be rather inaccurate as it depends on
-- the time points at wich the 'Circuit' computation is actuated.
-- Also Euler's method per se is not most accurate, although simple
-- enough for implementation.
--
-- Consider using the 'integ' function whenever possible.
-- That function can integrate with help of the Runge-Kutta method by
-- the specified integration time points that are passed in the simulation
-- specs to every 'Simulation', when running the model.
--
-- At the same time, the 'integCircuit' function has no mutable state
-- unlike the former. The latter consumes less memory but at the cost
-- of inaccuracy and relatively more slow simulation, had we requested
-- the integral in the same time points.
--
-- Regarding the recursive equations, the both functions allow defining them
-- but whithin different computations (either with help of the recursive
-- do-notation or the proc-notation).
integCircuit :: Double
                -- ^ the initial value
                -> Circuit Double Double
                -- ^ map the derivative to an integral
integCircuit init = start
  where
    start = 
      Circuit $ \a ->
      Event $ \p ->
      do let t = pointTime p
         return (init, next t init a)
    next t0 v0 a0 =
      Circuit $ \a ->
      Event $ \p ->
      do let t  = pointTime p
             dt = t - t0
             v  = v0 + a0 * dt
         v `seq` return (v, next t v a)

-- | A sum of differences starting from the specified initial value.
--
-- Consider using the more accurate 'diffsum' function whener possible as
-- it is calculated in every integration time point specified by specs
-- passed in to every 'Simulation', when running the model.
--
-- At the same time, the 'sumCircuit' function has no mutable state and
-- it consumes less memory than the former.
--
-- Regarding the recursive equations, the both functions allow defining them
-- but whithin different computations (either with help of the recursive
-- do-notation or the proc-notation).
sumCircuit :: Num a =>
              a
              -- ^ the initial value
              -> Circuit a a
              -- ^ map the difference to a sum
sumCircuit init = start
  where
    start = 
      Circuit $ \a ->
      Event $ \p ->
      return (init, next init a)
    next v0 a0 =
      Circuit $ \a ->
      Event $ \p ->
      do let v = v0 + a0
         v `seq` return (v, next v a)

-- | Approximate the circuit as a transform of time varying function,
-- calculating the values in the integration time points and then
-- interpolating in all other time points. The resulting transform
-- computation is synchronized with the event queue.         
--
-- This procedure consumes memory as the underlying memoization allocates
-- an array to store the calculated values.
circuitTransform :: Circuit a b -> Transform a b
circuitTransform cir = Transform start
  where
    start m =
      Simulation $ \r ->
      do ref <- newIORef cir
         invokeSimulation r $
           memo0Dynamics (next ref m)
    next ref m =
      Dynamics $ \p ->
      do a <- invokeDynamics p m
         cir <- readIORef ref
         (b, cir') <-
           invokeDynamics p $
           runEvent (runCircuit cir a)
         writeIORef ref cir'
         return b
