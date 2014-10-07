
-- |
-- Module     : Simulation.Aivika.Trans.Net
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines a 'Net' arrow that can be applied to modeling the queue networks
-- like the 'Processor' arrow from another module. Only the former has a more efficient
-- implementation of the 'Arrow' interface than the latter, although at the cost of
-- some decreasing in generality.
--
-- While the @Processor@ type is just a function that transforms the input 'Stream' into another,
-- the @Net@ type is actually an automaton that has an implementation very similar to that one
-- which the 'Circuit' type has, only the computations occur in the 'Process' monad. But unlike
-- the @Circuit@ type, the @Net@ type doesn't allow declaring recursive definitions, being based on
-- continuations.
--
-- In a nutshell, the @Net@ type is an interchangeable alternative to the @Processor@ type
-- with its weaknesses and strengths. The @Net@ arrow is useful for constructing computations
-- with help of the proc-notation to be transformed then to the @Processor@ computations that
-- are more general in nature and more easy-to-use but which computations created with help of
-- the proc-notation are not so efficient.
--
module Simulation.Aivika.Trans.Net
       (-- * Net Arrow
        Net(..),
        -- * Net Primitives
        emptyNet,
        arrNet,
        accumNet,
        -- * Specifying Identifier
        netUsingId,
        -- * Arrival Net
        arrivalNet,
        -- * Delaying Net
        delayNet,
        -- * Interchanging Nets with Processors
        netProcessor,
        processorNet) where

import qualified Control.Category as C
import Control.Arrow
import Control.Monad.Trans

import Data.IORef

import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Cont
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Stream
import Simulation.Aivika.Trans.QueueStrategy
import Simulation.Aivika.Trans.Resource
import Simulation.Aivika.Trans.Processor
import Simulation.Aivika.Trans.Ref
import Simulation.Aivika.Trans.Circuit
import Simulation.Aivika.Trans.Internal.Arrival

-- | Represents the net as an automaton working within the 'Process' computation.
newtype Net a b =
  Net { runNet :: a -> Process (b, Net a b)
        -- ^ Run the net.
      }

instance C.Category Net where

  id = Net $ \a -> return (a, C.id)

  (.) = dot
    where 
      (Net g) `dot` (Net f) =
        Net $ \a ->
        do (b, p1) <- f a
           (c, p2) <- g b
           return (c, p2 `dot` p1)

instance Arrow Net where

  arr f = Net $ \a -> return (f a, arr f)

  first (Net f) =
    Net $ \(b, d) ->
    do (c, p) <- f b
       return ((c, d), first p)

  second (Net f) =
    Net $ \(d, b) ->
    do (c, p) <- f b
       return ((d, c), second p)

  (Net f) *** (Net g) =
    Net $ \(b, b') ->
    do (c, p1) <- f b
       (c', p2) <- g b'
       return ((c, c'), p1 *** p2)
       
  (Net f) &&& (Net g) =
    Net $ \b ->
    do (c, p1) <- f b
       (c', p2) <- g b
       return ((c, c'), p1 &&& p2)

instance ArrowChoice Net where

  left x@(Net f) =
    Net $ \ebd ->
    case ebd of
      Left b ->
        do (c, p) <- f b
           return (Left c, left p)
      Right d ->
        return (Right d, left x)

  right x@(Net f) =
    Net $ \edb ->
    case edb of
      Right b ->
        do (c, p) <- f b
           return (Right c, right p)
      Left d ->
        return (Left d, right x)

  x@(Net f) +++ y@(Net g) =
    Net $ \ebb' ->
    case ebb' of
      Left b ->
        do (c, p1) <- f b
           return (Left c, p1 +++ y)
      Right b' ->
        do (c', p2) <- g b'
           return (Right c', x +++ p2)

  x@(Net f) ||| y@(Net g) =
    Net $ \ebc ->
    case ebc of
      Left b ->
        do (d, p1) <- f b
           return (d, p1 ||| y)
      Right b' ->
        do (d, p2) <- g b'
           return (d, x ||| p2)

-- | A net that never finishes its work.
emptyNet :: Net a b
emptyNet = Net $ const neverProcess

-- | Create a simple net by the specified handling function
-- that runs the discontinuous process for each input value to get an output.
arrNet :: (a -> Process b) -> Net a b
arrNet f =
  let x =
        Net $ \a ->
        do b <- f a
           return (b, x)
  in x

-- | Accumulator that outputs a value determined by the supplied function.
accumNet :: (acc -> a -> Process (acc, b)) -> acc -> Net a b
accumNet f acc =
  Net $ \a ->
  do (acc', b) <- f acc a
     return (b, accumNet f acc') 

-- | Create a net that will use the specified process identifier.
-- It can be useful to refer to the underlying 'Process' computation which
-- can be passivated, interrupted, canceled and so on. See also the
-- 'processUsingId' function for more details.
netUsingId :: ProcessId -> Net a b -> Net a b
netUsingId pid (Net f) =
  Net $ processUsingId pid . f

-- | Transform the net to an equivalent processor (a rather cheap transformation).
netProcessor :: Net a b -> Processor a b
netProcessor = Processor . loop
  where loop x as =
          Cons $
          do (a, as') <- runStream as
             (b, x') <- runNet x a
             return (b, loop x' as')

-- | Transform the processor to a similar net (a more costly transformation).
processorNet :: Processor a b -> Net a b
processorNet x =
  Net $ \a ->
  do readingA <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
     writingA <- liftSimulation $ newResourceWithMaxCount FCFS 1 (Just 1)
     readingB <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
     writingB <- liftSimulation $ newResourceWithMaxCount FCFS 1 (Just 1)
     conting  <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
     refA <- liftIO $ newIORef Nothing
     refB <- liftIO $ newIORef Nothing
     let input =
           do requestResource readingA
              Just a <- liftIO $ readIORef refA
              liftIO $ writeIORef refA Nothing
              releaseResource writingA
              return (a, Cons input)
         consume bs =
           do (b, bs') <- runStream bs
              requestResource writingB
              liftIO $ writeIORef refB (Just b)
              releaseResource readingB
              requestResource conting
              consume bs'
         loop a =
           do requestResource writingA
              liftIO $ writeIORef refA (Just a)
              releaseResource readingA
              requestResource readingB
              Just b <- liftIO $ readIORef refB
              liftIO $ writeIORef refB Nothing
              releaseResource writingB
              return (b, Net $ \a -> releaseResource conting >> loop a)
     spawnProcess CancelTogether $
       consume $ runProcessor x (Cons input)
     loop a

-- | A net that adds the information about the time points at which 
-- the values were received.
arrivalNet :: Net a (Arrival a)
arrivalNet =
  let loop t0 =
        Net $ \a ->
        do t <- liftDynamics time
           let b = Arrival { arrivalValue = a,
                             arrivalTime  = t,
                             arrivalDelay = 
                               case t0 of
                                 Nothing -> Nothing
                                 Just t0 -> Just (t - t0) }
           return (b, loop $ Just t)
  in loop Nothing

-- | Delay the input by one step using the specified initial value.
delayNet :: a -> Net a a
delayNet a0 =
  Net $ \a ->
  return (a0, delayNet a)
