
-- |
-- Module     : Simulation.Aivika.Var
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines a variable that is bound up with the event queue and 
-- that keeps the history of changes storing the values in an array, which
-- allows using the variable in differential and difference equations under
-- some conditions.
--
module Simulation.Aivika.Var
       (Var,
        varChanged,
        varChanged_,
        newVar,
        readVar,
        writeVar,
        modifyVar,
        freezeVar) where

import Data.Array
import Data.Array.IO.Safe
import Data.IORef

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Signal
import Simulation.Aivika.Signal

import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.UVector as UV

-- | Like the 'Ref' reference but keeps the history of changes in 
-- different time points. The 'Var' variable is usually safe in the hybrid 
-- simulation, for example, when it can be used in the differential or
-- difference equations unless you update the variable twice in the
-- same integration time point. Only this variable is much slower than
-- the reference.
data Var a = 
  Var { varXS    :: UV.UVector Double, 
        varYS    :: V.Vector a,
        varChangedSource :: SignalSource a }
     
-- | Create a new variable bound to the specified event queue.
newVar :: a -> Simulation (Var a)
newVar a =
  Simulation $ \r ->
  do xs <- UV.newVector
     ys <- V.newVector
     UV.appendVector xs $ spcStartTime $ runSpecs r
     V.appendVector ys a
     s  <- invokeSimulation r newSignalSource
     return Var { varXS = xs,
                  varYS = ys, 
                  varChangedSource = s }

-- | Read the value of a variable, forcing the bound event queue to raise 
-- the events in case of need.
--
-- It is safe to run the resulting computation with help of the 'runEvent'
-- function using modes 'IncludingCurrentEventsOrFromPast' and
-- 'IncludingEarlierEventsOrFromPast', which is necessary if you are going
-- to use the variable in the differential or difference equations. Only
-- in the latter case it is preferrable if the variable is not updated twice
-- in the same integration time point; otherwise, different values can be returned
-- for the same point.
readVar :: Var a -> Event a
readVar v =
  Event $ \p ->
  do let xs = varXS v
         ys = varYS v
         t  = pointTime p
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if x <= t 
       then V.readVector ys i
       else do i <- UV.vectorBinarySearch xs t
               if i >= 0
                 then V.readVector ys i
                 else V.readVector ys $ - (i + 1) - 1

-- | Write a new value into the variable.
writeVar :: Var a -> a -> Event ()
writeVar v a =
  Event $ \p ->
  do let xs = varXS v
         ys = varYS v
         t  = pointTime p
         s  = varChangedSource v
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if t < x 
       then error "Cannot update the past data: writeVar."
       else if t == x
            then V.writeVector ys i $! a
            else do UV.appendVector xs t
                    V.appendVector ys $! a
     invokeEvent p $ triggerSignal s a

-- | Mutate the contents of the variable, forcing the bound event queue to
-- raise all pending events in case of need.
modifyVar :: Var a -> (a -> a) -> Event ()
modifyVar v f =
  Event $ \p ->
  do let xs = varXS v
         ys = varYS v
         t  = pointTime p
         s  = varChangedSource v
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if t < x
       then error "Cannot update the past data: modifyVar."
       else if t == x
            then do a <- V.readVector ys i
                    let b = f a
                    V.writeVector ys i $! b
                    invokeEvent p $ triggerSignal s b
            else do i <- UV.vectorBinarySearch xs t
                    if i >= 0
                      then do a <- V.readVector ys i
                              let b = f a
                              UV.appendVector xs t
                              V.appendVector ys $! b
                              invokeEvent p $ triggerSignal s b
                      else do a <- V.readVector ys $ - (i + 1) - 1
                              let b = f a
                              UV.appendVector xs t
                              V.appendVector ys $! b
                              invokeEvent p $ triggerSignal s b

-- | Freeze the variable and return in arrays the time points and corresponded 
-- values when the variable had changed in different time points: (1) the last
-- actual value per each time point is provided and (2) the time points are
-- sorted in ascending order.
--
-- If you need to get all changes including those ones that correspond to the same
-- simulation time points then you can use the 'newSignalHistory' function passing
-- in the 'varChanged' signal to it and then call function 'readSignalHistory'.
freezeVar :: Var a -> Event (Array Int Double, Array Int a)
freezeVar v =
  Event $ \p ->
  do xs <- UV.freezeVector (varXS v)
     ys <- V.freezeVector (varYS v)
     return (xs, ys)
     
-- | Return a signal that notifies about every change of the variable state.
varChanged :: Var a -> Signal a
varChanged v = publishSignal (varChangedSource v)

-- | Return a signal that notifies about every change of the variable state.
varChanged_ :: Var a -> Signal ()
varChanged_ v = mapSignal (const ()) $ varChanged v     