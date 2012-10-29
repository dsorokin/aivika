
-- |
-- Module     : Simulation.Aivika.Dynamics.Var
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module defines a variable that is bound to the event queue and 
-- that keeps the history of changes storing the values in an array.
--
module Simulation.Aivika.Dynamics.Var
       (Var,
        varQueue,
        varChanged,
        varChanged_,
        newVar,
        readVar,
        writeVar,
        modifyVar,
        freezeVar) where

import Data.Array
import Data.Array.IO
import Data.IORef

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Internal.Signal

import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.UVector as UV

-- | Like the 'Ref' reference but keeps the history of changes in 
-- different time points. The 'Var' variable is safe in the hybrid 
-- simulation and when you use different event queues, but this variable is 
-- slower than references.
data Var a = 
  Var { varQueue :: EventQueue,  -- ^ Return the bound event queue.
        varRun   :: Dynamics (),
        varXS    :: UV.UVector Double, 
        varYS    :: V.Vector a,
        varChangedSource :: SignalSource a, 
        varUpdatedSource :: SignalSource a }
     
-- | Create a new variable bound to the specified event queue.
newVar :: EventQueue -> a -> Simulation (Var a)
newVar q a =
  Simulation $ \r ->
  do xs <- UV.newVector
     ys <- V.newVector
     UV.appendVector xs $ spcStartTime $ runSpecs r
     V.appendVector ys a
     s  <- invokeSimulation r newSignalSourceUnsafe
     u  <- invokeSimulation r $ newSignalSourceWithUpdate $ runQueue q
     return Var { varQueue = q,
                  varRun   = runQueue q,
                  varXS = xs,
                  varYS = ys, 
                  varChangedSource = s, 
                  varUpdatedSource = u }

-- | Read the value of a variable, forcing the bound event queue to raise 
-- the events in case of need.
readVar :: Var a -> Dynamics a
readVar v =
  Dynamics $ \p ->
  do invokeDynamics p $ varRun v
     let xs = varXS v
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
writeVar :: Var a -> a -> Dynamics ()
writeVar v a =
  Dynamics $ \p ->
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
     invokeDynamics p $ triggerSignal s a

-- | Mutate the contents of the variable, forcing the bound event queue to
-- raise all pending events in case of need.
modifyVar :: Var a -> (a -> a) -> Dynamics ()
modifyVar v f =
  Dynamics $ \p ->
  do invokeDynamics p $ varRun v
     let xs = varXS v
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
                    invokeDynamics p $ triggerSignal s b
            else do i <- UV.vectorBinarySearch xs t
                    if i >= 0
                      then do a <- V.readVector ys i
                              let b = f a
                              UV.appendVector xs t
                              V.appendVector ys $! b
                              invokeDynamics p $ triggerSignal s b
                      else do a <- V.readVector ys $ - (i + 1) - 1
                              let b = f a
                              UV.appendVector xs t
                              V.appendVector ys $! b
                              invokeDynamics p $ triggerSignal s b

-- | Freeze the variable and return in arrays the time points and corresponded 
-- values when the variable had changed.
freezeVar :: Var a -> Dynamics (Array Int Double, Array Int a)
freezeVar v =
  Dynamics $ \p ->
  do invokeDynamics p $ varRun v
     xs <- UV.freezeVector (varXS v)
     ys <- V.freezeVector (varYS v)
     return (xs, ys)
     
-- | Return a signal that notifies about every change of the variable state.
varChanged :: Var a -> Signal a
varChanged v = merge2Signals m1 m2    -- N.B. The order is important!
  where m1 = publishSignal (varUpdatedSource v)
        m2 = publishSignal (varChangedSource v)

-- | Return a signal that notifies about every change of the variable state.
varChanged_ :: Var a -> Signal ()
varChanged_ v = mapSignal (const ()) $ varChanged v     

invokeDynamics :: Point -> Dynamics a -> IO a
{-# INLINE invokeDynamics #-}
invokeDynamics p (Dynamics m) = m p

invokeSimulation :: Run -> Simulation a -> IO a
{-# INLINE invokeSimulation #-}
invokeSimulation r (Simulation m) = m r