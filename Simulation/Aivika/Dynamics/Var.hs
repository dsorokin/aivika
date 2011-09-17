
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
        newVar,
        varQueue,
        readVar,
        writeVar,
        modifyVar) where

import Data.Array
import Data.Array.IO
import Data.IORef

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.EventQueue

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
        varYS    :: V.Vector a}
     
-- | Create a new variable bound to the specified event queue.
newVar :: EventQueue -> a -> Dynamics (Var a)
newVar q a =
  Dynamics $ \p ->
  do xs <- UV.newVector
     ys <- V.newVector
     UV.appendVector xs $ spcStartTime $ pointSpecs p
     V.appendVector ys a
     return Var { varQueue = q,
                  varRun   = queueRun q,
                  varXS = xs,
                  varYS = ys }

-- | Read the value of a variable, forcing the bound event queue to raise 
-- the events in case of need.
readVar :: Var a -> Dynamics a
readVar v =
  Dynamics $ \p ->
  do let Dynamics m = varRun v
     m p
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
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if t < x 
       then error "Cannot update the past data: writeVar."
       else if t == x
            then V.writeVector ys i $! a
            else do UV.appendVector xs t
                    V.appendVector ys $! a

-- | Mutate the contents of the variable, forcing the bound event queue to
-- raise all pending events in case of need.
modifyVar :: Var a -> (a -> a) -> Dynamics ()
modifyVar v f =
  Dynamics $ \p ->
  do let Dynamics m = varRun v
     m p
     let xs = varXS v
         ys = varYS v
         t  = pointTime p
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if t < x
       then error "Cannot update the past data: modifyVar."
       else if t == x
            then do a <- V.readVector ys i
                    V.writeVector ys i $! f a
            else do i <- UV.vectorBinarySearch xs t
                    if i >= 0
                      then do a <- V.readVector ys i
                              UV.appendVector xs t
                              V.appendVector ys $! f a
                      else do a <- V.readVector ys $ - (i + 1) - 1
                              UV.appendVector xs t
                              V.appendVector ys $! f a
