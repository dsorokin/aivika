
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Dynamics.UVar
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module defines a variable that is bound to the event queue and 
-- that keeps the history of changes storing the values in an unboxed array.
--
module Simulation.Aivika.Dynamics.UVar
       (UVar,
        uvarQueue,
        newUVar,
        readUVar,
        writeUVar,
        modifyUVar,
        freezeUVar) where

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.IORef

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.EventQueue

import qualified Simulation.Aivika.UVector as UV

-- | A version of the 'Var' type which uses an unboxed array to store the values 
-- in time points. You should prefer this type whenever possible.
data UVar a = 
  UVar { uvarQueue :: EventQueue, -- ^ Return the bound event queue.
         uvarRun   :: Dynamics (),
         uvarXS    :: UV.UVector Double, 
         uvarYS    :: UV.UVector a}
     
-- | Create a new variable bound to the specified event queue.
newUVar :: (MArray IOUArray a IO) => EventQueue -> a -> Simulation (UVar a)
newUVar q a =
  Simulation $ \r ->
  do xs <- UV.newVector
     ys <- UV.newVector
     UV.appendVector xs $ spcStartTime $ runSpecs r
     UV.appendVector ys a
     return UVar { uvarQueue = q,
                   uvarRun   = queueRun q,
                   uvarXS = xs,
                   uvarYS = ys }

-- | Read the value of a variable, forcing the bound event queue to raise 
-- the events in case of need.
readUVar :: (MArray IOUArray a IO) => UVar a -> Dynamics a
readUVar v =
  Dynamics $ \p ->
  do let Dynamics m = uvarRun v
     m p
     let xs = uvarXS v
         ys = uvarYS v
         t  = pointTime p
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if x <= t 
       then UV.readVector ys i
       else do i <- UV.vectorBinarySearch xs t
               if i >= 0
                 then UV.readVector ys i
                 else UV.readVector ys $ - (i + 1) - 1

-- | Write a new value into the variable.
writeUVar :: (MArray IOUArray a IO) => UVar a -> a -> Dynamics ()
writeUVar v a =
  Dynamics $ \p ->
  do let xs = uvarXS v
         ys = uvarYS v
         t  = pointTime p
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if t < x 
       then error "Cannot update the past data: writeUVar."
       else if t == x
            then UV.writeVector ys i $! a
            else do UV.appendVector xs t
                    UV.appendVector ys $! a

-- | Mutate the contents of the variable, forcing the bound event queue to
-- raise all pending events in case of need.
modifyUVar :: (MArray IOUArray a IO) => UVar a -> (a -> a) -> Dynamics ()
modifyUVar v f =
  Dynamics $ \p ->
  do let Dynamics m = uvarRun v
     m p
     let xs = uvarXS v
         ys = uvarYS v
         t  = pointTime p
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if t < x
       then error "Cannot update the past data: modifyUVar."
       else if t == x
            then do a <- UV.readVector ys i
                    UV.writeVector ys i $! f a
            else do i <- UV.vectorBinarySearch xs t
                    if i >= 0
                      then do a <- UV.readVector ys i
                              UV.appendVector xs t
                              UV.appendVector ys $! f a
                      else do a <- UV.readVector ys $ - (i + 1) - 1
                              UV.appendVector xs t
                              UV.appendVector ys $! f a

-- | Freeze the variable and return in arrays the time points and corresponded 
-- values when the variable had changed.
freezeUVar :: (MArray IOUArray a IO) => 
              UVar a -> Dynamics (Array Int Double, Array Int a)
freezeUVar v =
  Dynamics $ \p ->
  do let Dynamics m = uvarRun v
     m p
     xs <- UV.freezeVector (uvarXS v)
     ys <- UV.freezeVector (uvarYS v)
     return (xs, ys)