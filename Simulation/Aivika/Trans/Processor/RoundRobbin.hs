
-- |
-- Module     : Simulation.Aivika.Trans.Processor.RoundRobbin
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the Round-Robbin processor.
--
module Simulation.Aivika.Trans.Processor.RoundRobbin
       (roundRobbinProcessor,
        roundRobbinProcessorUsingIds) where

import Control.Monad

import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Processor
import Simulation.Aivika.Trans.Stream
import Simulation.Aivika.Trans.Queue.Infinite

-- | Represents the Round-Robbin processor that tries to perform the task within
-- the specified timeout. If the task times out, then it is canceled and returned
-- to the processor again; otherwise, the successful result is redirected to output.
roundRobbinProcessor :: Processor (Process Double, Process a) a
roundRobbinProcessor =
  Processor $
  runProcessor roundRobbinProcessorUsingIds . mapStreamM f where
    f (timeout, p) =
      let x = do timeout' <- timeout
                 pid <- liftSimulation newProcessId
                 return (timeout', pid)
      in return (x, p)

-- | Like 'roundRobbinProcessor' but allows specifying the process identifiers which
-- must be unique for every new attemp to perform the task even if the task is the same.
roundRobbinProcessorUsingIds :: Processor (Process (Double, ProcessId), Process a) a
roundRobbinProcessorUsingIds =
  Processor $ \xs ->
  Cons $
  do q <- liftEvent newFCFSQueue
     let process =
           do t@(x, p) <- dequeue q
              (timeout, pid) <- x
              result <- timeoutProcessUsingId timeout pid p
              case result of
                Just a  -> return a
                Nothing ->
                  do liftEvent $ enqueue q t 
                     process
         processor =
           bufferProcessor
           (consumeStream $ liftEvent . enqueue q)
           (repeatProcess process)
     runStream $ runProcessor processor xs
