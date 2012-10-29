
-- |
-- Module     : Simulation.Aivika.Dynamics.Agent
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module introduces an agent-based modeling.
--

module Simulation.Aivika.Dynamics.Agent
       (Agent,
        AgentState,
        newAgent,
        newState,
        newSubstate,
        agentQueue,
        agentState,
        agentStateChanged,
        agentStateChanged_,
        activateState,
        initState,
        stateAgent,
        stateParent,
        addTimeout,
        addTimer,
        stateActivation,
        stateDeactivation) where

import Data.IORef
import Control.Monad

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Internal.Signal

--
-- Agent-based Modeling
--

-- | Represents an agent.
data Agent = Agent { agentQueue :: EventQueue,
                     -- ^ Return the bound event queue.
                     agentModeRef :: IORef AgentMode,
                     agentStateRef :: IORef (Maybe AgentState), 
                     agentStateChangedSource :: SignalSource (Maybe AgentState), 
                     agentStateUpdatedSource :: SignalSource (Maybe AgentState) }

-- | Represents the agent state.
data AgentState = AgentState { stateAgent :: Agent,
                               -- ^ Return the corresponded agent.
                               stateParent :: Maybe AgentState,
                               -- ^ Return the parent state or 'Nothing'.
                               stateActivateRef :: IORef (Dynamics ()),
                               stateDeactivateRef :: IORef (Dynamics ()), 
                               stateVersionRef :: IORef Int }
                  
data AgentMode = CreationMode
               | InitialMode
               | TransientMode
               | ProcessingMode
                      
instance Eq Agent where
  x == y = agentStateRef x == agentStateRef y      -- unique references
  
instance Eq AgentState where
  x == y = stateVersionRef x == stateVersionRef y  -- unique references

findPath :: AgentState -> AgentState -> ([AgentState], [AgentState])
findPath source target = 
  if stateAgent source == stateAgent target 
  then
    partitionPath path1 path2
  else
    error "Different agents: findPath."
      where
        path1 = fullPath source []
        path2 = fullPath target []
        fullPath st acc =
          case stateParent st of
            Nothing  -> st : acc
            Just st' -> fullPath st' (st : acc)
        partitionPath path1 path2 =
          case (path1, path2) of
            (h1 : t1, [h2]) | h1 == h2 -> 
              (reverse path1, path2)
            (h1 : t1, h2 : t2) | h1 == h2 -> 
              partitionPath t1 t2
            _ -> 
              (reverse path1, path2)
            
traversePath :: AgentState -> AgentState -> Dynamics ()
traversePath source target =
  let (path1, path2) = findPath source target
      agent = stateAgent source
      activate st p =
        do Dynamics m <- readIORef (stateActivateRef st)
           m p
      deactivate st p =
        do Dynamics m <- readIORef (stateDeactivateRef st)
           m p
  in Dynamics $ \p ->
       do writeIORef (agentModeRef agent) TransientMode
          forM_ path1 $ \st ->
            do writeIORef (agentStateRef agent) (Just st)
               deactivate st p
               -- it makes all timeout and timer handlers obsolete
               modifyIORef (stateVersionRef st) (1 +)
          forM_ path2 $ \st ->
            do when (st == target) $
                 writeIORef (agentModeRef agent) InitialMode
               writeIORef (agentStateRef agent) (Just st)
               activate st p
               when (st == target) $
                 writeIORef (agentModeRef agent) ProcessingMode
          unless (null path1 && null path2) $
            triggerAgentStateChanged p agent

-- | Add to the state a timeout handler that will be actuated 
-- in the specified time period, while the state remains active.
addTimeout :: AgentState -> Double -> Dynamics () -> Dynamics ()
addTimeout st dt (Dynamics action) =
  Dynamics $ \p ->
  do let q = agentQueue (stateAgent st)
         Dynamics m0 = runQueueSync q
     m0 p    -- ensure that the agent state is actual
     v <- readIORef (stateVersionRef st)
     let m1 = Dynamics $ \p ->
           do -- checkTime p (stateAgent st) "addTimeout"
              v' <- readIORef (stateVersionRef st)
              when (v == v') $ action p
         Dynamics m2 = enqueue q (pointTime p + dt) m1
     m2 p

-- | Add to the state a timer handler that will be actuated
-- in the specified time period and then repeated again many times,
-- while the state remains active.
addTimer :: AgentState -> Dynamics Double -> Dynamics () -> Dynamics ()
addTimer st (Dynamics dt) (Dynamics action) =
  Dynamics $ \p ->
  do let q = agentQueue (stateAgent st)
         Dynamics m0 = runQueueSync q
     m0 p    -- ensure that the agent state is actual
     v <- readIORef (stateVersionRef st)
     let m1 = Dynamics $ \p ->
           do -- checkTime p (stateAgent st) "addTimer"
              v' <- readIORef (stateVersionRef st)
              when (v == v') $ do { m2 p; action p }
         Dynamics m2 = 
           Dynamics $ \p ->
           do dt' <- dt p
              let Dynamics m3 = enqueue q (pointTime p + dt') m1
              m3 p
     m2 p

-- | Create a new state.
newState :: Agent -> Simulation AgentState
newState agent =
  Simulation $ \r ->
  do aref <- newIORef $ return ()
     dref <- newIORef $ return ()
     vref <- newIORef 0
     return AgentState { stateAgent = agent,
                         stateParent = Nothing,
                         stateActivateRef = aref,
                         stateDeactivateRef = dref,
                         stateVersionRef = vref }

-- | Create a child state.
newSubstate :: AgentState -> Simulation AgentState
newSubstate parent =
  Simulation $ \r ->
  do let agent = stateAgent parent 
     aref <- newIORef $ return ()
     dref <- newIORef $ return ()
     vref <- newIORef 0
     return AgentState { stateAgent = agent,
                         stateParent = Just parent,
                         stateActivateRef= aref,
                         stateDeactivateRef = dref,
                         stateVersionRef = vref }

-- | Create an agent bound with the specified event queue.
newAgent :: EventQueue -> Simulation Agent
newAgent queue =
  Simulation $ \r ->
  do modeRef    <- newIORef CreationMode
     stateRef   <- newIORef Nothing
     let Simulation m1 = newSignalSourceUnsafe
         Simulation m2 = newSignalSourceWithUpdate $ runQueue queue
     stateChangedSource <- m1 r
     stateUpdatedSource <- m2 r
     return Agent { agentQueue = queue,
                    agentModeRef = modeRef,
                    agentStateRef = stateRef, 
                    agentStateChangedSource = stateChangedSource, 
                    agentStateUpdatedSource = stateUpdatedSource }

-- | Return the selected downmost active state.
agentState :: Agent -> Dynamics (Maybe AgentState)
agentState agent =
  Dynamics $ \p -> 
  do let Dynamics m = runQueueSync $ agentQueue agent 
     m p    -- ensure that the agent state is actual
     readIORef (agentStateRef agent)
                   
-- | Select the next downmost active state.       
activateState :: AgentState -> Dynamics ()
activateState st =
  Dynamics $ \p ->
  do let agent = stateAgent st
         Dynamics m = runQueueSync $ agentQueue agent 
     m p    -- ensure that the agent state is actual
     mode <- readIORef (agentModeRef agent)
     case mode of
       CreationMode ->
         case stateParent st of
           Just _ ->
             error $ 
             "To run the agent for the first time, an initial state " ++
             "must be top-level: activateState."
           Nothing ->
             do writeIORef (agentModeRef agent) InitialMode
                writeIORef (agentStateRef agent) (Just st)
                Dynamics m <- readIORef (stateActivateRef st)
                m p
                writeIORef (agentModeRef agent) ProcessingMode
                triggerAgentStateChanged p agent
       InitialMode ->
         error $ 
         "Use the initState function during " ++
         "the state activation: activateState."
       TransientMode ->
         error $
         "Use the initState function during " ++
         "the state activation: activateState."
       ProcessingMode ->
         do Just st0 <- readIORef (agentStateRef agent)
            let Dynamics m = traversePath st0 st
            m p
              
-- | Activate the child state during the direct activation of 
-- the parent state. This call is ignored in other cases.
initState :: AgentState -> Dynamics ()
initState st =
  Dynamics $ \p ->
  do let agent = stateAgent st
         Dynamics m = runQueueSync $ agentQueue agent 
     m p    -- ensure that the agent state is actual
     mode <- readIORef (agentModeRef agent)
     case mode of
       CreationMode ->
         error $
         "To run the agent for the fist time, use " ++
         "the activateState function: initState."
       InitialMode ->
         do Just st0 <- readIORef (agentStateRef agent)
            let Dynamics m = traversePath st0 st
            m p
       TransientMode -> 
         return ()
       ProcessingMode ->
         error $
         "Use the activateState function everywhere outside " ++
         "the state activation: initState."

-- | Set the activation computation for the specified state.
stateActivation :: AgentState -> Dynamics () -> Simulation ()
stateActivation st action =
  Simulation $ \r ->
  writeIORef (stateActivateRef st) action
  
-- | Set the deactivation computation for the specified state.
stateDeactivation :: AgentState -> Dynamics () -> Simulation ()
stateDeactivation st action =
  Simulation $ \r ->
  writeIORef (stateDeactivateRef st) action
  
-- | Trigger the signal when the agent state changes.
triggerAgentStateChanged :: Point -> Agent -> IO ()
triggerAgentStateChanged p agent =
  do st <- readIORef (agentStateRef agent)
     let Dynamics m = triggerSignal (agentStateChangedSource agent) st
     m p

-- | Return a signal that notifies about every change of the state.
agentStateChanged :: Agent -> Signal (Maybe AgentState)
agentStateChanged v = merge2Signals m1 m2    -- N.B. The order is important!
  where m1 = publishSignal (agentStateUpdatedSource v)
        m2 = publishSignal (agentStateChangedSource v)

-- | Return a signal that notifies about every change of the state.
agentStateChanged_ :: Agent -> Signal ()
agentStateChanged_ agent =
  mapSignal (const ()) $ agentStateChanged agent
