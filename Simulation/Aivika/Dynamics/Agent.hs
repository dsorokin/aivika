
-- |
-- Module     : Simulation.Aivika.Dynamics.Agent
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module introduces basic entities for the agent-based modeling.
--
-- WARNING: the module is not well tested. This caution is related mainly to
-- managing the nested states.
-- 
-- At the same time, the timer and timeout handlers seem to be well tested as
-- they are just light-weight wrappers creating the event handlers that are
-- already processed by the event queue.
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
        stateDeactivation,
        setStateActivation,
        setStateDeactivation,
        setStateTransition) where

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
                               stateTransitRef :: IORef (Dynamics (Maybe AgentState)),
                               stateVersionRef :: IORef Int }
                  
data AgentMode = CreationMode
               | InitialMode
               | TransientMode
               | ProcessingMode
                      
instance Eq Agent where
  x == y = agentStateRef x == agentStateRef y      -- unique references
  
instance Eq AgentState where
  x == y = stateVersionRef x == stateVersionRef y  -- unique references

fullPath :: AgentState -> [AgentState] -> [AgentState]
fullPath st acc =
  case stateParent st of
    Nothing  -> st : acc
    Just st' -> fullPath st' (st : acc)

partitionPath :: [AgentState] -> [AgentState] -> ([AgentState], [AgentState])
partitionPath path1 path2 =
  case (path1, path2) of
    (h1 : t1, [h2]) | h1 == h2 -> 
      (reverse path1, path2)
    (h1 : t1, h2 : t2) | h1 == h2 -> 
      partitionPath t1 t2
    _ ->
      (reverse path1, path2)

findPath :: Maybe AgentState -> AgentState -> ([AgentState], [AgentState])
findPath Nothing target = ([], fullPath target [])
findPath (Just source) target
  | stateAgent source /= stateAgent target =
    error "Different agents: findPath."
  | otherwise =
    partitionPath path1 path2
  where
    path1 = fullPath source []
    path2 = fullPath target []

traversePath :: Maybe AgentState -> AgentState -> Dynamics ()
traversePath source target =
  let (path1, path2) = findPath source target
      agent = stateAgent target
      activate st p =
        do Dynamics m <- readIORef (stateActivateRef st)
           m p
      deactivate st p =
        do Dynamics m <- readIORef (stateDeactivateRef st)
           m p
      transit st p =
        do Dynamics m <- readIORef (stateTransitRef st)
           m p
      continue st p =
        do let Dynamics m = traversePath (Just target) st
           m p
  in Dynamics $ \p ->
       unless (null path1 && null path2) $
       do writeIORef (agentModeRef agent) TransientMode
          forM_ path1 $ \st ->
            do writeIORef (agentStateRef agent) (Just st)
               deactivate st p
               -- it makes all timeout and timer handlers outdated
               modifyIORef (stateVersionRef st) (1 +)
          forM_ path2 $ \st ->
            do when (st == target) $
                 writeIORef (agentModeRef agent) InitialMode
               writeIORef (agentStateRef agent) (Just st)
               activate st p
          writeIORef (agentModeRef agent) TransientMode     
          st' <- transit target p
          case st' of
            Nothing ->
              do writeIORef (agentModeRef agent) ProcessingMode
                 triggerAgentStateChanged p agent
            Just st' ->
              continue st' p

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
     tref <- newIORef $ return Nothing
     vref <- newIORef 0
     return AgentState { stateAgent = agent,
                         stateParent = Nothing,
                         stateActivateRef = aref,
                         stateDeactivateRef = dref,
                         stateTransitRef = tref,
                         stateVersionRef = vref }

-- | Create a child state.
newSubstate :: AgentState -> Simulation AgentState
newSubstate parent =
  Simulation $ \r ->
  do let agent = stateAgent parent 
     aref <- newIORef $ return ()
     dref <- newIORef $ return ()
     tref <- newIORef $ return Nothing
     vref <- newIORef 0
     return AgentState { stateAgent = agent,
                         stateParent = Just parent,
                         stateActivateRef= aref,
                         stateDeactivateRef = dref,
                         stateTransitRef = tref,
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
                   
-- | Select the next downmost active state. The activation is repeated while
-- there is the transition state defined by 'setStateTransition'.
activateState :: AgentState -> Dynamics ()
activateState st =
  Dynamics $ \p ->
  do let agent = stateAgent st
         Dynamics m = runQueueSync $ agentQueue agent 
     m p    -- ensure that the agent state is actual
     mode <- readIORef (agentModeRef agent)
     case mode of
       CreationMode ->
         do x0 <- readIORef (agentStateRef agent)
            let Dynamics m = traversePath x0 st
            m p
       InitialMode ->
         error $ 
         "Use the setStateTransition function to define " ++
         "the transition state: activateState."
       TransientMode ->
         error $
         "Use the setStateTransition function to define " ++
         "the transition state: activateState."
       ProcessingMode ->
         do x0 @ (Just st0) <- readIORef (agentStateRef agent)
            let Dynamics m = traversePath x0 st
            m p

{-# DEPRECATED initState "Rewrite using the setStateTransition function instead." #-}
              
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
         do x0 @ (Just st0) <- readIORef (agentStateRef agent)
            let Dynamics m = traversePath x0 st
            m p
       TransientMode -> 
         return ()
       ProcessingMode ->
         error $
         "Use the activateState function everywhere outside " ++
         "the state activation: initState."

{-# DEPRECATED stateActivation "Use the setStateActivation function instead" #-}
{-# DEPRECATED stateDeactivation "Use the setStateDeactivation function instead" #-}

-- | Set the activation computation for the specified state.
stateActivation :: AgentState -> Dynamics () -> Simulation ()
stateActivation = setStateActivation
  
-- | Set the deactivation computation for the specified state.
stateDeactivation :: AgentState -> Dynamics () -> Simulation ()
stateDeactivation = setStateDeactivation
  
-- | Set the activation computation for the specified state.
setStateActivation :: AgentState -> Dynamics () -> Simulation ()
setStateActivation st action =
  Simulation $ \r ->
  writeIORef (stateActivateRef st) action
  
-- | Set the deactivation computation for the specified state.
setStateDeactivation :: AgentState -> Dynamics () -> Simulation ()
setStateDeactivation st action =
  Simulation $ \r ->
  writeIORef (stateDeactivateRef st) action
  
-- | Set the transition state which will be next and which is used only
-- when activating the state directly with help of 'activateState'.
-- If the state was activated intermediately, when activating directly
-- another state, then this computation is not used.
setStateTransition :: AgentState -> Dynamics (Maybe AgentState) -> Simulation ()
setStateTransition st action =
  Simulation $ \r ->
  writeIORef (stateTransitRef st) action
  
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
