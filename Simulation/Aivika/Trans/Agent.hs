
-- |
-- Module     : Simulation.Aivika.Trans.Agent
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module introduces basic entities for the agent-based modeling.
--
module Simulation.Aivika.Trans.Agent
       (Agent,
        AgentState,
        newAgent,
        newState,
        newSubstate,
        selectedState,
        selectedStateChanged,
        selectedStateChanged_,
        selectState,
        stateAgent,
        stateParent,
        addTimeout,
        addTimer,
        setStateActivation,
        setStateDeactivation,
        setStateTransition) where

import Data.IORef
import Control.Monad

import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Signal

--
-- Agent-based Modeling
--

-- | Represents an agent.
data Agent = Agent { agentModeRef            :: IORef AgentMode,
                     agentStateRef           :: IORef (Maybe AgentState), 
                     agentStateChangedSource :: SignalSource (Maybe AgentState) }

-- | Represents the agent state.
data AgentState = AgentState { stateAgent         :: Agent,
                               -- ^ Return the corresponded agent.
                               stateParent        :: Maybe AgentState,
                               -- ^ Return the parent state or 'Nothing'.
                               stateActivateRef   :: IORef (Event ()),
                               stateDeactivateRef :: IORef (Event ()),
                               stateTransitRef    :: IORef (Event (Maybe AgentState)),
                               stateVersionRef    :: IORef Int }
                  
data AgentMode = CreationMode
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

traversePath :: Maybe AgentState -> AgentState -> Event ()
traversePath source target =
  let (path1, path2) = findPath source target
      agent = stateAgent target
      activate st p   = invokeEvent p =<< readIORef (stateActivateRef st)
      deactivate st p = invokeEvent p =<< readIORef (stateDeactivateRef st)
      transit st p    = invokeEvent p =<< readIORef (stateTransitRef st)
      continue st p   = invokeEvent p $ traversePath (Just target) st
  in Event $ \p ->
       unless (null path1 && null path2) $
       do writeIORef (agentModeRef agent) TransientMode
          forM_ path1 $ \st ->
            do writeIORef (agentStateRef agent) (Just st)
               deactivate st p
               -- it makes all timeout and timer handlers outdated
               modifyIORef (stateVersionRef st) (1 +)
          forM_ path2 $ \st ->
            do writeIORef (agentStateRef agent) (Just st)
               activate st p
          st' <- transit target p
          case st' of
            Nothing ->
              do writeIORef (agentModeRef agent) ProcessingMode
                 triggerAgentStateChanged p agent
            Just st' ->
              continue st' p

-- | Add to the state a timeout handler that will be actuated 
-- in the specified time period, while the state remains active.
addTimeout :: AgentState -> Double -> Event () -> Event ()
addTimeout st dt action =
  Event $ \p ->
  do v <- readIORef (stateVersionRef st)
     let m1 = Event $ \p ->
           do v' <- readIORef (stateVersionRef st)
              when (v == v') $
                invokeEvent p action
         m2 = enqueueEvent (pointTime p + dt) m1
     invokeEvent p m2

-- | Add to the state a timer handler that will be actuated
-- in the specified time period and then repeated again many times,
-- while the state remains active.
addTimer :: AgentState -> Event Double -> Event () -> Event ()
addTimer st dt action =
  Event $ \p ->
  do v <- readIORef (stateVersionRef st)
     let m1 = Event $ \p ->
           do v' <- readIORef (stateVersionRef st)
              when (v == v') $
                do invokeEvent p m2
                   invokeEvent p action
         m2 = Event $ \p ->
           do dt' <- invokeEvent p dt
              invokeEvent p $ enqueueEvent (pointTime p + dt') m1
     invokeEvent p m2

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

-- | Create an agent.
newAgent :: Simulation Agent
newAgent =
  Simulation $ \r ->
  do modeRef  <- newIORef CreationMode
     stateRef <- newIORef Nothing
     stateChangedSource <- invokeSimulation r newSignalSource
     return Agent { agentModeRef = modeRef,
                    agentStateRef = stateRef, 
                    agentStateChangedSource = stateChangedSource }

-- | Return the selected active state.
selectedState :: Agent -> Event (Maybe AgentState)
selectedState agent =
  Event $ \p -> readIORef (agentStateRef agent)
                   
-- | Select the state. The activation and selection are repeated while
-- there is the transition state defined by 'setStateTransition'.
selectState :: AgentState -> Event ()
selectState st =
  Event $ \p ->
  do let agent = stateAgent st
     mode <- readIORef (agentModeRef agent)
     case mode of
       CreationMode ->
         do x0 <- readIORef (agentStateRef agent)
            invokeEvent p $ traversePath x0 st
       TransientMode ->
         error $
         "Use the setStateTransition function to define " ++
         "the transition state: activateState."
       ProcessingMode ->
         do x0 @ (Just st0) <- readIORef (agentStateRef agent)
            invokeEvent p $ traversePath x0 st

-- | Set the activation computation for the specified state.
setStateActivation :: AgentState -> Event () -> Simulation ()
setStateActivation st action =
  Simulation $ \r ->
  writeIORef (stateActivateRef st) action
  
-- | Set the deactivation computation for the specified state.
setStateDeactivation :: AgentState -> Event () -> Simulation ()
setStateDeactivation st action =
  Simulation $ \r ->
  writeIORef (stateDeactivateRef st) action
  
-- | Set the transition state which will be next and which is used only
-- when selecting the state directly with help of 'selectState'.
-- If the state was activated intermediately, when selecting
-- another state, then this computation is not used.
setStateTransition :: AgentState -> Event (Maybe AgentState) -> Simulation ()
setStateTransition st action =
  Simulation $ \r ->
  writeIORef (stateTransitRef st) action
  
-- | Trigger the signal when the agent state changes.
triggerAgentStateChanged :: Point -> Agent -> IO ()
triggerAgentStateChanged p agent =
  do st <- readIORef (agentStateRef agent)
     invokeEvent p $ triggerSignal (agentStateChangedSource agent) st

-- | Return a signal that notifies about every change of the selected state.
selectedStateChanged :: Agent -> Signal (Maybe AgentState)
selectedStateChanged agent =
  publishSignal (agentStateChangedSource agent)

-- | Return a signal that notifies about every change of the selected state.
selectedStateChanged_ :: Agent -> Signal ()
selectedStateChanged_ agent =
  mapSignal (const ()) $ selectedStateChanged agent
