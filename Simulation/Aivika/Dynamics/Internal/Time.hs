
-- |
-- Module     : Simulation.Aivika.Dynamics.Internal.Time
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.0.3
--
-- This module defines the time parameters.
--

module Simulation.Aivika.Dynamics.Internal.Time
       (starttime, 
        stoptime, 
        dt, 
        time,
        integTimes) where

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics

-- | Return the start simulation time.
starttime :: Dynamics Double
starttime = Dynamics $ return . spcStartTime . pointSpecs

-- | Return the stop simulation time.
stoptime :: Dynamics Double
stoptime = Dynamics $ return . spcStopTime . pointSpecs

-- | Return the integration time step.
dt :: Dynamics Double
dt = Dynamics $ return . spcDT . pointSpecs

-- | Return the current simulation time.
time :: Dynamics Double
time = Dynamics $ return . pointTime 

-- | Return the integration time points.
integTimes :: Simulation [Double]
integTimes =
  Simulation $ \r ->
  do let sc  = runSpecs r
         (nl, nu) = iterationBnds sc
         t n = basicTime sc n 0
     return $ map t [nl .. nu]
