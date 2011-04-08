
-- Copyright (c) 2009, 2010, 2011 David Sorokin <david.sorokin@gmail.com>
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Dynamics
-- Copyright  : Copyright (c) 2009-2011, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 6.12.1
--
-- Aivika is a multi-paradigm simulation library. It allows us to integrate 
-- a system of ordinary differential equations. Also it can be applied to
-- the Discrete Event Simulation. It supports the event-oriented, 
-- process-oriented and activity-oriented paradigms. Aivika also supports 
-- the Agent-based Modeling. Finally, it can be applied to System Dynamics.
--
module Simulation.Aivika.Dynamics 
       (-- * Dynamics
        Dynamics,
        DynamicsTrans(..),
        Specs(..),
        Method(..),
        runDynamics1,
        runDynamics,
        runDynamicsIO,
        -- ** Time parameters
        starttime,
        stoptime,
        dt,
        time,
        -- ** Maximum and Minimum
        maxD,
        minD,
        -- ** Integrals
        Integ,
        newInteg,
        integInit,
        integValue,
        integDiff,
        -- ** Table Functions
        lookupD,
        lookupStepwiseD,
        -- ** Interpolation
        initD,
        discrete,
        interpolate,
        -- ** Memoization and Sequential Calculations
        Memo,
        UMemo,
        memo,
        umemo,
        memo0,
        umemo0,
        -- ** Utility
        once,
        -- * Event Queue
        DynamicsQueue,
        newQueue,
        enqueueDC,
        enqueueD,
        runQueue,
        -- * References
        DynamicsRef,
        newRef,
        refQueue,
        readRef,
        writeRef,
        writeRef',
        modifyRef,
        modifyRef',
        -- * Discontinuous Processes
        DynamicsPID,
        DynamicsProc,
        newPID,
        pidQueue,
        holdProcD,
        holdProc,
        passivateProc,
        procPassive,
        reactivateProc,
        procPID,
        runProc,
        -- * Resources
        DynamicsResource,
        newResource,
        resourceQueue,
        resourceInitCount,
        resourceCount,
        requestResource,
        releaseResource,
        -- * Agent-based Modeling
        Agent,
        AgentState,
        newAgent,
        newState,
        newSubstate,
        agentQueue,
        agentState,
        activateState,
        initState,
        stateAgent,
        stateParent,
        addTimeoutD,
        addTimeout,
        addTimerD,
        addTimer,
        stateActivation,
        stateDeactivation) where

import Data.Array
import Data.Array.IO
import Data.IORef
import Control.Monad
import Control.Monad.Trans

import qualified Simulation.Aivika.Queue as Q
import qualified Simulation.Aivika.PriorityQueue as PQ

--
-- The Dynamics Monad
--
-- A value of the Dynamics monad represents an abstract dynamic 
-- process, i.e. a time varying polymorphic function. This is 
-- a key point of the Aivika simulation library.
--

-- | A value in the 'Dynamics' monad represents a dynamic process, i.e.
-- a polymorphic time varying function.
newtype Dynamics a = Dynamics (Parameters -> IO a)

-- | It defines the simulation time appended with additional information.
data Parameters = Parameters { parSpecs :: Specs,    -- ^ the simulation specs
                               parTime :: Double,    -- ^ the current time
                               parIteration :: Int,  -- ^ the current iteration
                               parPhase :: Int }     -- ^ the current phase

-- | It defines the simulation specs.
data Specs = Specs { spcStartTime :: Double,    -- ^ the start time
                     spcStopTime :: Double,     -- ^ the stop time
                     spcDT :: Double,           -- ^ the integration time step
                     spcMethod :: Method        -- ^ the integration method
                   } deriving (Eq, Ord, Show)

-- | It defines the integration method.
data Method = Euler          -- ^ Euler's method
            | RungeKutta2    -- ^ the 2nd order Runge-Kutta method
            | RungeKutta4    -- ^ the 4th order Runge-Kutta method
            deriving (Eq, Ord, Show)

iterations :: Specs -> [Int]
iterations sc = [i1 .. i2] where
  i1 = 0
  i2 = round ((spcStopTime sc - 
               spcStartTime sc) / spcDT sc)

iterationBnds :: Specs -> (Int, Int)
iterationBnds sc = (0, round ((spcStopTime sc - 
                               spcStartTime sc) / spcDT sc))

iterationLoBnd :: Specs -> Int
iterationLoBnd sc = 0

iterationHiBnd :: Specs -> Int
iterationHiBnd sc = round ((spcStopTime sc - 
                            spcStartTime sc) / spcDT sc)

phases :: Specs -> [Int]
phases sc = 
  case spcMethod sc of
    Euler -> [0]
    RungeKutta2 -> [0, 1]
    RungeKutta4 -> [0, 1, 2, 3]

phaseBnds :: Specs -> (Int, Int)
phaseBnds sc = 
  case spcMethod sc of
    Euler -> (0, 0)
    RungeKutta2 -> (0, 1)
    RungeKutta4 -> (0, 3)

phaseLoBnd :: Specs -> Int
phaseLoBnd sc = 0
                  
phaseHiBnd :: Specs -> Int
phaseHiBnd sc = 
  case spcMethod sc of
    Euler -> 0
    RungeKutta2 -> 1
    RungeKutta4 -> 3

basicTime :: Specs -> Int -> Int -> Double
basicTime sc n ph =
  if ph < 0 then 
    error "Incorrect phase: basicTime"
  else
    spcStartTime sc + n' * spcDT sc + delta (spcMethod sc) ph 
      where n' = fromInteger (toInteger n)
            delta Euler       0 = 0
            delta RungeKutta2 0 = 0
            delta RungeKutta2 1 = spcDT sc
            delta RungeKutta4 0 = 0
            delta RungeKutta4 1 = spcDT sc / 2
            delta RungeKutta4 2 = spcDT sc / 2
            delta RungeKutta4 3 = spcDT sc

neighborhood :: Specs -> Double -> Double -> Bool
neighborhood sc t t' = 
  abs (t - t') <= spcDT sc / 1.0e6

instance Monad Dynamics where
  return  = returnD
  m >>= k = bindD m k

returnD :: a -> Dynamics a
returnD a = Dynamics (\ps -> return a)

bindD :: Dynamics a -> (a -> Dynamics b) -> Dynamics b
bindD (Dynamics m) k = 
  Dynamics $ \ps -> 
  do a <- m ps
     let Dynamics m' = k a
     m' ps

subrunDynamics1 :: Dynamics a -> Specs -> IO a
subrunDynamics1 (Dynamics m) sc =
  do let n = iterationHiBnd sc
         t = basicTime sc n 0
     m Parameters { parSpecs = sc,
                    parTime = t,
                    parIteration = n,
                    parPhase = 0 }

subrunDynamics :: Dynamics a -> Specs -> [IO a]
subrunDynamics (Dynamics m) sc =
  do let (nl, nu) = iterationBnds sc
         parameterise n = Parameters { parSpecs = sc,
                                       parTime = basicTime sc n 0,
                                       parIteration = n,
                                       parPhase = 0 }
     map (m . parameterise) [nl .. nu]

-- | Run the simulation and return the result in the last 
-- time point using the specified simulation specs.
runDynamics1 :: Dynamics (Dynamics a) -> Specs -> IO a
runDynamics1 (Dynamics m) sc = 
  do d <- m Parameters { parSpecs = sc,
                         parTime = spcStartTime sc,
                         parIteration = 0,
                         parPhase = 0 }
     subrunDynamics1 d sc

-- | Run the simulation and return the results in all 
-- integration time points using the specified simulation specs.
runDynamics :: Dynamics (Dynamics a) -> Specs -> IO [a]
runDynamics (Dynamics m) sc = 
  do d <- m Parameters { parSpecs = sc,
                         parTime = spcStartTime sc,
                         parIteration = 0,
                         parPhase = 0 }
     sequence $ subrunDynamics d sc

-- | Run the simulation and return the results in all 
-- integration time points using the specified simulation specs.
runDynamicsIO :: Dynamics (Dynamics a) -> Specs -> IO [IO a]
runDynamicsIO (Dynamics m) sc =
  do d <- m Parameters { parSpecs = sc,
                         parTime = spcStartTime sc,
                         parIteration = 0,
                         parPhase = 0 }
     return $ subrunDynamics d sc

instance Functor Dynamics where
  fmap f (Dynamics m) = 
    Dynamics $ \ps -> do { a <- m ps; return $ f a }

instance Eq (Dynamics a) where
  x == y = error "Can't compare dynamics." 

instance Show (Dynamics a) where
  showsPrec _ x = showString "<< Dynamics >>"

liftMD :: (a -> b) -> Dynamics a -> Dynamics b
liftMD f (Dynamics x) =
  Dynamics $ \ps -> do { a <- x ps; return $ f a }

liftM2D :: (a -> b -> c) -> Dynamics a -> Dynamics b -> Dynamics c
liftM2D f (Dynamics x) (Dynamics y) =
  Dynamics $ \ps -> do { a <- x ps; b <- y ps; return $ f a b }

instance (Num a) => Num (Dynamics a) where
  x + y = liftM2D (+) x y
  x - y = liftM2D (-) x y
  x * y = liftM2D (*) x y
  negate = liftMD negate
  abs = liftMD abs
  signum = liftMD signum
  fromInteger i = return $ fromInteger i

instance (Fractional a) => Fractional (Dynamics a) where
  x / y = liftM2D (/) x y
  recip = liftMD recip
  fromRational t = return $ fromRational t

instance (Floating a) => Floating (Dynamics a) where
  pi = return pi
  exp = liftMD exp
  log = liftMD log
  sqrt = liftMD sqrt
  x ** y = liftM2D (**) x y
  sin = liftMD sin
  cos = liftMD cos
  tan = liftMD tan
  asin = liftMD asin
  acos = liftMD acos
  atan = liftMD atan
  sinh = liftMD sinh
  cosh = liftMD cosh
  tanh = liftMD tanh
  asinh = liftMD asinh
  acosh = liftMD acosh
  atanh = liftMD atanh

instance MonadIO Dynamics where
  liftIO m = Dynamics $ const m

-- | The 'DynamicsTrans' class defines a type which the 'Dynamics' 
-- computation can be lifted to.
class DynamicsTrans m where
  -- | Lift the computation.
  liftD :: Dynamics a -> m a

--
-- Integration Parameters and Time
--

-- | Return the start simulation time.
starttime :: Dynamics Double
starttime = Dynamics $ return . spcStartTime . parSpecs

-- | Return the stop simulation time.
stoptime :: Dynamics Double
stoptime = Dynamics $ return . spcStopTime . parSpecs

-- | Return the integration time step.
dt :: Dynamics Double
dt = Dynamics $ return . spcDT . parSpecs

-- | Return the current simulation time.
time :: Dynamics Double
time = Dynamics $ return . parTime 

--
-- Maximum and Minimum
--

-- | Return the maximum.
maxD :: (Ord a) => Dynamics a -> Dynamics a -> Dynamics a
maxD = liftM2D max

-- | Return the minimum.
minD :: (Ord a) => Dynamics a -> Dynamics a -> Dynamics a
minD = liftM2D min
           
--
-- System Dynamics
--

-- | The 'Integ' type represents an integral.
data Integ = Integ { integInit     :: Dynamics Double,   -- ^ The initial value.
                     integExternal :: IORef (Dynamics Double),
                     integInternal :: IORef (Dynamics Double) }

-- | Create a new integral with the specified initial value.
newInteg :: Dynamics Double -> Dynamics Integ
newInteg i = 
  do r1 <- liftIO $ newIORef $ initD i 
     r2 <- liftIO $ newIORef $ initD i 
     let integ = Integ { integInit     = i, 
                         integExternal = r1,
                         integInternal = r2 }
         z = Dynamics $ \ps -> 
           do (Dynamics m) <- readIORef (integInternal integ)
              m ps
     y <- umemo interpolate z
     liftIO $ writeIORef (integExternal integ) y
     return integ

-- | Return the integral's value.
integValue :: Integ -> Dynamics Double
integValue integ = 
  Dynamics $ \ps ->
  do (Dynamics m) <- readIORef (integExternal integ)
     m ps

-- | Set the derivative for the integral.
integDiff :: Integ -> Dynamics Double -> Dynamics ()
integDiff integ diff =
  do let z = Dynamics $ \ps ->
           do y <- readIORef (integExternal integ)
              let i = integInit integ
              case spcMethod (parSpecs ps) of
                Euler -> integEuler diff i y ps
                RungeKutta2 -> integRK2 diff i y ps
                RungeKutta4 -> integRK4 diff i y ps
     liftIO $ writeIORef (integInternal integ) z

integEuler :: Dynamics Double
             -> Dynamics Double 
             -> Dynamics Double 
             -> Parameters -> IO Double
integEuler (Dynamics f) (Dynamics i) (Dynamics y) ps = 
  case parIteration ps of
    0 -> 
      i ps
    n -> do 
      let sc  = parSpecs ps
          ty  = basicTime sc (n - 1) 0
          psy = ps { parTime = ty, parIteration = n - 1, parPhase = 0 }
      a <- y psy
      b <- f psy
      let !v = a + spcDT (parSpecs ps) * b
      return v

integRK2 :: Dynamics Double
           -> Dynamics Double
           -> Dynamics Double
           -> Parameters -> IO Double
integRK2 (Dynamics f) (Dynamics i) (Dynamics y) ps =
  case parPhase ps of
    0 -> case parIteration ps of
      0 ->
        i ps
      n -> do
        let sc = parSpecs ps
            ty = basicTime sc (n - 1) 0
            t1 = ty
            t2 = basicTime sc (n - 1) 1
            psy = ps { parTime = ty, parIteration = n - 1, parPhase = 0 }
            ps1 = psy
            ps2 = ps { parTime = t2, parIteration = n - 1, parPhase = 1 }
        vy <- y psy
        k1 <- f ps1
        k2 <- f ps2
        let !v = vy + spcDT sc / 2.0 * (k1 + k2)
        return v
    1 -> do
      let sc = parSpecs ps
          n  = parIteration ps
          ty = basicTime sc n 0
          t1 = ty
          psy = ps { parTime = ty, parIteration = n, parPhase = 0 }
          ps1 = psy
      vy <- y psy
      k1 <- f ps1
      let !v = vy + spcDT sc * k1
      return v
    _ -> 
      error "Incorrect phase: integ"

integRK4 :: Dynamics Double
           -> Dynamics Double
           -> Dynamics Double
           -> Parameters -> IO Double
integRK4 (Dynamics f) (Dynamics i) (Dynamics y) ps =
  case parPhase ps of
    0 -> case parIteration ps of
      0 -> 
        i ps
      n -> do
        let sc = parSpecs ps
            ty = basicTime sc (n - 1) 0
            t1 = ty
            t2 = basicTime sc (n - 1) 1
            t3 = basicTime sc (n - 1) 2
            t4 = basicTime sc (n - 1) 3
            psy = ps { parTime = ty, parIteration = n - 1, parPhase = 0 }
            ps1 = psy
            ps2 = ps { parTime = t2, parIteration = n - 1, parPhase = 1 }
            ps3 = ps { parTime = t3, parIteration = n - 1, parPhase = 2 }
            ps4 = ps { parTime = t4, parIteration = n - 1, parPhase = 3 }
        vy <- y psy
        k1 <- f ps1
        k2 <- f ps2
        k3 <- f ps3
        k4 <- f ps4
        let !v = vy + spcDT sc / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
        return v
    1 -> do
      let sc = parSpecs ps
          n  = parIteration ps
          ty = basicTime sc n 0
          t1 = ty
          psy = ps { parTime = ty, parIteration = n, parPhase = 0 }
          ps1 = psy
      vy <- y psy
      k1 <- f ps1
      let !v = vy + spcDT sc / 2.0 * k1
      return v
    2 -> do
      let sc = parSpecs ps
          n  = parIteration ps
          ty = basicTime sc n 0
          t2 = basicTime sc n 1
          psy = ps { parTime = ty, parIteration = n, parPhase = 0 }
          ps2 = ps { parTime = t2, parIteration = n, parPhase = 1 }
      vy <- y psy
      k2 <- f ps2
      let !v = vy + spcDT sc / 2.0 * k2
      return v
    3 -> do
      let sc = parSpecs ps
          n  = parIteration ps
          ty = basicTime sc n 0
          t3 = basicTime sc n 2
          psy = ps { parTime = ty, parIteration = n, parPhase = 0 }
          ps3 = ps { parTime = t3, parIteration = n, parPhase = 2 }
      vy <- y psy
      k3 <- f ps3
      let !v = vy + spcDT sc * k3
      return v
    _ -> 
      error "Incorrect phase: integ"

-- smoothI :: Dynamics Double -> Dynamics Double -> Dynamics Double 
--           -> Dynamics Double
-- smoothI x t i = y where
--   y = integ ((x - y) / t) i

-- smooth :: Dynamics Double -> Dynamics Double -> Dynamics Double
-- smooth x t = smoothI x t x

-- smooth3I :: Dynamics Double -> Dynamics Double -> Dynamics Double 
--            -> Dynamics Double
-- smooth3I x t i = y where
--   y  = integ ((s1 - y) / t') i
--   s1 = integ ((s0 - s1) / t') i
--   s0 = integ ((x - s0) / t') i
--   t' = t / 3.0

-- smooth3 :: Dynamics Double -> Dynamics Double -> Dynamics Double
-- smooth3 x t = smooth3I x t x

-- smoothNI :: Dynamics Double -> Dynamics Double -> Int -> Dynamics Double 
--            -> Dynamics Double
-- smoothNI x t n i = s ! n where
--   s   = array (1, n) [(k, f k) | k <- [1 .. n]]
--   f 0 = integ ((x - s ! 0) / t') i
--   f k = integ ((s ! (k - 1) - s ! k) / t') i
--   t'  = t / fromIntegral n

-- smoothN :: Dynamics Double -> Dynamics Double -> Int -> Dynamics Double
-- smoothN x t n = smoothNI x t n x

-- delay1I :: Dynamics Double -> Dynamics Double -> Dynamics Double 
--           -> Dynamics Double
-- delay1I x t i = y where
--   y = integ (x - y) (i * t) / t

-- delay1 :: Dynamics Double -> Dynamics Double -> Dynamics Double
-- delay1 x t = delay1I x t x

-- delay3I :: Dynamics Double -> Dynamics Double -> Dynamics Double 
--           -> Dynamics Double
-- delay3I x t i = y where
--   y  = integ (s1 - y) (i * t') / t'
--   s1 = integ (s0 - s1) (i * t') / t'
--   s0 = integ (x - s0) (i * t') / t'
--   t' = t / 3.0

-- delay3 :: Dynamics Double -> Dynamics Double -> Dynamics Double
-- delay3 x t = delay3I x t x

-- delayNI :: Dynamics Double -> Dynamics Double -> Int -> Dynamics Double 
--           -> Dynamics Double
-- delayNI x t n i = s ! n where
--   s   = array (1, n) [(k, f k) | k <- [1 .. n]]
--   f 0 = integ (x - s ! 0) (i * t') / t'
--   f k = integ (s ! (k - 1) - s ! k) (i * t') / t'
--   t'  = t / fromIntegral n

-- delayN :: Dynamics Double -> Dynamics Double -> Int -> Dynamics Double
-- delayN x t n = delayNI x t n x

-- forecast :: Dynamics Double -> Dynamics Double -> Dynamics Double 
--            -> Dynamics Double
-- forecast x at hz =
--   x * (1.0 + (x / smooth x at - 1.0) / at * hz)

-- trend :: Dynamics Double -> Dynamics Double -> Dynamics Double 
--         -> Dynamics Double
-- trend x at i =
--   (x / smoothI x at (x / (1.0 + i * at)) - 1.0) / at

--
-- Table Functions
--

-- | Lookup @x@ in a table of pairs @(x, y)@ using linear interpolation.
lookupD :: Dynamics Double -> Array Int (Double, Double) -> Dynamics Double
lookupD (Dynamics m) tbl =
  Dynamics (\ps -> do a <- m ps; return $ find first last a) where
    (first, last) = bounds tbl
    find left right x =
      if left > right then
        error "Incorrect index: table"
      else
        let index = (left + 1 + right) `div` 2
            x1    = fst $ tbl ! index
        in if x1 <= x then 
             let y | index < right = find index right x
                   | right == last  = snd $ tbl ! right
                   | otherwise     = 
                     let x2 = fst $ tbl ! (index + 1)
                         y1 = snd $ tbl ! index
                         y2 = snd $ tbl ! (index + 1)
                     in y1 + (y2 - y1) * (x - x1) / (x2 - x1) 
             in y
           else
             let y | left < index = find left (index - 1) x
                   | left == first = snd $ tbl ! left
                   | otherwise    = error "Incorrect index: table"
             in y

-- | Lookup @x@ in a table of pairs @(x, y)@ using stepwise function.
lookupStepwiseD :: Dynamics Double -> Array Int (Double, Double)
                  -> Dynamics Double
lookupStepwiseD (Dynamics m) tbl =
  Dynamics (\ps -> do a <- m ps; return $ find first last a) where
    (first, last) = bounds tbl
    find left right x =
      if left > right then
        error "Incorrect index: table"
      else
        let index = (left + 1 + right) `div` 2
            x1    = fst $ tbl ! index
        in if x1 <= x then 
             let y | index < right = find index right x
                   | right == last  = snd $ tbl ! right
                   | otherwise     = snd $ tbl ! right
             in y
           else
             let y | left < index = find left (index - 1) x
                   | left == first = snd $ tbl ! left
                   | otherwise    = error "Incorrect index: table"
             in y

-- --
-- -- Discrete Functions
-- --
    
-- delayTrans :: Dynamics a -> Dynamics Double -> Dynamics a 
--               -> (Dynamics a -> Dynamics a) -> Dynamics a
-- delayTrans (Dynamics x) (Dynamics d) (Dynamics i) tr = tr $ Dynamics r 
--   where
--     r ps = do 
--       let t  = parTime ps
--           sc = parSpecs ps
--           n  = parIteration ps
--       a <- d ps
--       let t' = (t - a) - spcStartTime sc
--           n' = fromInteger $ toInteger $ floor $ t' / spcDT sc
--           y | n' < 0    = i $ ps { parTime = spcStartTime sc,
--                                    parIteration = 0, 
--                                    parPhase = 0 }
--             | n' < n    = x $ ps { parTime = t',
--                                    parIteration = n',
--                                    parPhase = -1 }
--             | n' > n    = error "Cannot return the future data: delay"
--             | otherwise = error "Cannot return the current data: delay"
--       y    

-- delay :: (Memo a) => Dynamics a -> Dynamics Double -> Dynamics a
-- delay x d = delayTrans x d x $ memo0 discrete

-- delay' :: (UMemo a) => Dynamics a -> Dynamics Double -> Dynamics a
-- delay' x d = delayTrans x d x $ memo0' discrete

-- delayI :: (Memo a) => Dynamics a -> Dynamics Double -> Dynamics a -> Dynamics a
-- delayI x d i = delayTrans x d i $ memo0 discrete         

-- delayI' :: (UMemo a) => Dynamics a -> Dynamics Double -> Dynamics a -> Dynamics a
-- delayI' x d i = delayTrans x d i $ memo0' discrete         

--
-- Interpolation and Initial Value
--
-- These functions complement the memoization, possibly except for 
-- the initial function which can be also useful to get an initial 
-- value of any dynamic process. See comments to the Memoization 
-- section.
--      

-- | Return the initial value.
initD :: Dynamics a -> Dynamics a
initD (Dynamics m) =
  Dynamics $ \ps ->
  if parIteration ps == 0 && parPhase ps == 0 then
    m ps
  else
    let sc = parSpecs ps
    in m $ ps { parTime = basicTime sc 0 0,
                parIteration = 0,
                parPhase = 0 } 

-- | Discretize the computation in the integration time points.
discrete :: Dynamics a -> Dynamics a
discrete (Dynamics m) =
  Dynamics $ \ps ->
  let ph = parPhase ps
      r | ph == 0    = m ps
        | ph > 0    = let sc = parSpecs ps
                          n  = parIteration ps
                      in m $ ps { parTime = basicTime sc n 0,
                                  parPhase = 0 }
        | otherwise = let sc = parSpecs ps
                          t  = parTime ps
                          n  = parIteration ps
                          t' = spcStartTime sc + fromIntegral (n + 1) * spcDT sc
                          n' = if neighborhood sc t t' then n + 1 else n
                      in m $ ps { parTime = basicTime sc n' 0,
                                  parIteration = n',
                                  parPhase = 0 }
  in r

-- | Interpolate the computation based on the integration time points only.
interpolate :: Dynamics Double -> Dynamics Double
interpolate (Dynamics m) = 
  Dynamics $ \ps -> 
  if parPhase ps >= 0 then 
    m ps
  else 
    let sc = parSpecs ps
        t  = parTime ps
        x  = (t - spcStartTime sc) / spcDT sc
        n1 = max (floor x) (iterationLoBnd sc)
        n2 = min (ceiling x) (iterationHiBnd sc)
        t1 = basicTime sc n1 0
        t2 = basicTime sc n2 0
        z1 = m $ ps { parTime = t1, 
                      parIteration = n1, 
                      parPhase = 0 }
        z2 = m $ ps { parTime = t2,
                      parIteration = n2,
                      parPhase = 0 }
        r | t == t1   = z1
          | t == t2   = z2
          | otherwise = 
            do y1 <- z1
               y2 <- z2
               return $ y1 + (y2 - y1) * (t - t1) / (t2 - t1)
    in r

-- --
-- -- Memoization
-- --
-- -- The memoization creates such processes, which values are 
-- -- defined and then stored in the cache for the points of
-- -- integration. You should use some kind of interpolation 
-- -- like the interpolate function to process all other time 
-- -- points that don't coincide with the integration points:
-- --
-- --   x = memo interpolate y    -- a linear interpolation
-- --   x = memo discrete y       -- a discrete process
-- --

-- | The 'Memo' class specifies a type for which an array can be created.
class (MArray IOArray e IO) => Memo e where
  newMemoArray_ :: Ix i => (i, i) -> IO (IOArray i e)

-- | The 'UMemo' class specifies a type for which an unboxed array exists.
class (MArray IOUArray e IO) => UMemo e where
  newMemoUArray_ :: Ix i => (i, i) -> IO (IOUArray i e)

instance Memo e where
  newMemoArray_ = newArray_
    
instance (MArray IOUArray e IO) => UMemo e where
  newMemoUArray_ = newArray_

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and being aware of the Runge-Kutta method.
memo :: Memo e => (Dynamics e -> Dynamics e) -> Dynamics e 
       -> Dynamics (Dynamics e)
memo tr (Dynamics m) = 
  Dynamics $ \ps ->
  do let sc = parSpecs ps
         (phl, phu) = phaseBnds sc
         (nl, nu)   = iterationBnds sc
     arr   <- newMemoArray_ ((phl, nl), (phu, nu))
     nref  <- newIORef 0
     phref <- newIORef 0
     let r ps = 
           do let sc  = parSpecs ps
                  n   = parIteration ps
                  ph  = parPhase ps
                  phu = phaseHiBnd sc 
                  loop n' ph' = 
                    if (n' > n) || ((n' == n) && (ph' > ph)) 
                    then 
                      readArray arr (ph, n)
                    else 
                      let ps' = ps { parIteration = n', parPhase = ph',
                                     parTime = basicTime sc n' ph' }
                      in do a <- m ps'
                            a `seq` writeArray arr (ph', n') a
                            if ph' >= phu 
                              then do writeIORef phref 0
                                      writeIORef nref (n' + 1)
                                      loop (n' + 1) 0
                              else do writeIORef phref (ph' + 1)
                                      loop n' (ph' + 1)
              n'  <- readIORef nref
              ph' <- readIORef phref
              loop n' ph'
     return $ tr $ Dynamics r

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and being aware of the Runge-Kutta method.
umemo :: UMemo e => (Dynamics e -> Dynamics e) -> Dynamics e 
        -> Dynamics (Dynamics e)
umemo tr (Dynamics m) = 
  Dynamics $ \ps ->
  do let sc = parSpecs ps
         (phl, phu) = phaseBnds sc
         (nl, nu)   = iterationBnds sc
     arr   <- newMemoUArray_ ((phl, nl), (phu, nu))
     nref  <- newIORef 0
     phref <- newIORef 0
     let r ps =
           do let sc  = parSpecs ps
                  n   = parIteration ps
                  ph  = parPhase ps
                  phu = phaseHiBnd sc 
                  loop n' ph' = 
                    if (n' > n) || ((n' == n) && (ph' > ph)) 
                    then 
                      readArray arr (ph, n)
                    else 
                      let ps' = ps { parIteration = n', 
                                     parPhase = ph',
                                     parTime = basicTime sc n' ph' }
                      in do a <- m ps'
                            a `seq` writeArray arr (ph', n') a
                            if ph' >= phu 
                              then do writeIORef phref 0
                                      writeIORef nref (n' + 1)
                                      loop (n' + 1) 0
                              else do writeIORef phref (ph' + 1)
                                      loop n' (ph' + 1)
              n'  <- readIORef nref
              ph' <- readIORef phref
              loop n' ph'
     return $ tr $ Dynamics r

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and without knowledge of the Runge-Kutta method.
memo0 :: Memo e => (Dynamics e -> Dynamics e) -> Dynamics e 
        -> Dynamics (Dynamics e)
memo0 tr (Dynamics m) = 
  Dynamics $ \ps ->
  do let sc   = parSpecs ps
         bnds = iterationBnds sc
     arr   <- newMemoArray_ bnds
     nref  <- newIORef 0
     let r ps =
           do let sc = parSpecs ps
                  n  = parIteration ps
                  loop n' = 
                    if n' > n
                    then 
                      readArray arr n
                    else 
                      let ps' = ps { parIteration = n', parPhase = 0,
                                     parTime = basicTime sc n' 0 }
                      in do a <- m ps'
                            a `seq` writeArray arr n' a
                            writeIORef nref (n' + 1)
                            loop (n' + 1)
              n' <- readIORef nref
              loop n'
     return $ tr $ Dynamics r

-- | Memoize and order the computation in the integration time points using 
-- the specified interpolation and without knowledge of the Runge-Kutta method.
umemo0 :: UMemo e => (Dynamics e -> Dynamics e) -> Dynamics e 
         -> Dynamics (Dynamics e)
umemo0 tr (Dynamics m) = 
  Dynamics $ \ps ->
  do let sc   = parSpecs ps
         bnds = iterationBnds sc
     arr   <- newMemoUArray_ bnds
     nref  <- newIORef 0
     let r ps =
           do let sc = parSpecs ps
                  n  = parIteration ps
                  loop n' = 
                    if n' > n
                    then 
                      readArray arr n
                    else 
                      let ps' = ps { parIteration = n', parPhase = 0,
                                     parTime = basicTime sc n' 0 }
                      in do a <- m ps'
                            a `seq` writeArray arr n' a
                            writeIORef nref (n' + 1)
                            loop (n' + 1)
              n' <- readIORef nref
              loop n'
     return $ tr $ Dynamics r

--
-- Once
--

-- | Call the computation only once.
once :: Dynamics a -> Dynamics (Dynamics a)
once (Dynamics m) =
  Dynamics $ \ps ->
  do x <- newIORef Nothing
     let r ps =
           do a <- readIORef x
              case a of
                Just b -> 
                  return b
                Nothing ->
                  do b <- m ps
                     writeIORef x $ Just b
                     return $! b
     return $ Dynamics r

--
-- The DynamicsCont Monad
--
-- It looks somewhere like the ContT monad transformer parameterized by 
-- the Dynamics monad, although this analogy is not strong. The main 
-- idea is to represent the continuation as a dynamic process varying 
-- in time.
--

newtype DynamicsCont a = DynamicsCont (Dynamics (a -> IO ()) -> Dynamics ())

instance Monad DynamicsCont where
  return  = returnDC
  m >>= k = bindDC m k

returnDC :: a -> DynamicsCont a
returnDC a = 
  DynamicsCont $ \(Dynamics c) -> 
  Dynamics $ \ps -> 
  do cont' <- c ps
     cont' a
                          
bindDC :: DynamicsCont a -> (a -> DynamicsCont b) -> DynamicsCont b
bindDC (DynamicsCont m) k =
  DynamicsCont $ \c ->
  m $ Dynamics $ \ps -> 
  let cont' a = let (DynamicsCont m') = k a
                    (Dynamics u) = m' c
                in u ps
  in return cont'

runCont :: DynamicsCont a -> IO (a -> IO ()) -> Dynamics ()
runCont (DynamicsCont m) f = m $ Dynamics $ const f

instance DynamicsTrans DynamicsCont where
  liftD (Dynamics m) =
    DynamicsCont $ \(Dynamics c) ->
    Dynamics $ \ps ->
    do cont' <- c ps
       a <- m ps
       cont' a

instance Functor DynamicsCont where
  fmap = liftM

instance MonadIO DynamicsCont where
  liftIO m =
    DynamicsCont $ \(Dynamics c) ->
    Dynamics $ \ps ->
    do cont' <- c ps
       a <- m
       cont' a

--
-- The Event Queue (The Dynamics Queue)
--
-- The most exciting thing is that any event is just some value in 
-- the Dynamics monad, i.e. a computation, or saying differently, 
-- a dynamic process that has a single purpose to perform some 
-- side effect. To pass the message, we actually use a closure.
--

-- | The 'DynamicsQueue' type represents the event queue.
data DynamicsQueue = DynamicsQueue { 
  queuePQ   :: PQ.PriorityQueue (Dynamics (() -> IO ())),
  queueBusy :: IORef Bool,
  queueTime :: IORef Double, 
  queueRun  :: Dynamics () }

-- | Create a new event queue.
newQueue :: Dynamics DynamicsQueue
newQueue = 
  Dynamics $ \ps ->
  do let sc = parSpecs ps
     f <- newIORef False
     t <- newIORef $ spcStartTime sc
     let cont () = return ()
     pq <- PQ.newQueue $ return cont
     let q = DynamicsQueue { queuePQ   = pq,
                             queueBusy = f,
                             queueTime = t, 
                             queueRun  = subrunQueue q }
     return q
             
-- | Enqueue the event which must be actuated at the specified time.
enqueueDC :: DynamicsQueue -> Dynamics Double -> Dynamics (() -> IO ()) 
            -> Dynamics ()
enqueueDC q (Dynamics t) c = Dynamics r where
  r ps =
    do t' <- t ps
       let pq = queuePQ q
       PQ.enqueue pq t' c
    
-- | Enqueue the event which must be actuated at the specified time.
enqueueD :: DynamicsQueue -> Dynamics Double -> Dynamics () -> Dynamics ()
enqueueD q t (Dynamics m) = enqueueDC q t (Dynamics c) where
  c ps = let f () = m ps in return f
    
subrunQueue :: DynamicsQueue -> Dynamics ()
subrunQueue q = Dynamics r where
  r ps =
    do let f = queueBusy q
       f' <- readIORef f
       unless f' $
         do writeIORef f True
            call q ps
            writeIORef f False
  call q ps =
    do let pq = queuePQ q
       f <- PQ.queueNull pq
       unless f $
         do (t2, Dynamics c2) <- PQ.queueFront pq
            let t = queueTime q
            t' <- readIORef t
            when (t2 < t') $ 
              error "The time value is too small: subrunQueue"
            when (t2 <= parTime ps) $
              do writeIORef t t2
                 PQ.dequeue pq
                 let sc  = parSpecs ps
                     t0  = spcStartTime sc
                     dt  = spcDT sc
                     n2  = fromInteger $ toInteger $ floor ((t2 - t0) / dt)
                 k <- c2 $ ps { parTime = t2,
                               parIteration = n2,
                               parPhase = -1 }
                 k ()    -- raise the event
                 call q ps

-- | Run the event queue processing its events.
runQueue :: DynamicsQueue -> Dynamics ()
runQueue = queueRun

--
-- DynamicsPID and DynamicsProc
--
-- A value in the DynamicsProc monad represents a control process that can be 
-- suspended and resumed at any time. It behaves like a dynamic process too. 
-- Any value in the Dynamics monad can be lifted to the DynamicsProc monad. 
-- Moreover, a value in the DynamicsProc monad can be run in the Dynamics monad.
--
-- A value of the DynamicsPID type is just an identifier of such a process.
--

-- Public functions:
--
--   pidQueue
--   holdProcD
--   holdProc
--   passivateProc
--   procPassive
--   reactivateProc
--   runProc
--   procPID
--   newPID

-- | Represents a process handler, its PID.
data DynamicsPID = 
  DynamicsPID { pidQueue   :: DynamicsQueue,  -- ^ Return the bound event queue.
                pidStarted :: IORef Bool,
                pidCont    :: IORef (Maybe (Dynamics (() -> IO ()))) }

-- | Specifies a discontinuous process that can be suspended at any time
-- and then resumed later.
newtype DynamicsProc a = DynamicsProc (DynamicsPID -> DynamicsCont a)

-- | Hold the process for the specified time period.
holdProcD :: Dynamics Double -> DynamicsProc ()
holdProcD t =
  DynamicsProc $ \pid ->
  DynamicsCont $ \c ->
  enqueueDC (pidQueue pid) (t + time) c

-- | Hold the process for the specified time period.
holdProc :: Double -> DynamicsProc ()
holdProc t = holdProcD $ return t

-- | Passivate the process.
passivateProc :: DynamicsProc ()
passivateProc =
  DynamicsProc $ \pid ->
  DynamicsCont $ \c ->
  Dynamics $ \ps ->
  do let x = pidCont pid
     a <- readIORef x
     case a of
       Nothing -> writeIORef x $ Just c
       Just _  -> error "Cannot passivate the process twice: passivate"

-- | Test whether the process with the specified PID is passivated.
procPassive :: DynamicsPID -> DynamicsProc Bool
procPassive pid =
  DynamicsProc $ \_ ->
  DynamicsCont $ \(Dynamics c) ->
  Dynamics $ \ps ->
  do cont' <- c ps 
     let x = pidCont pid
     a <- readIORef x
     case a of
       Nothing -> cont' False
       Just _  -> cont' True

-- | Reactivate a process with the specified PID.
reactivateProc :: DynamicsPID -> DynamicsProc ()
reactivateProc pid =
  DynamicsProc $ \pid' ->
  DynamicsCont $ \c@(Dynamics cont) ->
  Dynamics $ \ps ->
  do let x = pidCont pid
     a <- readIORef x
     case a of
       Nothing ->
         do cont' <- cont ps
            cont' ()
       Just (Dynamics cont2) ->
         do writeIORef x Nothing
            let Dynamics m = enqueueDC (pidQueue pid') time c
            m ps
            cont2' <- cont2 ps
            cont2' ()

-- | Start the process with the specified PID at the desired time.
runProc :: DynamicsProc () -> DynamicsPID -> Dynamics Double -> Dynamics ()
runProc (DynamicsProc p) pid t =
  runCont m r
    where m = do y <- liftIO $ readIORef (pidStarted pid)
                 if y 
                   then error $
                        "A process with such PID " ++
                        "has been started already: runProc"
                   else liftIO $ writeIORef (pidStarted pid) True
                 DynamicsCont $ \c -> enqueueDC (pidQueue pid) t c
                 p pid
          r = let f () = return () in return f

-- | Return the current process PID.
procPID :: DynamicsProc DynamicsPID
procPID = DynamicsProc $ \pid -> return pid

-- | Create a new process PID.
newPID :: DynamicsQueue -> Dynamics DynamicsPID
newPID q =
  do x <- liftIO $ newIORef Nothing
     y <- liftIO $ newIORef False
     return DynamicsPID { pidQueue   = q,
                          pidStarted = y,
                          pidCont    = x }

instance Eq DynamicsPID where
  x == y = pidCont x == pidCont y    -- for the references are unique

instance Monad DynamicsProc where
  return  = returnDP
  m >>= k = bindDP m k

returnDP :: a -> DynamicsProc a
returnDP a = DynamicsProc (\pid -> return a)

bindDP :: DynamicsProc a -> (a -> DynamicsProc b) -> DynamicsProc b
bindDP (DynamicsProc m) k = 
  DynamicsProc $ \pid -> 
  do a <- m pid
     let DynamicsProc m' = k a
     m' pid

instance Functor DynamicsProc where
  fmap = liftM

instance DynamicsTrans DynamicsProc where
  liftD m = DynamicsProc $ \pid -> liftD m
  
instance MonadIO DynamicsProc where
  liftIO m = DynamicsProc $ \pid -> liftIO m

--
-- DynamicsResource
--
  
-- Public functions:  
--
--   resourceQueue
--   resourceInitCount
--   resourceCount
--   requestResource
--   releaseResource
--   newResource
  
-- | Represents a limited resource.
data DynamicsResource = 
  DynamicsResource { resourceQueue     :: DynamicsQueue,  
                     -- ^ Return the bound event queue.
                     resourceInitCount :: Int,
                     -- ^ Return the initial count of the resource.
                     resourceCountRef  :: IORef Int, 
                     resourceWaitQueue :: Q.Queue (Dynamics (() -> IO ()))}

instance Eq DynamicsResource where
  x == y = resourceCountRef x == resourceCountRef y  -- unique references

-- | Create a new resource with the specified initial count.
newResource :: DynamicsQueue -> Int -> Dynamics DynamicsResource
newResource q initCount =
  Dynamics $ \ps ->
  do countRef  <- newIORef initCount
     waitQueue <- Q.newQueue
     return DynamicsResource { resourceQueue     = q,
                               resourceInitCount = initCount,
                               resourceCountRef  = countRef,
                               resourceWaitQueue = waitQueue }

-- | Return the current count of the resource.
resourceCount :: DynamicsResource -> DynamicsProc Int
resourceCount r =
  DynamicsProc $ \_ ->
  DynamicsCont $ \(Dynamics c) ->
  Dynamics $ \ps ->
  do cont' <- c ps 
     a <- readIORef (resourceCountRef r)
     cont' a

-- | Request for the resource decreasing its count in case of success,
-- otherwise suspending the discontinuous process until some other 
-- process releases the resource.
requestResource :: DynamicsResource -> DynamicsProc ()
requestResource r =
  DynamicsProc $ \_ ->
  DynamicsCont $ \c@(Dynamics cont) ->
  Dynamics $ \ps ->
  do a <- readIORef (resourceCountRef r)
     if a == 0 
       then Q.enqueue (resourceWaitQueue r) c
       else do let a' = a - 1
               a' `seq` writeIORef (resourceCountRef r) a'
               cont' <- cont ps
               cont' ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended processes as possible.
releaseResource :: DynamicsResource -> DynamicsProc ()
releaseResource r =
  DynamicsProc $ \_ ->
  DynamicsCont $ \(Dynamics c) ->
  Dynamics $ \ps ->
  do a <- readIORef (resourceCountRef r)
     let a' = a + 1
     when (a' > resourceInitCount r) $
       error $
       "The resource count cannot be greater than " ++
       "its initial value: releaseResource."
     f <- Q.queueNull (resourceWaitQueue r)
     if f 
       then a' `seq` writeIORef (resourceCountRef r) a'
       else do c2 <- Q.queueFront (resourceWaitQueue r)
               Q.dequeue (resourceWaitQueue r)
               let Dynamics m = enqueueDC (resourceQueue r) time c2
               m ps
     cont' <- c ps
     cont' ()

--
-- DynamicsRef
--

-- | The 'DynamicsRef' type represents a mutable variable similar to 
-- the 'IORef' variable but only bound to some event queue, which makes 
-- the variable coordinated with that queue.
data DynamicsRef a = 
  DynamicsRef { refQueue  :: DynamicsQueue,    -- ^ Return the bound event queue.
                refRunner :: Dynamics (),
                refValue  :: IORef a }

-- | Create a new reference bound to the specified event queue.
newRef :: DynamicsQueue -> a -> Dynamics (DynamicsRef a)
newRef q a =
  do x <- liftIO $ newIORef a
     return DynamicsRef { refQueue  = q,
                          refRunner = runQueue q,
                          refValue  = x }
     
-- | Read the value of a reference, forcing the bound event queue to raise 
-- the events in case of need.
readRef :: DynamicsRef a -> Dynamics a
readRef r = Dynamics $ \ps -> 
  do let Dynamics m = refRunner r
     m ps
     readIORef (refValue r)

-- | Write a new value into the reference.
writeRef :: DynamicsRef a -> a -> Dynamics ()
writeRef r a = Dynamics $ \ps -> 
  do writeIORef (refValue r) a
     let Dynamics m = refRunner r 
     m ps

-- | Mutate the contents of the reference, forcing the bound event queue to
-- raise all pending events in case of need.
modifyRef :: DynamicsRef a -> (a -> a) -> Dynamics ()
modifyRef r f = Dynamics $ \ps -> 
  do let Dynamics m = refRunner r 
     m ps
     modifyIORef (refValue r) f

-- | A strict version of the 'writeRef' function.
writeRef' :: DynamicsRef a -> a -> Dynamics ()
writeRef' r a = a `seq` writeRef r a

-- | A strict version of the 'modifyRef' function.
modifyRef' :: DynamicsRef a -> (a -> a) -> Dynamics ()
modifyRef' r f = Dynamics $ \ps ->
  do let Dynamics m = refRunner r
     m ps
     a <- readIORef (refValue r)
     let b = f a
     b `seq` writeIORef (refValue r) b

--
-- Agent-based Modeling
--

-- Public functions:
--
--   agentQueue
--   agentState
--   activateState
--   initState
--   stateAgent     
--   stateParent
--   addTimeoutD
--   addTimeout
--   addTimerD
--   addTimer
--   stateActivation
--   stateDeactivation
--   newState     
--   newSubstate
--   newAgent

-- | Represents an agent.
data Agent = Agent { agentQueue :: DynamicsQueue,
                     -- ^ Return the bound event queue.
                     agentModeRef :: IORef AgentMode,
                     agentStateRef :: IORef (Maybe AgentState) }

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
      activate st ps   =
        do Dynamics m <- readIORef (stateActivateRef st)
           m ps
      deactivate st ps =
        do Dynamics m <- readIORef (stateDeactivateRef st)
           m ps
  in Dynamics $ \ps ->
       do writeIORef (agentModeRef agent) TransientMode
          forM_ path1 $ \st ->
            do writeIORef (agentStateRef agent) (Just st)
               deactivate st ps
               -- it makes all timeout and timer handlers obsolete
               modifyIORef (stateVersionRef st) (1 +)
          forM_ path2 $ \st ->
            do when (st == target) $
                 writeIORef (agentModeRef agent) InitialMode
               writeIORef (agentStateRef agent) (Just st)
               activate st ps
               when (st == target) $
                 writeIORef (agentModeRef agent) ProcessingMode

-- | Add to the state a timeout handler that will be actuated 
-- in the specified time period, while the state remains active.
addTimeoutD :: AgentState -> Dynamics Double -> Dynamics () -> Dynamics ()
addTimeoutD st t (Dynamics action) =
  Dynamics $ \ps ->
  do v <- readIORef (stateVersionRef st)
     let m1 = Dynamics $ \ps ->
           do v' <- readIORef (stateVersionRef st)
              when (v == v') $ action ps
         q = agentQueue (stateAgent st)
         Dynamics m2 = enqueueD q (t + time) m1
     m2 ps

-- | Add to the state a timer handler that will be actuated
-- in the specified time period and then repeated again many times,
-- while the state remains active.
addTimerD :: AgentState -> Dynamics Double -> Dynamics () -> Dynamics ()
addTimerD st t (Dynamics action) =
  Dynamics $ \ps ->
  do v <- readIORef (stateVersionRef st)
     let m1 = Dynamics $ \ps ->
           do v' <- readIORef (stateVersionRef st)
              when (v == v') $ do { m2 ps; action ps }
         q = agentQueue (stateAgent st)
         Dynamics m2 = enqueueD q (t + time) m1
     m2 ps

-- | Add to the state a timeout handler that will be actuated 
-- in the specified time period, while the state remains active.
addTimeout :: AgentState -> Double -> Dynamics () -> Dynamics ()
addTimeout st t = addTimeoutD st (return t)

-- | Add to the state a timer handler that will be actuated
-- in the specified time period and then repeated again many times,
-- while the state remains active.
addTimer :: AgentState -> Double -> Dynamics () -> Dynamics ()
addTimer st t = addTimerD st (return t)

-- | Create a new state.
newState :: Agent -> Dynamics AgentState
newState agent =
  Dynamics $ \ps ->
  do aref <- newIORef $ return ()
     dref <- newIORef $ return ()
     vref <- newIORef 0
     return AgentState { stateAgent = agent,
                         stateParent = Nothing,
                         stateActivateRef = aref,
                         stateDeactivateRef = dref,
                         stateVersionRef = vref }

-- | Create a child state.
newSubstate :: AgentState -> Dynamics AgentState
newSubstate parent =
  Dynamics $ \ps ->
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
newAgent :: DynamicsQueue -> Dynamics Agent
newAgent queue =
  Dynamics $ \ps ->
  do modeRef    <- newIORef CreationMode
     stateRef   <- newIORef Nothing
     return Agent { agentQueue = queue,
                    agentModeRef = modeRef,
                    agentStateRef = stateRef }

-- | Return the selected downmost active state.
agentState :: Agent -> Dynamics (Maybe AgentState)
agentState agent =
  Dynamics $ \ps -> 
  do let Dynamics m = queueRun $ agentQueue agent 
     m ps    -- ensure that the agent state is actual
     readIORef (agentStateRef agent)
                   
-- | Select the next downmost active state.       
activateState :: AgentState -> Dynamics ()
activateState st =
  Dynamics $ \ps ->
  do let agent = stateAgent st
         Dynamics m = queueRun $ agentQueue agent 
     m ps    -- ensure that the agent state is actual
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
                m ps
                writeIORef (agentModeRef agent) ProcessingMode
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
            m ps
              
-- | Activate the child state during the direct activation of 
-- the parent state. This call is ignored in other cases.
initState :: AgentState -> Dynamics ()
initState st =
  Dynamics $ \ps ->
  do let agent = stateAgent st
         Dynamics m = queueRun $ agentQueue agent 
     m ps    -- ensure that the agent state is actual
     mode <- readIORef (agentModeRef agent)
     case mode of
       CreationMode ->
         error $
         "To run the agent for the fist time, use " ++
         "the activateState function: initState."
       InitialMode ->
         do Just st0 <- readIORef (agentStateRef agent)
            let Dynamics m = traversePath st0 st
            m ps
       TransientMode -> 
         return ()
       ProcessingMode ->
         error $
         "Use the activateState function everywhere outside " ++
         "the state activation: initState."

-- | Set the activation computation for the specified state.
stateActivation :: AgentState -> Dynamics () -> Dynamics ()
stateActivation st action =
  Dynamics $ \ps ->
  writeIORef (stateActivateRef st) action
  
-- | Set the deactivation computation for the specified state.
stateDeactivation :: AgentState -> Dynamics () -> Dynamics ()
stateDeactivation st action =
  Dynamics $ \ps ->
  writeIORef (stateDeactivateRef st) action
  
          