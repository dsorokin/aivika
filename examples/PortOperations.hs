
{-# LANGUAGE RecursiveDo #-}

-- Example: Port Operations
--
-- It is described in different sources [1, 2]. So, this is chapter 12 of [2] and section 6.13 of [1].
--
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006
-- 
-- A port in Africa is used to load tankers with crude oil for overwater shipment.
-- The port has facilities for loading as many as three tankers simultaneously.
-- The  tankers, which arrive at the port every 11 +/- 7 hours, are of three different
-- types. The relative frequency of the various types, and their loading time
-- requirements, are as follows:
-- 
-- Type      Relative Frequency      Loading Time, Hours
--   1              0.25                   18 +/- 2
--   2              0.55                   24 +/- 3
--   3              0.20                   36 +/- 4
-- 
-- There is one tug at the port. Tankers of all types require the services of this tug
-- to move into a berth, and later to move out of a berth. When the tug is available,
-- any berthing or de-berthing activity takes about one hour. Top priority is given to
-- the berthing activity.
-- 
-- A shipper is considering bidding on a contract to transport oil from the port to
-- the United Kingdom. He has determined that 5 tankers of a particular type would
-- have to be committed to this task to meet contract specifications. These tankers
-- would require 21 +/- 3 hours to load oil at the port. After loading and de-berthing,
-- they would travel to the United Kingdom, offload the oil, and return to the port for
-- reloading. Their round-trip travel time, including offloading, is estimated to be
-- 240 +/- hours.
-- 
-- A complicated factor is that the port experiences storms. The time between
-- the onset of storms is exponentially distributed with a mean of 48 hours and a 
-- storm lasts 4 +/- 2 hours. No tug can start an operation until a storm is over.
-- 
-- Before the port authorities can commit themselves to accommodating the
-- proposed 5 tankers, the effect of the additional port traffic on the in-port residence
-- time of the current port users must be determined. It is desired to simulate the
-- operation of the port for a one-year period (= 8640 hours) under the proposed new
-- commitment to measure in-port residence time of the proposed additional tankers,
-- as well as the three types of tankers which already use the port. All durations
-- given as ranges are uniformly distributed.        

import Prelude hiding (id, (.)) 

import Control.Monad
import Control.Monad.Trans
import Control.Arrow
import Control.Category (id, (.))

import Data.Array

import Simulation.Aivika
import Simulation.Aivika.Queue
import qualified Simulation.Aivika.Resource as R

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 8760.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

data Tunker =
  Tunker { tunkerLoadingTime :: Double,
           tunkerType :: Int }

model :: Simulation Results
model = mdo
  portTime' <- forM [1..4] $ \i ->
    newRef emptySamplingStats
  let portTime =
        array (1, 4) $ zip [1..] portTime'
  berth <-
    runEventInStartTime $
    R.newFCFSResource 3
  tug   <-
    runEventInStartTime $
    R.newFCFSResource 1
  let tunkers13 = randomUniformStream 4 18
      tunkers4  = takeStream 5 $
                  randomUniformStream 48 48
  runProcessInStartTime $
    flip consumeStream tunkers13 $ \x ->
    do p <- liftParameter $
            randomUniform 0 1
       let tp | p <= 0.25 = 1
              | p <= 0.25 + 0.55 = 2
              | otherwise = 3
       case tp of
         1 -> liftEvent arv1
         2 -> liftEvent arv2
         3 -> liftEvent arv3
  runProcessInStartTime $
    flip consumeStream tunkers4 $ \x ->
    liftEvent arv4
  let arv1 :: Event ()
      arv1 = do
        loadingTime <- liftParameter $
                       randomUniform 16 20
        let t = Tunker loadingTime 1
        runProcess (port t)
      arv2 :: Event ()
      arv2 = do
        loadingTime <- liftParameter $
                       randomUniform 21 27
        let t = Tunker loadingTime 2
        runProcess (port t)
      arv3 :: Event ()
      arv3 = do
        loadingTime <- liftParameter $
                       randomUniform 32 40
        let t = Tunker loadingTime 3
        runProcess (port t)
      arv4 :: Event ()
      arv4 = do
        loadingTime <- liftParameter $
                       randomUniform 18 24
        let t = Tunker loadingTime 4
        runProcess (port t)
  let port :: Tunker -> Process ()
      port t = do
        t0 <- liftDynamics time
        R.requestResource berth
        R.requestResource tug
        holdProcess 1
        R.releaseResource tug
        holdProcess (tunkerLoadingTime t)
        R.requestResource tug
        holdProcess 1
        R.releaseResource tug
        R.releaseResource berth
        t1 <- liftDynamics time
        let tp = tunkerType t 
        liftEvent $
          modifyRef (portTime ! tp) $
          addSamplingStats (t1 - t0)
        when (tp == 4) $
          liftEvent $
          runProcess $
          do randomUniformProcess_  216 264
             liftEvent arv4
      storm :: Process ()
      storm = do
        randomExponentialProcess_ 48
        R.decResourceCount tug 1
        randomUniformProcess_ 2 6
        liftEvent $
          R.incResourceCount tug 1
        storm
  runProcessInStartTime storm
  return $
    results
    [resultSource
     "portTime" "Port Time"
     portTime,
     --
     resultSource
     "berth" "Berth"
     berth,
     --
     resultSource
     "tug" "Tug"
     tug ]

modelSummary :: Simulation Results
modelSummary =
  fmap resultSummary model

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  -- model specs
  modelSummary specs
