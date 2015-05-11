
-- Example: Inventory System with Lost Sales and Backorders 
--
-- It is described in different sources [1, 2]. So, this is chapter 11 of [2] and section 6.7 of [1].
--
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
--
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 312.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- | The time between demands for a radio.
avgRadioDemand = 0.2

-- | The percent of customers who will backorder the radio.
backorderPercent = 0.2

-- | The stock control level to be ordered up.
stockControlLevel = 72

-- | The inventory position for reordering radio.
reorderPositionThreshold = 18

-- | The initial radios in stock.
radio0 = 72 :: Int

-- | The time from the placement of an order to its receipt
leadTime = 3

-- | How often to order the radios?
reviewPeriod = 4

-- | Clear the statistics at the end of the first year
clearingTime = 52
  
model :: Simulation Results
model = do
  -- the start time
  t0 <- liftParameter starttime
  -- the inventory position
  invPos <- newRef $ returnTimingCounter t0 radio0
  -- the radios in stock
  radio <- newFCFSResource radio0
  -- the time between lost sales
  tbLostSales <- newRef emptySamplingStats
  -- the last arrive time for the lost sale
  lostSaleArrive <- newRef Nothing
  -- a customer order
  let customerOrder :: Event ()
      customerOrder = do
        do t <- liftDynamics time
           modifyRef invPos $
             decTimingCounter t 1
           runProcess $
             requestResource radio
  -- a customer has been lost
  let customerLost :: Event ()
      customerLost = do
        t0 <- readRef lostSaleArrive
        t  <- liftDynamics time
        case t0 of
          Nothing -> return ()
          Just t0 ->
            modifyRef tbLostSales $
            addSamplingStats (t - t0)
        writeRef lostSaleArrive (Just t)
  -- a customer arrival process
  let customerArrival :: Process ()
      customerArrival = do
        randomExponentialProcess_ avgRadioDemand
        liftEvent $ do
          r <- resourceCount radio
          if r > 0
            then customerOrder
            else do b <- liftParameter $
                         randomTrue backorderPercent
                    if b
                      then customerOrder
                      else customerLost
        customerArrival
  -- start the customer arrival process
  runProcessInStartTime customerArrival
  -- the safety stock
  safetyStock <- newRef emptySamplingStats
  -- an inventory review process
  let invReview :: Process ()
      invReview = do
        x <- liftEvent $ readRef invPos
        let n = timingCounterValue x
        when (n <= reorderPositionThreshold) $
          do let orderQty = stockControlLevel - n
             liftEvent $
               do t <- liftDynamics time
                  modifyRef invPos $
                    setTimingCounter t stockControlLevel
             holdProcess leadTime
             liftEvent $
               do r <- resourceCount radio
                  modifyRef safetyStock $
                    addSamplingStats r
                  incResourceCount radio orderQty
  -- start the inventory review process
  runEventInStartTime $
    enqueueEventWithTimes [t0, t0 + reviewPeriod ..] $
    runProcess invReview
  -- clear the statistics at the end of the first year
  runEventInStartTime $
    enqueueEvent clearingTime $
    do t <- liftDynamics time
       modifyRef invPos $ \x ->
         returnTimingCounter t (timingCounterValue x)
       writeRef tbLostSales emptySamplingStats
       writeRef safetyStock emptySamplingStats
  -- return the simulation results
  return $
    results
    [resultSource
     "radio" "the number of radios in stock"
     (resourceCount radio),
     --
     resultSource
     "invPos" "the inventory position"
     invPos,
     --
     resultSource
     "tbLostSales" "the time between lost sales"
     tbLostSales,
     --
     resultSource
     "safetyStock" "the safety stock"
     safetyStock]

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  (fmap resultSummary model) specs
