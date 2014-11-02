
-- Example: Inventory System with Lost Sales and Backorders 
--
-- It is described in different sources [1, 2]. So, this is chapter 11 of [2] and section 6.7 of [1].
--
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
--
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

import Control.Monad
import Control.Monad.Trans
import Control.Category

import Data.Monoid

import Simulation.Aivika
import qualified Simulation.Aivika.Queue.Infinite as IQ

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
inventoryPositionThreshold = 18

-- | The initial stock of radios.
radioStock0 = 72 :: Int

-- | The time from the placement of an order to its receipt
procurementLeadTime = 3

-- | How often to order the radios?
procurementPeriod = 4

-- | Clear the statistics at the end of the first year
clearingTime = 52
  
model :: Simulation Results
model = do
  -- the start time
  t0 <- liftParameter starttime
  -- the radios in stock
  radioStock <- newRef $ returnTimingCounter t0 radioStock0
  -- the number of orders
  orderCount <- newRef emptyTimingCounter
  -- the queue of backorders
  backorderQueue <- runEventInStartTime $ IQ.newFCFSQueue
  -- the total number of customers
  totalCustomerCount <- newRef (0 :: Int)
  -- the total order count
  totalOrderCount <- newRef (0 :: Int)
  -- the total number of backorders
  totalBackorderCount <- newRef (0 :: Int)
  -- the number of immediate sales
  immedSalesCount <- newRef (0 :: Int)
  -- the lost sales count
  lostSalesCount <- newRef (0 :: Int)
  -- whether the procurement initiated?
  procuring <- newRef False
  -- the inventotyPosition
  let inventoryPosition = do
        x1 <- readRef radioStock
        x2 <- readRef orderCount
        x3 <- IQ.queueCount backorderQueue
        return (timingCounterValue x1 +
                timingCounterValue x2 -
                x3)
  -- implement the ordering policy of the company
  runEventInStartTime $
    enqueueEventWithTimes [t0, t0 + procurementPeriod..] $
    do c <- readRef orderCount
       when (timingCounterValue c == 0) $
         do x <- inventoryPosition
            when (x < inventoryPositionThreshold) $
              do let order = stockControlLevel - x
                 t0 <- liftDynamics time
                 modifyRef orderCount $ incTimingCounter t0 order
                 modifyRef totalOrderCount (+ order)
                 enqueueEvent (t0 + procurementLeadTime) $
                   do t <- liftDynamics time
                      modifyRef radioStock $ incTimingCounter t order
                      modifyRef orderCount $ resetTimingCounter t
                      y1 <- readRef radioStock
                      y2 <- IQ.queueCount backorderQueue
                      let dy = min (timingCounterValue y1) y2
                      modifyRef radioStock $ decTimingCounter t dy
                      forM_ [1..dy] $ \i ->
                        do IQ.tryDequeue backorderQueue
                           return ()
  -- a stream of customers
  let customers = randomExponentialStream avgRadioDemand
  -- model their behavior
  runProcessInStartTime $
    flip consumeStream customers $ \a ->
    liftEvent $
    do modifyRef totalCustomerCount (+ 1)
       t <- liftDynamics time
       x <- readRef radioStock
       if timingCounterValue x > 0
         then do modifyRef radioStock $ decTimingCounter t 1
                 modifyRef immedSalesCount (+ 1)
         else do b <- liftParameter $
                      randomTrue backorderPercent
                 if b
                   then do modifyRef totalBackorderCount (+ 1)
                           IQ.enqueue backorderQueue a
                   else modifyRef lostSalesCount (+ 1)
  -- clear the statistics at the end of the first year (??)
  runEventInStartTime $
    enqueueEvent clearingTime $
    do modifyRef radioStock $ \x -> x { timingCounterStats = emptyTimingStats }
       modifyRef orderCount $ \x -> x { timingCounterStats = emptyTimingStats }
       -- N.B. there is not yet clearing of the backorderQueue statistics
  -- return the simulation results in start time
  return $
    results
    [resultSource
     "radioStock" "the radios in stock"
     radioStock,
     --
     resultSource
     "inventoryPosition" "inventory position"
     inventoryPosition,
     --
     resultSource
     "orderCount" "the number of orders"
     orderCount,
     --
     resultSource
     "backorderQueue" "the queue of backorders"
     backorderQueue,
     --
     resultSource
     "totalCustomerCount" "the total number of customers"
     totalCustomerCount,
     --
     resultSource
     "totalOrderCount" "the total order count"
     totalOrderCount,
     --
     resultSource
     "totalBackorderCount" "the total number of backorders"
     totalBackorderCount,
     --
     resultSource
     "lostSalesCount" "the lost sales count"
     lostSalesCount,
     --
     resultSource
     "immedSalesCount" "the number of immediate sales"
     immedSalesCount]

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  (fmap resultSummary model) specs
