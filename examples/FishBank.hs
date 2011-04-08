
import Data.Array

import Simulation.Aivika.Dynamics

specs = Specs { spcStartTime = 0, 
                spcStopTime = 13, 
                spcDT = 0.01,
                -- spcDT = 0.000005,
                spcMethod = RungeKutta4 }

model :: Dynamics (Dynamics Double)
model =
  do fishInteg <- newInteg 1000
     shipsInteg <- newInteg 10
     totalProfitInteg <- newInteg 0
     -- integral values --
     let fish = integValue fishInteg
         ships = integValue shipsInteg
         totalProfit = integValue totalProfitInteg
     -- auxiliary values --
     let annualProfit = profit
         area = 100
         carryingCapacity = 1000
         catchPerShip = 
           lookupD density $
           listArray (1, 11) [(0.0, -0.048), (1.2, 10.875), (2.4, 17.194), 
                              (3.6, 20.548), (4.8, 22.086), (6.0, 23.344), 
                              (7.2, 23.903), (8.4, 24.462), (9.6, 24.882), 
                              (10.8, 25.301), (12.0, 25.86)]
         deathFraction = 
           lookupD (fish / carryingCapacity) $
           listArray (1, 11) [(0.0, 5.161), (0.1, 5.161), (0.2, 5.161), 
                              (0.3, 5.161), (0.4, 5.161), (0.5, 5.161), 
                              (0.6, 5.118), (0.7, 5.247), (0.8, 5.849), 
                              (0.9, 6.151), (10.0, 6.194)]
         density = fish / area
         fishDeathRate = maxD 0 (fish * deathFraction)
         fishHatchRate = maxD 0 (fish * hatchFraction)
         fishPrice = 20
         fractionInvested = 0.2
         hatchFraction = 6
         operatingCost = ships * 250
         profit = revenue - operatingCost
         revenue = totalCatchPerYear * fishPrice
         shipBuildingRate = maxD 0 (profit * fractionInvested / shipCost)
         shipCost = 300
         totalCatchPerYear = maxD 0 (ships * catchPerShip)
     -- derivatives --
     integDiff fishInteg (fishHatchRate - fishDeathRate - totalCatchPerYear)
     integDiff shipsInteg shipBuildingRate
     integDiff totalProfitInteg annualProfit
     -- results --
     return annualProfit

main = do a <- runDynamics1 model specs
          print a
