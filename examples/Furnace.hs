
import Data.Maybe
import System.Random
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Ref
import Simulation.Aivika.Process
import Simulation.Aivika.Random

import qualified Simulation.Aivika.DoubleLinkedList as DLL

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                -- spcStopTime = 1000.0,
                spcStopTime = 300.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4 }
        
-- | Return an exponentially distributed random value with mean 
-- 1 / @lambda@, where @lambda@ is a parameter of the function.
exprnd :: Double -> IO Double
exprnd lambda =
  do x <- getStdRandom random
     return (- log x / lambda)
     
-- | Return a random initial temperature of the item.     
temprnd :: IO Double
temprnd =
  do x <- getStdRandom random
     return (400.0 + (600.0 - 400.0) * x)

-- | Represents the furnace.
data Furnace = 
  Furnace { furnaceNormalGen :: IO Double,
            -- ^ The normal random number generator.
            furnacePits :: [Pit],
            -- ^ The pits for ingots.
            furnacePitCount :: Ref Int,
            -- ^ The count of active pits with ingots.
            furnaceAwaitingIngots :: DLL.DoubleLinkedList Ingot,
            -- ^ The awaiting ingots in the queue.
            furnaceQueueCount :: Ref Int,
            -- ^ The queue count.
            furnaceWaitCount :: Ref Int,
            -- ^ The count of awaiting ingots.
            furnaceWaitTime :: Ref Double,
            -- ^ The wait time for all loaded ingots.
            furnaceHeatingTime :: Ref Double,
            -- ^ The heating time for all unloaded ingots.
            furnaceTemp :: Ref Double,
            -- ^ The furnace temperature.
            furnaceTotalCount :: Ref Int,
            -- ^ The total count of ingots.
            furnaceLoadCount :: Ref Int,
            -- ^ The count of loaded ingots.
            furnaceUnloadCount :: Ref Int,
            -- ^ The count of unloaded ingots.
            furnaceUnloadTemps :: Ref [Double]
            -- ^ The temperatures of all unloaded ingots.
            }

-- | A pit in the furnace to place the ingots.
data Pit = 
  Pit { pitIngot :: Ref (Maybe Ingot),
        -- ^ The ingot in the pit.
        pitTemp :: Ref Double
        -- ^ The ingot temperature in the pit.
        }

data Ingot = 
  Ingot { ingotFurnace :: Furnace,
          -- ^ The furnace.
          ingotReceiveTime :: Double,
          -- ^ The time at which the ingot was received.
          ingotReceiveTemp :: Double,
          -- ^ The temperature with which the ingot was received.
          ingotLoadTime :: Double,
          -- ^ The time of loading in the furnace.
          ingotLoadTemp :: Double,
          -- ^ The temperature when the ingot was loaded in the furnace.
          ingotCoeff :: Double
          -- ^ The heating coefficient.
          }

-- | Create a furnace.
newFurnace :: Simulation Furnace
newFurnace =
  do normalGen <- liftIO newNormalGen
     pits <- sequence [newPit | i <- [1..10]]
     pitCount <- newRef 0
     awaitingIngots <- liftIO DLL.newList
     queueCount <- newRef 0
     waitCount <- newRef 0
     waitTime <- newRef 0.0
     heatingTime <- newRef 0.0
     h <- newRef 1650.0
     totalCount <- newRef 0
     loadCount <- newRef 0
     unloadCount <- newRef 0
     unloadTemps <- newRef []
     return Furnace { furnaceNormalGen = normalGen,
                      furnacePits = pits,
                      furnacePitCount = pitCount,
                      furnaceAwaitingIngots = awaitingIngots,
                      furnaceQueueCount = queueCount,
                      furnaceWaitCount = waitCount,
                      furnaceWaitTime = waitTime,
                      furnaceHeatingTime = heatingTime,
                      furnaceTemp = h,
                      furnaceTotalCount = totalCount,
                      furnaceLoadCount = loadCount, 
                      furnaceUnloadCount = unloadCount, 
                      furnaceUnloadTemps = unloadTemps }

-- | Create a new pit.
newPit :: Simulation Pit
newPit =
  do ingot <- newRef Nothing
     h' <- newRef 0.0
     return Pit { pitIngot = ingot,
                  pitTemp  = h' }

-- | Create a new ingot.
newIngot :: Furnace -> Event Ingot
newIngot furnace =
  do t  <- liftDynamics time
     xi <- liftIO $ furnaceNormalGen furnace
     h' <- liftIO temprnd
     let c = 0.1 + (0.05 + xi * 0.01)
     return Ingot { ingotFurnace = furnace,
                    ingotReceiveTime = t,
                    ingotReceiveTemp = h',
                    ingotLoadTime = t,
                    ingotLoadTemp = h',
                    ingotCoeff = c }

-- | Heat the ingot up in the pit if there is such an ingot.
heatPitUp :: Pit -> Event ()
heatPitUp pit =
  do ingot <- readRef (pitIngot pit)
     case ingot of
       Nothing -> 
         return ()
       Just ingot -> do
         
         -- update the temperature of the ingot.
         let furnace = ingotFurnace ingot
         dt' <- liftDynamics dt
         h'  <- readRef (pitTemp pit)
         h   <- readRef (furnaceTemp furnace)
         writeRef (pitTemp pit) $ 
           h' + dt' * (h - h') * ingotCoeff ingot

-- | Check whether there are ready ingots in the pits.
ingotsReady :: Furnace -> Event Bool
ingotsReady furnace =
  fmap (not . null) $ 
  filterM (fmap (>= 2200.0) . readRef . pitTemp) $ 
  furnacePits furnace

-- | Try to unload the ready ingot from the specified pit.
tryUnloadPit :: Furnace -> Pit -> Event ()
tryUnloadPit furnace pit =
  do h' <- readRef (pitTemp pit)
     when (h' >= 2000.0) $
       do Just ingot <- readRef (pitIngot pit)  
          unloadIngot ingot pit

-- | Try to load an awaiting ingot in the specified empty pit.
tryLoadPit :: Furnace -> Pit -> Event ()       
tryLoadPit furnace pit =
  do let ingots = furnaceAwaitingIngots furnace
     flag <- liftIO $ DLL.listNull ingots
     unless flag $
       do ingot <- liftIO $ DLL.listFirst ingots
          liftIO $ DLL.listRemoveFirst ingots
          t' <- liftDynamics time
          modifyRef (furnaceQueueCount furnace) (+ (-1))
          loadIngot (ingot { ingotLoadTime = t',
                             ingotLoadTemp = 400.0 }) pit
              
-- | Unload the ingot from the specified pit.       
unloadIngot :: Ingot -> Pit -> Event ()
unloadIngot ingot pit = 
  do h' <- readRef (pitTemp pit)
     writeRef (pitIngot pit) Nothing
     writeRef (pitTemp pit) 0.0
     
     -- count the active pits
     let furnace = ingotFurnace ingot
     count <- readRef (furnacePitCount furnace)
     writeRef (furnacePitCount furnace) (count - 1)
     
     -- how long did we heat the ingot up?
     t' <- liftDynamics time
     modifyRef (furnaceHeatingTime furnace)
       (+ (t' - ingotLoadTime ingot))
     
     -- what is the temperature of the unloaded ingot?
     modifyRef (furnaceUnloadTemps furnace) (h' :)
     
     -- count the unloaded ingots
     modifyRef (furnaceUnloadCount furnace) (+ 1)
     
-- | Load the ingot in the specified pit
loadIngot :: Ingot -> Pit -> Event ()
loadIngot ingot pit =
  do writeRef (pitIngot pit) $ Just ingot
     writeRef (pitTemp pit) $ ingotLoadTemp ingot
     
     -- count the active pits
     let furnace = ingotFurnace ingot
     count <- readRef (furnacePitCount furnace)
     writeRef (furnacePitCount furnace) (count + 1)
     
     -- decrease the furnace temperature
     h <- readRef (furnaceTemp furnace)
     let h' = ingotLoadTemp ingot
         dh = - (h - h') / fromInteger (toInteger (count + 1))
     writeRef (furnaceTemp furnace) $ h + dh

     -- how long did we keep the ingot in the queue?
     t' <- liftDynamics time
     modifyRef (furnaceWaitCount furnace) (+ 1) 
     modifyRef (furnaceWaitTime furnace)
       (+ (t' - ingotReceiveTime ingot))

     -- count the loaded ingots
     modifyRef (furnaceLoadCount furnace) (+ 1)
  
-- | Start iterating the furnace processing through the event queue.
startIteratingFurnace :: Furnace -> Event ()
startIteratingFurnace furnace = 
  let pits = furnacePits furnace
  in enqueueEventWithIntegTimes $
     do ready <- ingotsReady furnace
        when ready $ 
          do mapM_ (tryUnloadPit furnace) pits
             pits' <- emptyPits furnace
             mapM_ (tryLoadPit furnace) pits'
        mapM_ heatPitUp pits
        
        -- update the temperature of the furnace
        dt' <- liftDynamics dt
        h   <- readRef (furnaceTemp furnace)
        writeRef (furnaceTemp furnace) $
          h + dt' * (2600.0 - h) * 0.2

-- | Return all empty pits.
emptyPits :: Furnace -> Event [Pit]
emptyPits furnace =
  filterM (fmap isNothing . readRef . pitIngot) $
  furnacePits furnace

-- | Accept a new ingot.
acceptIngot :: Furnace -> Event ()
acceptIngot furnace =
  do ingot <- newIngot furnace
     
     -- counting
     modifyRef (furnaceTotalCount furnace) (+ 1)
     
     -- check what to do with the new ingot
     count <- readRef (furnacePitCount furnace)
     if count >= 10
       then do let ingots = furnaceAwaitingIngots furnace
               liftIO $ DLL.listAddLast ingots ingot
               modifyRef (furnaceQueueCount furnace) (+ 1)
       else do pit:_ <- emptyPits furnace
               loadIngot ingot pit
       
-- | Process the furnace.
processFurnace :: Furnace -> Process ()
processFurnace furnace =
  do delay <- liftIO $ exprnd (1.0 / 2.5)
     holdProcess delay
     -- we have got a new ingot
     liftEvent $ acceptIngot furnace
     -- repeat it again
     processFurnace furnace

-- | Initialize the furnace.
initializeFurnace :: Furnace -> Event ()
initializeFurnace furnace =
  do x1 <- newIngot furnace
     x2 <- newIngot furnace
     x3 <- newIngot furnace
     x4 <- newIngot furnace
     x5 <- newIngot furnace
     x6 <- newIngot furnace
     let p1 : p2 : p3 : p4 : p5 : p6 : ps = 
           furnacePits furnace
     loadIngot (x1 { ingotLoadTemp = 550.0 }) p1
     loadIngot (x2 { ingotLoadTemp = 600.0 }) p2
     loadIngot (x3 { ingotLoadTemp = 650.0 }) p3
     loadIngot (x4 { ingotLoadTemp = 700.0 }) p4
     loadIngot (x5 { ingotLoadTemp = 750.0 }) p5
     loadIngot (x6 { ingotLoadTemp = 800.0 }) p6
     writeRef (furnaceTotalCount furnace) 6
     writeRef (furnaceTemp furnace) 1650.0
     
-- | Return a count, average and deviation.
stats :: [Double] -> (Int, Double, Double)
stats xs = (length xs, ex, sx)
  where
    n  = fromInteger $ toInteger $ length xs
    ex = sum xs / n
    dx = (sum . map rho) xs / (n - 1.0)
    sx = sqrt dx
    rho x = (x - ex) ^ 2

-- | The simulation model.
model :: Simulation ()
model =
  do furnace <- newFurnace
     pid <- newProcessId

     -- initialize the furnace and start its iterating in start time
     runEventInStartTime IncludingCurrentEvents $
       do initializeFurnace furnace
          startIteratingFurnace furnace
     
     -- accept input ingots
     runProcessInStartTime IncludingCurrentEvents
       pid (processFurnace furnace)
     
     -- run the model in the final time point
     runEventInStopTime IncludingCurrentEvents $
       do -- the ingots
          c0 <- readRef (furnaceTotalCount furnace)
          c1 <- readRef (furnaceLoadCount furnace)
          c2 <- readRef (furnaceUnloadCount furnace)
          c3 <- readRef (furnaceWaitCount furnace)
              
          liftIO $ do
            putStrLn "The count of ingots:"
            putStrLn $ "  total  = " ++ show c0
            putStrLn $ "  loaded = " ++ show c1
            putStrLn $ "  ready  = " ++ show c2
            putStrLn $ "  awaited in the queue = " ++ show c3
            putStrLn ""
         
          -- the temperature of the ready ingots
          (n1, e1, d1) <- 
            fmap stats $ readRef (furnaceUnloadTemps furnace)
                
          liftIO $ do 
            putStrLn "The temperature of the ready ingots:"
            putStrLn $ "  average   = " ++ show e1
            putStrLn $ "  deviation = " ++ show d1
            putStrLn ""
                
          -- the ingots in pits
          r2 <- readRef (furnacePitCount furnace)
              
          liftIO $ do
            putStrLn "The ingots in pits (in the final time): "
            putStrLn $ show r2
            putStrLn ""
              
          -- the queue size
          r3 <- readRef (furnaceQueueCount furnace)
     
          liftIO $ do
            putStrLn "The queue size (in the final time): "
            putStrLn $ show r3
            putStrLn ""
              
          -- the mean wait time in the queue
          waitTime <- readRef (furnaceWaitTime furnace)
          waitCount <- readRef (furnaceWaitCount furnace)

          let t4 = waitTime / fromIntegral waitCount
         
          -- the mean heating time
          heatingTime <- readRef (furnaceHeatingTime furnace)
          unloadCount <- readRef (furnaceUnloadCount furnace)

          let t5 = heatingTime / fromIntegral unloadCount
                    
          liftIO $ do
            putStrLn $ "The mean wait time: " ++ show t4
            putStrLn $ "The mean heating time: " ++ show t5

-- | The main program.
main = runSimulation model specs
