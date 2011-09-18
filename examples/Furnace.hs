
import Data.Maybe
import Random
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Base
import Simulation.Aivika.Dynamics.Lift
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Ref
import Simulation.Aivika.Dynamics.UVar
import Simulation.Aivika.Dynamics.Process
import Simulation.Aivika.Dynamics.Memo
import Simulation.Aivika.Dynamics.Random
import Simulation.Aivika.Statistics

import qualified Simulation.Aivika.Queue as Q

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
  Furnace { furnaceQueue :: EventQueue,
            -- ^ The event queue.
            furnaceNormalGen :: IO Double,
            -- ^ The normal random number generator.
            furnacePits :: [Pit],
            -- ^ The pits for ingots.
            furnacePitCount :: UVar Int,
            -- ^ The count of active pits with ingots.
            furnacePitCountStats :: Statistics Int,
            -- ^ The statistics about the active pits.
            furnaceAwaitingIngots :: Q.Queue Ingot,
            -- ^ The awaiting ingots in the queue.
            furnaceQueueCount :: UVar Int,
            -- ^ The queue count.
            furnaceQueueCountStats :: Statistics Int,
            -- ^ The statistics about the queue count.
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
  Pit { pitQueue :: EventQueue,
        -- ^ The bound dynamics queue.
        pitIngot :: Ref (Maybe Ingot),
        -- ^ The ingot in the pit.
        pitTemp :: Ref Double
        -- ^ The ingot temperature in the pit.
        }

data Ingot = 
  Ingot { ingotFurnace :: Furnace,
          -- ^ Return the furnace.
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
newFurnace :: EventQueue -> Dynamics Furnace
newFurnace queue =
  do normalGen <- liftIO normalGen
     pits <- sequence [newPit queue | i <- [1..10]]
     pitCount <- newUVar queue 0
     pitCountStats <- liftIO newStatistics
     awaitingIngots <- liftIO Q.newQueue
     queueCount <- newUVar queue 0
     queueCountStats <- liftIO newStatistics
     waitCount <- newRef queue 0
     waitTime <- newRef queue 0.0
     heatingTime <- newRef queue 0.0
     h <- newRef queue 1650.0
     totalCount <- newRef queue 0
     loadCount <- newRef queue 0
     unloadCount <- newRef queue 0
     unloadTemps <- newRef queue []
     return Furnace { furnaceQueue = queue,
                      furnaceNormalGen = normalGen,
                      furnacePits = pits,
                      furnacePitCount = pitCount,
                      furnacePitCountStats = pitCountStats,
                      furnaceAwaitingIngots = awaitingIngots,
                      furnaceQueueCount = queueCount,
                      furnaceQueueCountStats = queueCountStats,
                      furnaceWaitCount = waitCount,
                      furnaceWaitTime = waitTime,
                      furnaceHeatingTime = heatingTime,
                      furnaceTemp = h,
                      furnaceTotalCount = totalCount,
                      furnaceLoadCount = loadCount, 
                      furnaceUnloadCount = unloadCount, 
                      furnaceUnloadTemps = unloadTemps }

-- | Create a new pit.
newPit :: EventQueue -> Dynamics Pit
newPit queue =
  do ingot <- newRef queue Nothing
     h' <- newRef queue 0.0
     return Pit { pitQueue = queue,
                  pitIngot = ingot,
                  pitTemp  = h' }

-- | Create a new ingot.
newIngot :: Furnace -> Dynamics Ingot
newIngot furnace =
  do t  <- time
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
heatPitUp :: Pit -> Dynamics ()
heatPitUp pit =
  do ingot <- readRef (pitIngot pit)
     case ingot of
       Nothing -> 
         return ()
       Just ingot -> do
         
         -- update the temperature of the ingot.
         let furnace = ingotFurnace ingot
         dt' <- dt
         h'  <- readRef (pitTemp pit)
         h   <- readRef (furnaceTemp furnace)
         writeRef (pitTemp pit) $ 
           h' + dt' * (h - h') * ingotCoeff ingot

-- | Check whether there are ready ingots in the pits.
ingotsReady :: Furnace -> Dynamics Bool
ingotsReady furnace =
  fmap (not . null) $ 
  filterM (fmap (>= 2200.0) . readRef . pitTemp) $ 
  furnacePits furnace

-- | Try to unload the ready ingot from the specified pit.
tryUnloadPit :: Furnace -> Pit -> Dynamics ()
tryUnloadPit furnace pit =
  do h' <- readRef (pitTemp pit)
     when (h' >= 2000.0) $
       do Just ingot <- readRef (pitIngot pit)  
          unloadIngot ingot pit

-- | Try to load an awaiting ingot in the specified empty pit.
tryLoadPit :: Furnace -> Pit -> Dynamics ()       
tryLoadPit furnace pit =
  do let ingots = furnaceAwaitingIngots furnace
     flag <- liftIO $ Q.queueNull ingots
     unless flag $
       do ingot <- liftIO $ Q.queueFront ingots
          liftIO $ Q.dequeue ingots
          t' <- time
          modifyUVar (furnaceQueueCount furnace) (+ (-1))
          c <- readUVar (furnaceQueueCount furnace)
          liftIO $ addStatistics (furnaceQueueCountStats furnace) c
          loadIngot (ingot { ingotLoadTime = t',
                             ingotLoadTemp = 400.0 }) pit
              
-- | Unload the ingot from the specified pit.       
unloadIngot :: Ingot -> Pit -> Dynamics ()
unloadIngot ingot pit = 
  do h' <- readRef (pitTemp pit)
     writeRef (pitIngot pit) Nothing
     writeRef (pitTemp pit) 0.0
     
     -- count the active pits
     let furnace = ingotFurnace ingot
     count <- readUVar (furnacePitCount furnace)
     writeUVar (furnacePitCount furnace) (count - 1)
     liftIO $ addStatistics (furnacePitCountStats furnace) (count - 1)
     
     -- how long did we heat the ingot up?
     t' <- time
     modifyRef (furnaceHeatingTime furnace)
       (+ (t' - ingotLoadTime ingot))
     
     -- what is the temperature of the unloaded ingot?
     modifyRef (furnaceUnloadTemps furnace) (h' :)
     
     -- count the unloaded ingots
     modifyRef (furnaceUnloadCount furnace) (+ 1)
     
-- | Load the ingot in the specified pit
loadIngot :: Ingot -> Pit -> Dynamics ()
loadIngot ingot pit =
  do writeRef (pitIngot pit) $ Just ingot
     writeRef (pitTemp pit) $ ingotLoadTemp ingot
     
     -- count the active pits
     let furnace = ingotFurnace ingot
     count <- readUVar (furnacePitCount furnace)
     writeUVar (furnacePitCount furnace) (count + 1)
     liftIO $ addStatistics (furnacePitCountStats furnace) (count + 1)
     
     -- decrease the furnace temperature
     h <- readRef (furnaceTemp furnace)
     let h' = ingotLoadTemp ingot
         dh = - (h - h') / fromInteger (toInteger (count + 1))
     writeRef (furnaceTemp furnace) $ h + dh

     -- how long did we keep the ingot in the queue?
     t' <- time
     when (ingotReceiveTime ingot < t') $
       do modifyRef (furnaceWaitCount furnace) (+ 1) 
          modifyRef (furnaceWaitTime furnace)
            (+ (t' - ingotReceiveTime ingot))

     -- count the loaded ingots
     modifyRef (furnaceLoadCount furnace) (+ 1)
  
-- | Iterate the furnace processing.
iterateFurnace :: Furnace -> Dynamics (Dynamics ())
iterateFurnace furnace = 
  let pits = furnacePits furnace
  in iterateD $
     do ready <- ingotsReady furnace
        when ready $ 
          do mapM_ (tryUnloadPit furnace) pits
             pits' <- emptyPits furnace
             mapM_ (tryLoadPit furnace) pits'
        mapM_ heatPitUp pits
        
        -- update the temperature of the furnace
        dt' <- dt
        h   <- readRef (furnaceTemp furnace)
        writeRef (furnaceTemp furnace) $
          h + dt' * (2600.0 - h) * 0.2

-- | Return all empty pits.
emptyPits :: Furnace -> Dynamics [Pit]
emptyPits furnace =
  filterM (fmap isNothing . readRef . pitIngot) $
  furnacePits furnace

-- | Accept a new ingot.
acceptIngot :: Furnace -> Dynamics ()
acceptIngot furnace =
  do ingot <- newIngot furnace
     
     -- counting
     modifyRef (furnaceTotalCount furnace) (+ 1)
     
     -- check what to do with the new ingot
     count <- readUVar (furnacePitCount furnace)
     if count >= 10
       then do let ingots = furnaceAwaitingIngots furnace
               liftIO $ Q.enqueue ingots ingot
               modifyUVar (furnaceQueueCount furnace) (+ 1)
               c <- readUVar (furnaceQueueCount furnace)
               liftIO $ addStatistics (furnaceQueueCountStats furnace) c
       else do pit:_ <- emptyPits furnace
               loadIngot ingot pit
       
-- | Process the furnace.
processFurnace :: Furnace -> Process ()
processFurnace furnace =
  do delay <- liftIO $ exprnd (1.0 / 2.5)
     holdProcess delay
     -- we have got a new ingot
     liftD $ acceptIngot furnace
     -- repeat it again
     processFurnace furnace

-- | Initialize the furnace.
initializeFurnace :: Furnace -> Dynamics ()
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
model :: Dynamics (Dynamics ())
model =
  do queue <- newQueue
     furnace <- newFurnace queue
     pid <- newProcessID queue
     
     initializeFurnace furnace
     
     -- get the furnace iterator
     iterator <- iterateFurnace furnace
     
     -- accept input ingots
     t0 <- starttime
     runProcess (processFurnace furnace) pid t0
     
     let system :: Dynamics ()
         system = 
           do iterator   --  iterate in each time point
         
              -- the ingots
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
              r2 <- liftIO $ statisticsResults (furnacePitCountStats furnace)
              
              liftIO $ do
                putStrLn "The ingots in pits: "
                putStrLn $ showResults r2 2 []
                putStrLn ""
              
              -- the queue size
              r3 <- liftIO $ statisticsResults (furnaceQueueCountStats furnace)
     
              liftIO $ do
                putStrLn "The queue size: "
                putStrLn $ showResults r3 2 []
                putStrLn ""
              
              -- the mean wait time in the queue
              t4 <- readRef (furnaceWaitTime furnace) /
                   fmap (fromInteger . toInteger)
                   (readRef (furnaceWaitCount furnace))
              
              -- the mean heating time
              t5 <- readRef (furnaceHeatingTime furnace) /
                   fmap (fromInteger . toInteger)
                   (readRef (furnaceUnloadCount furnace))
                    
              liftIO $ do
                putStrLn $ "The mean wait time: " ++ show t4
                putStrLn $ "The mean heating time: " ++ show t5
         
     return system

-- | The main program.
main = runDynamics1 model specs
