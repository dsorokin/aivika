
{-# LANGUAGE RecursiveDo #-}

-- Example: Simulation of a Bouncing Ball.
--
-- It is described in MATLAB & Simulink example available by the following link:
--
-- http://www.mathworks.com/help/simulink/examples/simulation-of-a-bouncing-ball.html

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika
import Simulation.Aivika.SystemDynamics

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 25.0,
                spcDT = 0.01,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- the acceleration due to gravity
g = 9.81

-- the initial velocity
v0 = 15

-- the initial position
x0 = 10

-- the coefficient of restitution of the ball
k = 0.8

model :: Simulation Results
model = mdo
  v <- integEither dv v0
  x <- integEither dx x0
  let dv = do
        x' <- x
        v' <- v
        if x' < 0
          then return $ Left (- k * v')
          else return $ Right (- g)
      dx = do
        x' <- x
        v' <- v
        if x' < 0
          then return $ Left 0
          else return $ Right v'
  return $
    results
    [resultSource "t" "time" time,
     resultSource "x" "position" x,
     resultSource "v" "velocity" v]

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
