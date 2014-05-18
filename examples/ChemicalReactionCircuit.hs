
-- Note that the integCircut function uses Euler's method regardless of
-- the simulation specs specified. Therefore, to receieve almost the same
-- results in the old example based on using the integ function, you should
-- specify Euler's method in their specs in that file, although the Runge-Kutta
-- method gives similar results too, which is expected.
--
-- Finally, the integ function can be significantly faster than integCircuit,
-- although they have different purposes.

{-# LANGUAGE Arrows #-}

import Control.Arrow

import Simulation.Aivika

specs = Specs { spcStartTime = 0, 
                spcStopTime = 13, 
                spcDT = 0.01,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

circuit :: Circuit () [Double]
circuit =
  let ka = 1
      kb = 1
  in proc () -> do
    rec let da = - ka * a
            db = ka * a - kb * b
            dc = kb * b
        a  <- integCircuit 100 -< da
        b  <- integCircuit 0 -< db
        c  <- integCircuit 0 -< dc
    returnA -< [a, b, c]

model :: Simulation [Double]
model =
  do results <-
       runTransform (circuitTransform circuit) $
       return ()
     runDynamicsInStopTime results

main = runSimulation model specs >>= print
