
import Simulation.Aivika.Dynamics

specs = Specs { spcStartTime = 0, 
                spcStopTime = 13, 
                spcDT = 0.01,
                spcMethod = RungeKutta4 }

model :: Dynamics (Dynamics [Double])
model =
  do integA <- newInteg 100
     integB <- newInteg 0
     integC <- newInteg 0
     let a = integValue integA
         b = integValue integB
         c = integValue integC
     let ka = 1
         kb = 1
     integDiff integA (- ka * a)
     integDiff integB (ka * a - kb * b)
     integDiff integC (kb * b)
     return $ sequence [a, b, c]

main = 
  do a <- runDynamics1 model specs
     print a
