
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart
import Simulation.Aivika.Experiment.Chart.Backend.Cairo

import Graphics.Rendering.Chart.Backend.Cairo

specs = Specs 0 100 0.1 RungeKutta4 SimpleGenerator

experiment = 
  defaultExperiment {
    experimentSpecs = specs }

generators =
  [outputView defaultExperimentSpecsView,
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Random Time Series",
     timeSeriesDescription = "Random " ++ show (m1, m2),
     timeSeriesLeftYSeries = resultByName "rnd" },
   outputView $ defaultTimingStatsView {
     timingStatsTitle = "Random Statistics",
     timingStatsDescription = "Random " ++ show (m1, m2),
     timingStatsSeries = resultByName "rnd" },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Normal Time Series",
     timeSeriesDescription = "Normal " ++ show (mu, nu),
     timeSeriesLeftYSeries = resultByName "normal" },
   outputView $ defaultTimingStatsView {
     timingStatsTitle = "Normal Statistics",
     timingStatsDescription = "Normal " ++ show (mu, nu),
     timingStatsSeries = resultByName "normal" },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Exponential Time Series",
     timeSeriesDescription = "Exponential " ++ show mu,
     timeSeriesLeftYSeries = resultByName "exp" },
   outputView $ defaultTimingStatsView {
     timingStatsTitle = "Exponential Statistics",
     timingStatsDescription = "Exponential " ++ show mu,
     timingStatsSeries = resultByName "exp" },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Poisson Time Series",
     timeSeriesDescription = "Poisson " ++ show mu,
     timeSeriesLeftYSeries = resultByName "poisson" },
   outputView $ defaultTimingStatsView {
     timingStatsTitle = "Poisson Statistics",
     timingStatsDescription = "Poisson " ++ show mu,
     timingStatsSeries = resultByName "poisson" },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Binomial Time Series",
     timeSeriesDescription = "Binomial " ++ show (p, n),
     timeSeriesLeftYSeries = resultByName "binomial" },
   outputView $ defaultTimingStatsView {
     timingStatsTitle = "Binomial Statistics",
     timingStatsDescription = "Binomial " ++ show (p, n),
     timingStatsSeries = resultByName "binomial" } ]

m1 = 2 :: Double
m2 = 8 :: Double

mu = 5 :: Double
nu = 3 :: Double

p = 0.1 :: Double
n = 5 :: Int

model :: Simulation Results
model =
  do rndX <- memoRandomUniformDynamics (return m1) (return m2)
     normalX <- memoRandomNormalDynamics (return mu) (return nu)
     expX <- memoRandomExponentialDynamics (return mu)
     poissonX <- memoRandomPoissonDynamics (return mu)
     binomialX <- memoRandomBinomialDynamics (return p) (return n)
     return $
       results
       [resultSource "rnd" "rnd" rndX,
        resultSource "normal" "normal" normalX,
        resultSource "exp" "exp" expX,
        resultSource "poisson" "poisson" poissonX,
        resultSource "binomial" "binomial" binomialX]
    
main = runExperiment experiment generators (CairoRenderer PNG) model
