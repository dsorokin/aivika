
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
     timeSeriesTitle = "Random Time Series",
     timeSeriesDescription = "Triag " ++ show (m1, m3, m2) ++
                             ", mu = " ++ show (triangularMean m1 m3 m2) ++
                             ", nu = " ++ show (triangularDeviation m1 m3 m2),
     timeSeriesLeftYSeries = resultByName "triag" },
   outputView $ defaultTimingStatsView {
     timingStatsTitle = "Random Statistics",
     timingStatsDescription = "Triag " ++ show (m1, m3, m2) ++
                              ", mu = " ++ show (triangularMean m1 m3 m2) ++
                              ", nu = " ++ show (triangularDeviation m1 m3 m2),
     timingStatsSeries = resultByName "triag" },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Normal Time Series",
     timeSeriesDescription = "Normal " ++ show (mu, nu),
     timeSeriesLeftYSeries = resultByName "normal" },
   outputView $ defaultTimingStatsView {
     timingStatsTitle = "Normal Statistics",
     timingStatsDescription = "Normal " ++ show (mu, nu),
     timingStatsSeries = resultByName "normal" },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "LogNormal Time Series",
     timeSeriesDescription = "Log Normal " ++ show (mu, nu) ++
                             ", mu = " ++ show (logNormalMean mu nu) ++
                             ", nu = " ++ show (logNormalDeviation mu nu),
     timeSeriesLeftYSeries = resultByName "lognormal" },
   outputView $ defaultTimingStatsView {
     timingStatsTitle = "LogNormal Statistics",
     timingStatsDescription = "Log Normal " ++ show (mu, nu) ++
                              ", mu = " ++ show (logNormalMean mu nu) ++
                              ", nu = " ++ show (logNormalDeviation mu nu),
     timingStatsSeries = resultByName "lognormal" },
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

triangularMean a m b = (a + m + b) / 3
triangularDeviation a m b = sqrt $ (a*(a-m) + b*(b-a) + m*(m-b)) / 18

logNormalMean mu nu = exp (mu + nu * nu / 2)
logNormalDeviation mu nu = sqrt $ exp (nu * nu + 2 * mu) * (exp (nu * nu) - 1)

m1 = 2 :: Double
m2 = 8 :: Double
m3 = 6 :: Double

mu = 1 :: Double
nu = 2 :: Double

p = 0.1 :: Double
n = 5 :: Int

model :: Simulation Results
model =
  do rndX <- memoRandomUniformDynamics (return m1) (return m2)
     triagX <- memoRandomTriangularDynamics (return m1) (return m3) (return m2)
     normalX <- memoRandomNormalDynamics (return mu) (return nu)
     logNormalX <- memoRandomLogNormalDynamics (return mu) (return nu)
     expX <- memoRandomExponentialDynamics (return mu)
     poissonX <- memoRandomPoissonDynamics (return mu)
     binomialX <- memoRandomBinomialDynamics (return p) (return n)
     return $
       results
       [resultSource "rnd" "rnd" rndX,
        resultSource "triag" "triag" triagX,
        resultSource "normal" "normal" normalX,
        resultSource "lognormal" "lognormal" logNormalX,
        resultSource "exp" "exp" expX,
        resultSource "poisson" "poisson" poissonX,
        resultSource "binomial" "binomial" binomialX]
    
main = runExperiment experiment generators (WebPageRenderer $ CairoRenderer PNG) model
