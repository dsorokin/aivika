
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

specs = Specs 0 100 0.1 RungeKutta4 SimpleGenerator

experiment =
  defaultExperiment {
    experimentGenerators =
       [outputView defaultExperimentSpecsView,
        outputView $ defaultTimeSeriesView {
          timeSeriesTitle = "Random " ++ show (m1, m2),
          timeSeries = [Left "rnd"]
          },
        outputView $ defaultTimingStatsView {
          timingStatsTitle = "Random " ++ show (m1, m2),
          timingStatsSeries = ["rnd"]
          },
        outputView $ defaultTimeSeriesView {
          timeSeriesTitle = "Normal " ++ show (mu, nu),
          timeSeries = [Left "normal"]
          },
        outputView $ defaultTimingStatsView {
          timingStatsTitle = "Normal " ++ show (mu, nu),
          timingStatsSeries = ["normal"]
          },
        outputView $ defaultTimeSeriesView {
          timeSeriesTitle = "Exponential " ++ show mu,
          timeSeries = [Left "exp"]
          },
        outputView $ defaultTimingStatsView {
          timingStatsTitle = "Exponential " ++ show mu,
          timingStatsSeries = ["exp"]
          },
        outputView $ defaultTimeSeriesView {
          timeSeriesTitle = "Poisson " ++ show mu,
          timeSeries = [Left "poisson"]
          },
        outputView $ defaultTimingStatsView {
          timingStatsTitle = "Poisson " ++ show mu,
          timingStatsSeries = ["poisson"]
          },
        outputView $ defaultTimeSeriesView {
          timeSeriesTitle = "Binomial " ++ show (p, n),
          timeSeries = [Left "binomial"]
          },
        outputView $ defaultTimingStatsView {
          timingStatsTitle = "Binomial " ++ show (p, n),
          timingStatsSeries = ["binomial"]
          }
       ]
  }

m1 = 2 :: Double
m2 = 8 :: Double

mu = 5 :: Double
nu = 3 :: Double

p = 0.1 :: Double
n = 5 :: Int

model :: Simulation ExperimentData
model =
  do rndX <- memoRandomUniformDynamics (return m1) (return m2)
     normalX <- memoRandomNormalDynamics (return mu) (return nu)
     expX <- memoRandomExponentialDynamics (return mu)
     poissonX <- memoRandomPoissonDynamics (return mu)
     binomialX <- memoRandomBinomialDynamics (return p) (return n)
     experimentDataInStartTime
       [("rnd", seriesEntity "rnd" rndX),
        ("normal", seriesEntity "normal" normalX),
        ("exp", seriesEntity "exp" expX),
        ("poisson", seriesEntity "poisson" poissonX),
        ("binomial", seriesEntity "binomial" binomialX)]
    
main = runExperiment experiment model
