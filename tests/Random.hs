
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Trans

import Data.List

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart
import Simulation.Aivika.Experiment.Chart.Backend.Cairo

import Graphics.Rendering.Chart.Backend.Cairo

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates =
  foldl (\seen x -> if x `elem` seen
                    then seen
                    else seen ++ [x]) []
                    
specs = Specs 0 300 0.05 RungeKutta4 SimpleGenerator

experiment = 
  defaultExperiment {
    experimentTitle = "Test of Random Number Generators",
    experimentSpecs = specs }

seriesGenerator title description series =
  [outputView $ defaultTimeSeriesView {
      timeSeriesWidth = 1000,
      timeSeriesTitle = title ++ " - Time Series",
      timeSeriesDescription = description,
      timeSeriesLeftYSeries = series },
   outputView $ defaultHistogramView {
     histogramWidth = 1000,
     histogramTitle = title ++ " - Histogram",
     histogramDescription = description,
     histogramSeries = series },
   outputView $ defaultTimingStatsView {
     timingStatsTitle = title ++ " - Statistics",
     timingStatsDescription = description,
     timingStatsSeries = series }]

histogramGenerator title description series =
  [outputView $ defaultHistogramView {
      histogramWidth = 1000,
      histogramTitle = title ++ " - Histogram",
      histogramDescription = description,
      histogramSeries = series }]

uniformTitle  = "Uniform Random"
uniformDescr  = "Uniform " ++ show (m1, m2)
uniformSeries = resultByName "rnd"

triagTitle  = "Triangle Random"
triagDescr  = "Triangle " ++ show (m1, m3, m2) ++
              ", EX = " ++ show (triagMean m1 m3 m2) ++
              ", sqrt(DX) = " ++ show (triagDeviation m1 m3 m2)
triagSeries = resultByName "triag"

triagMean a m b      = (a + m + b) / 3
triagDeviation a m b = sqrt $ (a*(a-m) + b*(b-a) + m*(m-b)) / 18


normalTitle  = "Normal Random"
normalDescr  = "Normal " ++ show (mu, nu)
normalSeries = resultByName "normal"

logNormalTitle  = "LogNormal Random"
logNormalDescr  = "LogNormal " ++ show (logMu, logNu) ++
                  ", EX = " ++ show (logNormalMean logMu logNu) ++
                  ", sqrt(DX) = " ++ show (logNormalDeviation logMu logNu)
logNormalSeries = resultByName "lognormal"

logNormalMean mu nu      = exp (mu + nu * nu / 2)
logNormalDeviation mu nu = sqrt $ exp (nu * nu + 2 * mu) * (exp (nu * nu) - 1)

expTitle  = "Exponential Random"
expDescr  = "Exponential (" ++ show mu ++ ")"
expSeries = resultByName "exp"

poissonTitle  = "Poisson Random"
poissonDescr  = "Poisson (" ++ show mu ++ ")"
poissonSeries = resultByName "poisson"

binomialTitle  = "Binomial Random"
binomialDescr  = "Binomial " ++ show (p, n)
binomialSeries = resultByName "binomial"

gammaTitle kappa theta  = "Gamma " ++ show (kappa, theta) ++ " Random"
gammaDescr kappa theta  = "Gamma " ++ show (kappa, theta) ++
                          ", EX = " ++ show (gammaMean kappa theta) ++
                          ", sqrt(DX) = " ++ show (gammaDeviation kappa theta)
gammaSeries kappa theta = resultByName $ "gamma" ++ show (kappa, theta)

gammaSetTitle i pars  = "Gamma Distribution Set " ++ show i
gammaSetDescr i pars  = "It shows the Gamma distribution with different parameters."
gammaSetSeries i pars =
  mconcat $
  flip map pars $ \(kappa, theta) ->
  resultByName $ "gamma" ++ show (kappa, theta)

gammaMean kappa theta      = kappa * theta
gammaDeviation kappa theta = sqrt (kappa * theta * theta)

gammaParamSet = [[(0.5, 1), (1, 1), (3, 1)], [(3, 0.2), (3, 1/3), (3, 1)]]
gammaParams   = removeDuplicates $ concat gammaParamSet

betaTitle alpha beta  = "Beta " ++ show (alpha, beta) ++ " Random"
betaDescr alpha beta  = "Beta " ++ show (alpha, beta) ++
                          ", EX = " ++ show (betaMean alpha beta) ++
                          ", sqrt(DX) = " ++ show (betaDeviation alpha beta)
betaSeries alpha beta = resultByName $ "beta" ++ show (alpha, beta)

betaSetTitle i pars  = "Beta Distribution Set " ++ show i
betaSetDescr i pars  = "It shows the Beta distribution with different parameters."
betaSetSeries i pars =
  mconcat $
  flip map pars $ \(alpha, beta) ->
  resultByName $ "beta" ++ show (alpha, beta)

betaMean alpha beta      = alpha / (alpha + beta)
betaDeviation alpha beta = sqrt (alpha * beta / ((alpha + beta) * (alpha + beta) * (alpha + beta + 1)))

-- betaParamSet = [[(5, 1.5), (1.5, 5), (3, 1.5), (1.5, 3)],
--                 [(0.8, 0.2), (0.5, 0.5), (0.2, 0.8)],
--                 [(2, 0.2), (2, 0.8), (2, 1)],
--                 [(5, 5), (2, 2), (1, 1)]]

betaParamSet = [[(0.5, 0.5), (5, 1), (1, 3), (2, 2), (2, 5)]]
betaParams   = removeDuplicates $ concat betaParamSet

weibullTitle alpha beta  = "Weibull " ++ show (alpha, beta) ++ " Random"
weibullDescr alpha beta  = "Weibull " ++ show (alpha, beta)
weibullSeries alpha beta = resultByName $ "weibull" ++ show (alpha, beta)

weibullSetTitle i pars  = "Weibull Distribution Set " ++ show i
weibullSetDescr i pars  = "It shows the Weibull distribution with different parameters."
weibullSetSeries i pars =
  mconcat $
  flip map pars $ \(alpha, beta) ->
  resultByName $ "weibull" ++ show (alpha, beta)

-- weibullParamSet = [[(0.5, 1), (1, 1), (1.5, 1), (5, 1)]]
weibullParamSet = [[(1, 1), (1.5, 1), (5, 1)]]
weibullParams   = removeDuplicates $ concat weibullParamSet

discreteTitle  = "Discrete Random"
discreteDescr  = "Discrete PDF = " ++ show discretePDF
discreteSeries = resultByName "discrete"
discretePDF    = [(1, 0.1), (3, 0.3), (5, 0.5), (9, 0.1)] :: DiscretePDF Double

generators =
  [outputView defaultExperimentSpecsView] ++
  seriesGenerator uniformTitle uniformDescr uniformSeries ++
  seriesGenerator triagTitle triagDescr triagSeries ++
  seriesGenerator normalTitle normalDescr normalSeries ++
  seriesGenerator logNormalTitle logNormalDescr logNormalSeries ++
  seriesGenerator expTitle expDescr expSeries ++
  seriesGenerator poissonTitle poissonDescr poissonSeries ++
  seriesGenerator binomialTitle binomialDescr binomialSeries ++
  seriesGenerator discreteTitle discreteDescr discreteSeries ++
  parametricGenerator gammaTitle gammaDescr gammaSeries gammaParams ++
  setGenerator gammaSetTitle gammaSetDescr gammaSetSeries gammaParamSet ++
  parametricGenerator betaTitle betaDescr betaSeries betaParams ++
  setGenerator betaSetTitle betaSetDescr betaSetSeries betaParamSet ++
  parametricGenerator weibullTitle weibullDescr weibullSeries weibullParams ++
  setGenerator weibullSetTitle weibullSetDescr weibullSetSeries weibullParamSet
  where
    parametricGenerator title descr series params =
      concat $
      flip map params $ \(p1, p2) ->
      let title'  = title p1 p2
          descr'  = descr p1 p2
          series' = series p1 p2
      in seriesGenerator title' descr' series'
    setGenerator title descr series params =
      concat $
      flip map (zip [1..] params) $ \(i, ps) ->
      let title'  = title i ps
          descr'  = descr i ps
          series' = series i ps
      in histogramGenerator title' descr' series'

m1 = 2 :: Double
m2 = 8 :: Double
m3 = 6 :: Double

mu = 5 :: Double
nu = 3 :: Double

logMu = 0 :: Double
logNu = 0.3 :: Double

p = 0.1 :: Double
n = 5 :: Int

model :: Simulation Results
model =
  do rndX <- memoRandomUniformDynamics (return m1) (return m2)
     triagX <- memoRandomTriangularDynamics (return m1) (return m3) (return m2)
     normalX <- memoRandomNormalDynamics (return mu) (return nu)
     logNormalX <- memoRandomLogNormalDynamics (return logMu) (return logNu)
     expX <- memoRandomExponentialDynamics (return mu)
     poissonX <- memoRandomPoissonDynamics (return mu)
     binomialX <- memoRandomBinomialDynamics (return p) (return n)
     discreteX <- memoRandomDiscreteDynamics (return discretePDF)
     gammaXs <- forM gammaParams $ \(kappa, theta) ->
       memoRandomGammaDynamics (return kappa) (return theta)
     betaXs <- forM betaParams $ \(alpha, beta) ->
       memoRandomBetaDynamics (return alpha) (return beta)
     weibullXs <- forM weibullParams $ \(alpha, beta) ->
       memoRandomWeibullDynamics (return alpha) (return beta)
     let parametricSources title series params =
           flip map (zip series params) $ \(series, params) ->
           let name  = title ++ show params
               descr = title ++ show params
           in resultSource name descr series
     return $
       results $
       [resultSource "rnd" "rnd" rndX,
        resultSource "triag" "triag" triagX,
        resultSource "normal" "normal" normalX,
        resultSource "lognormal" "lognormal" logNormalX,
        resultSource "exp" "exp" expX,
        resultSource "poisson" "poisson" poissonX,
        resultSource "binomial" "binomial" binomialX,
        resultSource "discrete" "discrete" discreteX] ++
       parametricSources "gamma" gammaXs gammaParams ++
       parametricSources "beta" betaXs betaParams ++
       parametricSources "weibull" weibullXs weibullParams
    
main = runExperiment experiment generators (WebPageRenderer $ CairoRenderer PNG) model
