
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

generators =
  [outputView defaultExperimentSpecsView] ++
  seriesGenerator uniformTitle uniformDescr uniformSeries ++
  seriesGenerator triagTitle triagDescr triagSeries ++
  seriesGenerator normalTitle normalDescr normalSeries ++
  seriesGenerator logNormalTitle logNormalDescr logNormalSeries ++
  seriesGenerator expTitle expDescr expSeries ++
  seriesGenerator poissonTitle poissonDescr poissonSeries ++
  seriesGenerator binomialTitle binomialDescr binomialSeries ++
  (concat $
   flip map gammaParams $ \(kappa, theta) ->
   let title  = gammaTitle kappa theta
       descr  = gammaDescr kappa theta
       series = gammaSeries kappa theta
   in seriesGenerator title descr series) ++
  (concat $ flip map (zip [1..] gammaParamSet) $ \(i, pars) ->
    let title  = gammaSetTitle i pars
        descr  = gammaSetDescr i pars
        series = gammaSetSeries i pars
    in histogramGenerator title descr series) ++
  (concat $
   flip map betaParams $ \(alpha, beta) ->
   let title  = betaTitle alpha beta
       descr  = betaDescr alpha beta
       series = betaSeries alpha beta
   in seriesGenerator title descr series) ++
  (concat $ flip map (zip [1..] betaParamSet) $ \(i, pars) ->
    let title  = betaSetTitle i pars
        descr  = betaSetDescr i pars
        series = betaSetSeries i pars
    in histogramGenerator title descr series)

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
     gammaXs <- forM gammaParams $ \(kappa, theta) ->
       memoRandomGammaDynamics (return kappa) (return theta)
     betaXs <- forM betaParams $ \(alpha, beta) ->
       memoRandomBetaDynamics (return alpha) (return beta)
     return $
       results $
       [resultSource "rnd" "rnd" rndX,
        resultSource "triag" "triag" triagX,
        resultSource "normal" "normal" normalX,
        resultSource "lognormal" "lognormal" logNormalX,
        resultSource "exp" "exp" expX,
        resultSource "poisson" "poisson" poissonX,
        resultSource "binomial" "binomial" binomialX] ++
       (flip map (zip gammaXs gammaParams) $ \(gammaX, (kappa, theta)) ->
         let name  = "gamma" ++ show (kappa, theta)
             descr = "gamma" ++ show (kappa, theta)
         in resultSource name descr gammaX) ++
       (flip map (zip betaXs betaParams) $ \(betaX, (alpha, beta)) ->
         let name  = "beta" ++ show (alpha, beta)
             descr = "beta" ++ show (alpha, beta)
         in resultSource name descr betaX)
    
main = runExperiment experiment generators (WebPageRenderer $ CairoRenderer PNG) model
