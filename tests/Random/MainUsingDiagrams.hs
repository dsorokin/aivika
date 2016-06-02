
-- To run, package aivika-experiment-diagrams must be installed.

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart.Backend.Diagrams

import Graphics.Rendering.Chart.Backend.Diagrams

import qualified Data.Map as M

import Model

main = runExperiment experiment generators (WebPageRenderer $ DiagramsRenderer SVG loadCommonFonts) model
