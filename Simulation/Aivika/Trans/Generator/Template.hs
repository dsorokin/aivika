
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.Generator.Template
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It allows creating random number generators using template.
--
module Simulation.Aivika.Trans.Generator.Template where

import Control.Monad.Trans

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.Session.Template
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Internal.Template

instance Generating m => Generating (TemplateComp m) where

  data Generator (TemplateComp m) = TemplateGenerator (Generator m)

  generateUniform (TemplateGenerator g) min max =
    lift $ generateUniform g min max

  generateUniformInt (TemplateGenerator g) min max =
    lift $ generateUniformInt g min max

  generateNormal (TemplateGenerator g) mu nu =
    lift $ generateNormal g mu nu

  generateExponential (TemplateGenerator g) mu =
    lift $ generateExponential g mu

  generateErlang (TemplateGenerator g) beta m =
    lift $ generateErlang g beta m

  generatePoisson (TemplateGenerator g) mu =
    lift $ generatePoisson g mu

  generateBinomial (TemplateGenerator g) prob trials =
    lift $ generateBinomial g prob trials

  newGenerator (TemplateSession s) tp =
    lift $ fmap TemplateGenerator (newGenerator s $ unliftGeneratorType tp)

  newRandomGenerator (TemplateSession s) g =
    lift $ fmap TemplateGenerator (newRandomGenerator s g) 

  newRandomGenerator01 (TemplateSession s) g =
    lift $ fmap TemplateGenerator (newRandomGenerator01 s $ runComp g) 

-- | Unlift the generator from the specified template.
unliftGenerator :: Generating m => Generator (TemplateComp m) -> Generator m
unliftGenerator (TemplateGenerator g) = g

-- | Unlift the generator type from the specified template.
unliftGeneratorType :: Generating m => GeneratorType (TemplateComp m) -> GeneratorType m
unliftGeneratorType SimpleGenerator = SimpleGenerator
unliftGeneratorType (SimpleGeneratorWithSeed i) = SimpleGeneratorWithSeed i
unliftGeneratorType (CustomGenerator g) = CustomGenerator (fmap unliftGenerator $ runComp g)
unliftGeneratorType (CustomGenerator01 g) = CustomGenerator01 (runComp g)
