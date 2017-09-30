
-- |
-- Module     : Simulation.Aivika.Results.Locale
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines locales for outputting and printing the simulation results.
--
module Simulation.Aivika.Results.Locale
       (-- * Basic Types
        ResultLocale,
        ResultLocalisation(..),
        ResultDescription,
        UserDefinedResult(..),
        LocalisedResult(..),
        -- * Locale Codes
        russianResultLocale,
        englishResultLocale,
        -- * Localisations
        russianResultLocalisation,
        englishResultLocalisation,
        pathResultLocalisation,
        localisePathResultDescription,
        localisePathResultTitle,
        lookupResultLocalisation,
        -- * Unique Identifiers
        ResultId(..),
        -- * Utilities
        resultNameToTitle) where

import Simulation.Aivika.Results.Locale.Types
import Simulation.Aivika.Results.Locale.Russian
import Simulation.Aivika.Results.Locale.English
