
-- |
-- Module     : Simulation.Aivika.Trans.Table
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines the table functions.
--
module Simulation.Aivika.Trans.Table
       (tableLookup,
        tableLookupStepwise) where

import Data.Array

-- | Lookup @x@ in a table of pairs @(x, y)@ using linear interpolation.
tableLookup :: Double -> Array Int (Double, Double) -> Double
tableLookup x tbl = find first last x
  where
    (first, last) = bounds tbl
    find left right x =
      if left > right then
        error "Incorrect index: tableLookup"
      else
        let index = (left + 1 + right) `div` 2
            x1    = fst $ tbl ! index
        in if x1 <= x then 
             let y | index < right = find index right x
                   | right == last = snd $ tbl ! right
                   | otherwise     = 
                     let x2 = fst $ tbl ! (index + 1)
                         y1 = snd $ tbl ! index
                         y2 = snd $ tbl ! (index + 1)
                     in y1 + (y2 - y1) * (x - x1) / (x2 - x1) 
             in y
           else
             let y | left < index  = find left (index - 1) x
                   | left == first = snd $ tbl ! left
                   | otherwise     = error "Incorrect index: tableLookup"
             in y

-- | Lookup @x@ in a table of pairs @(x, y)@ using stepwise function.
tableLookupStepwise :: Double -> Array Int (Double, Double) -> Double
tableLookupStepwise x tbl = find first last x
  where
    (first, last) = bounds tbl
    find left right x =
      if left > right then
        error "Incorrect index: tableLookupStepwise"
      else
        let index = (left + 1 + right) `div` 2
            x1    = fst $ tbl ! index
        in if x1 <= x then 
             let y | index < right = find index right x
                   | right == last = snd $ tbl ! right
                   | otherwise     = snd $ tbl ! right
             in y
           else
             let y | left < index  = find left (index - 1) x
                   | left == first = snd $ tbl ! left
                   | otherwise     = error "Incorrect index: tableLookupStepwise"
             in y
