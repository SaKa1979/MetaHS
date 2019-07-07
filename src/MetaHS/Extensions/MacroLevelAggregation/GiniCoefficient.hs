{-|
Module      : MetaHS.MacroLevelAggregation
Description : The MetaHS Macro-level aggregation method: Gini-coefficient
Copyright   : Copyright (C) 2017-2019 H.H.R.F. Vos, S. Kamps
License     : MIT
Maintainer  : hhrf.vos@studie.ou.nl, sanderkamps79@gmail.com
Stability   : experimental
Calculates the Gini-coefficient of the metric population corresponding
to supplied Relation Key. Result is a value bounded by [0,1].
-}
module MetaHS.Extensions.MacroLevelAggregation.GiniCoefficient
  (giniCoefficient)
  where

import MetaHS.Extensions.MacroLevelAggregation.Utils
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel
import qualified Data.Set as Set
import qualified Data.List as List

-- | Calculate the Gini-coefficient of the metric values associated with the supplied metric Relation key
giniCoefficient :: RelationKey          -- ^ The MetaModel Relation Key (E.g., LCOM).
                -> MetaModel.MetaModel  -- ^ The MetaModel Containing the associated key.
                -> Double               -- ^ The Gini coefficient
giniCoefficient key mm = gini ordered_list
  where
  ordered_list = List.sort xs
  xs = getMetricElements key mm

-- | Calculates the Gini-coefficient of the elements contained in the supplied list
gini :: [Int]  -- ^ Ordered list
     -> Double -- ^ The Gini-coefficient
gini xs | null xs = 0.0
        | length xs == 1 = 0.0
        | otherwise = fromIntegral (deltaSum xs) / fromIntegral (last $ xsAccum xs) / fromIntegral (length xs)

-- | Sums up halve of all delta's of each consecutive element in a list. (x_i - x_i-1)
-- | This means that from a supplied list, the delta of each member with respect to
-- | each other member, will be added together.
-- | This function represents the numerator part of the Gini-coefficient algorithm.
-- | (although with one halve of the symmetric metric)
deltaSum :: [Int] -- ^ An ordered list
         -> Int   -- ^ All delta's added together
deltaSum xs = fromIntegral delta_cum
  where
  delta_cum = sum deltas
  deltas = zipWith3 (\idx x x_accum -> idx * x - x_accum) indices xs $ xsAccum xs
  xs_ord = List.sort xs
  indices = [0..length xs - 1]

-- | Every element in the resulting list is an accumulation of all it's predecessors.
xsAccum :: [Int] -- ^ An ordered list
         -> [Int] -- ^ Accumulated values
xsAccum = scanl (+) 0