{-|
Module      : MetaHS.MacroLevelAggregation
Description : The MetaHS Macro-level aggregation method: distribution
Copyright   : Copyright (C) 2017-2019 H.H.R.F. Vos, S. Kamps
License     : MIT
Maintainer  : hhrf.vos@studie.ou.nl, sanderkamps79@gmail.com
Stability   : experimental
Calculates the distribution of the metric values corresponding to supplied Relation Key.
-}
module MetaHS.Extensions.MacroLevelAggregation.Distribution
    where

import MetaHS.Extensions.MacroLevelAggregation.Utils
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel
import qualified Data.Set as Set

-- | Calculates the distribution of the metric values corresponding to supplied Relation Key.
distribution :: RelationKey          -- ^ The MetaModel Relation Key (E.g., LCOM).
             -> MetaModel.MetaModel  -- ^ The MetaModel Containing the associated key.
             -> Set.Set (Int, Int)   -- ^ The set containing tuple(s) (Frequency, MetricValue)
distribution key mm = Set.fromList [(countValues x xs, x) | x <- xs]
    where
    xs = getMetricElements key mm
    countValues x = length.filter(==x)