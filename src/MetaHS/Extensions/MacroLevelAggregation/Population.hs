{-|
Module      : MetaHS.MacroLevelAggregation
Description : The MetaHS Macro-level aggregation method: population
Copyright   : Copyright (C) 2017-2019 H.H.R.F. Vos, S. Kamps
License     : MIT
Maintainer  : hhrf.vos@studie.ou.nl, sanderkamps79@gmail.com
Stability   : experimental
Calculates the metric population corresponding to supplied Relation Key.
-}
module MetaHS.Extensions.MacroLevelAggregation.Population
    where

import MetaHS.Extensions.MacroLevelAggregation.Utils
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel
import Data.List (sort)

-- | Calculates the metric population corresponding to supplied Relation Key.
population :: RelationKey          -- ^ The MetaModel Relation Key (E.g., LCOM).
           -> MetaModel.MetaModel  -- ^ The MetaModel Containing the associated key.
           -> [Int]                -- ^ The set containing the MetricValues
population key mm = sort $ getMetricElements key mm