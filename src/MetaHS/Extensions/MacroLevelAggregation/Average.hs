{-|
Module      : MetaHS.MacroLevelAggregation
Description : The MetaHS Macro-level aggregation method: average
Copyright   : Copyright (C) 2017-2019 H.H.R.F. Vos, S. Kamps
License     : MIT
Maintainer  : hhrf.vos@studie.ou.nl, sanderkamps79@gmail.com
Stability   : experimental
Calculates the average of the metric values corresponding to supplied Relation Key.
-}
module MetaHS.Extensions.MacroLevelAggregation.Average
    where

import MetaHS.Extensions.MacroLevelAggregation.Utils
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel

-- | Calculate the average of the metric values associated with the supplied metric Relation key
average :: RelationKey          -- ^ The MetaModel Relation Key (E.g., LCOM).
        -> MetaModel.MetaModel  -- ^ The MetaModel Containing the associated key.
        -> Double               -- ^ The average
average key mm = fromIntegral addition / fromIntegral valuesCount
    where
    addition = sum values
    valuesCount = length values
    values = getMetricElements key mm