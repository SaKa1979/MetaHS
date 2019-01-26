module MetaHS.Extensions.MacroLevelAggregation.Average
    where

import MetaHS.Extensions.MacroLevelAggregation.Utils
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel

-- | Calculates the average for the supplied metric Relation Key
average :: RelationKey          -- ^ The MetaModel Relation Key (E.g., LCOM).
        -> MetaModel.MetaModel  -- ^ The MetaModel Containing the associated key.
        -> Double               -- ^ The average
average key mm = fromIntegral addition / fromIntegral valuesCount
    where
    addition = sum values
    valuesCount = length values
    values = getMetricElements key mm