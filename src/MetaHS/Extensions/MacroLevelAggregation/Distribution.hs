module MetaHS.Extensions.MacroLevelAggregation.Distribution
    where

import MetaHS.Extensions.MacroLevelAggregation.Utils
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel
import qualified Data.Set as Set

distribution :: RelationKey          -- ^ The MetaModel Relation Key (E.g., LCOM).
             -> MetaModel.MetaModel  -- ^ The MetaModel Containing the associated key.
             -> Set.Set (Int, Int)   -- ^ The set containing tuple(s) (Frequency, MetricValue)
distribution key mm = Set.fromList [(countValues x xs, x) | x <- xs]
    where
    xs = getMetricElements key mm
    countValues x = length.filter(==x)