module MetaHS.Extensions.MacroLevelAggregation.Population
    where

import MetaHS.Extensions.MacroLevelAggregation.Utils
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel
import Data.List (sort)

population :: RelationKey          -- ^ The MetaModel Relation Key (E.g., LCOM).
           -> MetaModel.MetaModel  -- ^ The MetaModel Containing the associated key.
           -> [Int]                -- ^ The set containing the MetricValues
population key mm = sort $ getMetricElements key mm