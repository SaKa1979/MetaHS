module MetaHS.Extensions.MacroLevelAggregation.Utils
    (getMetricElements)
    where

import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel
import qualified Data.Set as Set

-- | Extracts the metric values from the Relation associated with supplied key
getMetricElements :: RelationKey        -- ^ The MetaModel Relation Key (E.g., LCOM).
                  -> MetaModel.MetaModel-- ^ The MetaModel Containing the associated key.
                  -> [Int]              -- ^ The list containing all values of supplied key
getMetricElements key mm = map getValue $ Set.toList $getRelation key mm


-- | Extracts the value of the supplied metric Element Pair
getValue  :: MetaModel.Pair -- ^ The Module Element to analyze. Should be (Module,IntValue)
          -> Int            -- ^ The metric value for the supplied (Element) Pair.
getValue elementPair = MetaModel.intValue $ snd elementPair
