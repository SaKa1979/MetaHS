module MetaHS.Extensions.MacroLevelAggregation.Average
    (
     average
    ,frequency
    ) where

import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel
import qualified Data.Set as Set

-- | Calculates the average for the supplied metric Relation Key
average :: RelationKey          -- ^ The MetaModel Relation Key (E.g., LCOM).
        -> MetaModel.MetaModel  -- ^ The MetaModel Containing the associated key.
        -> Double               -- ^ The average
average key mm = fromIntegral(addition) / fromIntegral(valuesCount)
    where
    addition = foldr (+) 0 values
    valuesCount = length values
    values = getMetricElements key mm

distribution :: RelationKey          -- ^ The MetaModel Relation Key (E.g., LCOM).
          -> MetaModel.MetaModel  -- ^ The MetaModel Containing the associated key.
          -> Set.Set (Int, Int)   -- ^ The set containing tuple(s) (MetricValue,Frequency)
distribution key mm = Set.fromList $ [(x, countValues x xs) | x <- xs]
    where
    xs = getMetricElements key mm
    countValues x = length.filter(==x)

-- | Extracts the metric values from the Relation associated with supplied key
getMetricElements :: RelationKey        -- ^ The MetaModel Relation Key (E.g., LCOM).
                  -> MetaModel.MetaModel-- ^ The MetaModel Containing the associated key.
                  -> [Int]              -- ^ The list containing all values of supplied key
getMetricElements key mm = map getValue $ Set.toList $getRelation key mm


-- | Extracts the value of the supplied metric Element Pair
getValue  :: MetaModel.Pair -- ^ The Module Element to analyze.
          -> Int            -- ^ The metric value for the supplied (Element) Pair.
getValue elementPair = MetaModel.intValue $ snd elementPair