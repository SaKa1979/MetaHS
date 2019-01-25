module MetaHS.Extensions.MacroLevelAggregation.Average
    (
    average
    ) where

import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel
import qualified Data.Set as Set

average :: RelationKey
           -> MetaModel.MetaModel
           -> Double
average key mm = fromIntegral(addition) / fromIntegral(valuesCount)
    where
    addition = foldr (+) 0 values
    valuesCount = length values
    values = getMetricElements key mm


getMetricElements :: RelationKey
                  -> MetaModel.MetaModel
                  -> [Int]
getMetricElements key mm =
    map getValue $ Set.toList $getRelation key mm


getValue  :: MetaModel.Pair -- ^ The Module Element to analyze.
          -> Int            -- ^ The metric information for the given (Element) Pair.
getValue elementPair =
    MetaModel.intValue $ snd elementPair


