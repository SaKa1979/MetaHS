{-|
Module      : MetaHS.Extensions.MacroLevelAggregation.Utils
Description : Utility functions for accessing the metric information in the MetaModel
License     : <to-be-determined>
Maintainer  : sanderkamps79@gmail.com
Stability   : experimental
-}
module MetaHS.Extensions.MacroLevelAggregation.Utils
    (getMetricElements)
    where

import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel
import qualified Data.Set as Set

-- | Extracts the metric values from the Relation associated with supplied key
getMetricElements :: RelationKey        -- ^ The MetaModel Relation Key (E.g., LCOM).
                  -> MetaModel.MetaModel-- ^ The MetaModel Containing the associated key.
                  -> [Int]              -- ^ The list containing all (sane) values associated with the supplied key.
getMetricElements key mm = [x | Just x <- ms]
  where ms = map getValue $ Set.toList $getRelation key mm

-- | Extracts the value of the supplied metric Element Pair
getValue  :: MetaModel.Pair -- ^ The Module Element to analyze. Should be (Module,IntValue)
          -> Maybe Int      -- ^ The metric value for the supplied (Module,IntValue) Pair
getValue elementPair@(MetaModel.Module _, MetaModel.IntValue _) = Just (MetaModel.intValue $ snd elementPair)
getValue _ = Nothing