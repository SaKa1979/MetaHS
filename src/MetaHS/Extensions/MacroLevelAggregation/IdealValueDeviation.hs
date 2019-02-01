module MetaHS.Extensions.MacroLevelAggregation.IdealValueDeviation
  (idealValueDeviation)
  where

import MetaHS.Extensions.MacroLevelAggregation.Utils
import MetaHS.Extensions.MacroLevelAggregation.Average
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel

-- | Calculate the Ideal Value Deviation of the metric values associated with the supplied metric Relation key.
idealValueDeviation :: RelationKey          -- ^ The MetaModel Relation Key (E.g., LCOM).
                    -> MetaModel.MetaModel  -- ^ The MetaModel Containing the associated key.
                    -> Int                  -- ^ The lower bound.
                    -> Int                  -- ^ The ideal value.
                    -> Int                  -- ^ The upper bound.
                    -> Double               -- ^ The deviation from the ideal value.
idealValueDeviation key mm lowerBound idealValue upperBound = ivd ll ideal ul avg
  where
  ll = fromIntegral lowerBound
  ideal = fromIntegral idealValue
  ul = fromIntegral upperBound
  avg = average key mm

-- | Calculate the Ideal Value Deviation based on the supplied metric average.
ivd :: Double -- ^ The lower bound.
    -> Double -- ^ The ideal value.
    -> Double -- ^ The upper bound.
    -> Double -- ^ The metric average to be evaluated.
    -> Double -- ^ The deviation from the ideal value.
ivd ll ideal ul avg
  | ll < avg && avg <= ideal = lowerThanIdeal ll ideal avg
  | ideal < avg && avg < ul = higherThanIdeal ideal ul avg
  | avg <= ll || avg >= ul = 0.0

-- | Calculate the Ideal Value Deviation when the metric average is between the lower bound and the ideal value.
lowerThanIdeal :: Double -- ^ The lower bound.
               -> Double -- ^ The ideal value.
               -> Double -- ^ The metric average to be evaluated.
               -> Double -- ^ The deviation from the ideal value.
lowerThanIdeal ll ideal m = (m - ll) / (ideal - ll)

-- | Calculate the Ideal Value Deviation when the metric average is between the ideal value and the lower bound.
higherThanIdeal :: Double -- ^ The ideal value.
                -> Double -- ^ The upper bound.
                -> Double -- ^ The metric average to be evaluated.
                -> Double -- ^ The deviation from the ideal value.
higherThanIdeal ideal ul m = 1 - ((m - ideal) / (ul - ideal))