module MetaHS.Extensions.MacroLevelAggregation.Median
  (median)
where

import Data.List (sort)

import MetaHS.Extensions.MacroLevelAggregation.Utils
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.EDSL.MetaModel

-- | Calculate the average of the metric values associated with the supplied metric Relation key
median :: RelationKey          -- ^ The MetaModel Relation Key (E.g., LCOM).
        -> MetaModel.MetaModel  -- ^ The MetaModel Containing the associated key.
        -> Double               -- ^ The average
median key mm = calcMedian values
  where
    values = getMetricElements key mm

calcMedian :: [Int] -> Double --todo wrap in Maybe
calcMedian xs | null xs = 0.0 --todo and return Nothing
              | odd $ length xs = calcOdd xs
              | even $ length xs = calcEven xs

calcOdd :: [Int] -> Double
calcOdd xs = fromIntegral $ sort xs !! idx
  where idx = length xs `div` 2

calcEven :: [Int] -> Double
calcEven xs =   (fromIntegral (sort xs !! (idx - 1)) + fromIntegral (sort xs !! idx)) / 2
  where
    len = length xs
    idx = len `div` 2