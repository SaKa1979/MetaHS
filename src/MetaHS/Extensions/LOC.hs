module MetaHS.Extensions.LOC
  ( keyLoc
  , locAggregator
  ) where

import qualified Data.Set                   as Set
import           MetaHS.DataModel.MetaModel
import           MetaHS.EDSL.MetaModel

-- | MetaModel key used for the LOC relation.
keyLoc :: RelationKey
keyLoc = "LOC"

calcLoc ::
     MetaModel-- ^ The meta-model.
  -> Element -- ^ Module
  -> Int -- ^ The calculated LCOM metric value.
calcLoc mm mod = (foldr (+) 0 ls)
  where
    ls = [extractLines x | x <- es]
    es = [elementSource mm mc | mc <- moduleContains mm mod]
    extractLines :: Maybe Element -> Int
    extractLines jl =
      case jl of
        Just Location {locationStartLine = ls, locationEndLine = le} -> (le - ls) + 1
        Nothing -> 0

-- | LOC aggregator that adds the LOC metric relation to the supplied meta-model.
locAggregator :: MetaModel-- ^ The supplied meta-model.
              -> MetaModel-- ^ The complemented meta-model.
locAggregator mm = setRelation keyLoc r mm
  where
    r = foldr f Set.empty $ getModules mm
    f mod s = Set.insert (mod, lv mod) s
    lv m = IntValue $ calcLoc mm m
