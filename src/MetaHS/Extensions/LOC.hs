{-|
Module      : MetaHS.Extensions.LOC
Description : The MetaHS EDSL LOC aggregation
Copyright   : Copyright (C) 2017-2019 H.H.R.F. Vos, S. Kamps
License     : MIT
Maintainer  : hhrf.vos@studie.ou.nl, sanderkamps79@gmail.com
Stability   : experimental
Metamodel Elements that are interesting (those that occupy distinct line numbers) are :
ModuleHead, ModuleImport, TypeSynonym, DataType, Function, TypeSignature,
TypeClass and Instance. All other Elements that are part of the _contains relations are ignored.

LOC - Sum of all non-empty, non-comment lines in a module.
-}
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
  -> Element  -- ^ Module
  -> Int      -- ^ The calculated LCOM metric value.
calcLoc mm mod = sum ls
  where
    ls = [extractLines x | x <- es]
    es = [elementSource mm mc | mc <- moduleContains mm mod, isInteresting mc]
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
    r = foldr f Set.empty $ filter (\x -> x /= Module {name = "?"}) $ getModules mm
    f mod = Set.insert (mod, lv mod)
    lv m = IntValue $ calcLoc mm m

-- | The Elements for which the loc is measured.
isInteresting :: Element -> Bool
isInteresting ModuleHead {} = True
isInteresting ModuleImport{} = True
isInteresting TypeSynonym{} = True
isInteresting DataType{} = True
isInteresting Function{} = True
isInteresting TypeSignature{} = True
isInteresting TypeClass{} = True
isInteresting Instance{} = True
isInteresting Pragma{} = True
isInteresting  _ = False