{-|
Module      : MetaHS.Extensions
Description : The MetaHS LCOM aggregation
License     : None
Maintainer  : sanderkamps79@gmail.com
Stability   : experimental
Date        : 3/9/19

Metamodel Element that is interesting (those that represents modules being imported) is :
Module. All other Elements that are part of the _contains relations are ignored.

CBO - Coupling between object classes
    The coupling between object classes (CBO) metric represents the number of classes coupled to a given class
    (efferent couplings, Ce).
    This coupling can occur through method calls, field accesses, inheritance, arguments, return types, and exceptions.
    ref. Chidamber and Kemerer
-}
module MetaHS.Extensions.CBO
  (keyCbo
  ,cboAggregator)
  where

import qualified Data.Set                   as Set
import           MetaHS.DataModel.MetaModel
import           MetaHS.EDSL.MetaModel

-- | MetaModel key used for the CBO relation.
keyCbo :: RelationKey
keyCbo = "CBO"

-- | CBO aggregator that adds the CBO metric relation to the supplied meta-model.
cboAggregator :: MetaModel -- ^ The supplied meta-model.
              -> MetaModel -- ^ The complemented meta-model.
cboAggregator mm = setRelation keyCbo r mm where
  r = foldr f Set.empty $ filter (\x -> not $ x == Module{name="?"}) $ getModules mm
  f mod s = Set.insert (mod, lv mod) s
  lv m = IntValue $ calcCbo mm m

calcCbo ::
     MetaModel-- ^ The meta-model.
  -> Element -- ^ Module
  -> Int -- ^ The calculated LCOM metric value.
calcCbo mm mod = length xs
  where
    xs = filter isInteresting $ moduleImports mm mod

-- | The Elements for which the CBO is measured.
isInteresting :: Element -> Bool
isInteresting (Module{}) = True
isInteresting  _         = False
