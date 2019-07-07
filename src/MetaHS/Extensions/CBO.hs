{-|
Module      : MetaHS.Extensions
Description : The MetaHS LCOM aggregation
Copyright   : Copyright (C) 2017-2019 H.H.R.F. Vos, S. Kamps
License     : MIT
Maintainer  : hhrf.vos@studie.ou.nl, sanderkamps79@gmail.com
Stability   : experimental
Metamodel Element that is interesting (those that represents modules being imported) is :
Module. All other Elements that are part of the _contains relations are ignored.

CBO - Coupling between object classes
      The coupling between object classes (CBO) metric represents the number of classes coupled to a given class
      (efferent couplings, Ce).
      This coupling can occur through method calls, field accesses, inheritance, arguments, return types, and exceptions.
      ref. Chidamber and Kemerer (1994)
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
cboAggregator mm = setRelation keyCbo r mm
  where
    r = foldr f Set.empty $ filter (\x -> x /= Module {name = "?"}) $ getModules mm
    f mod = Set.insert (mod, lv mod)
    lv m = IntValue $ calcCbo mm m

calcCbo ::
     MetaModel-- ^ The meta-model.
  -> Element  -- ^ Module
  -> Int      -- ^ The calculated LCOM metric value.
calcCbo mm mod = length xs
  where
    xs = filter isInteresting $ moduleImports mm mod

-- | The Elements for which the CBO is measured.
isInteresting :: Element -> Bool
isInteresting Module {} = True
isInteresting  _        = False