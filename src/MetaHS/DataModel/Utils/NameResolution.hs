{-|
Module      : MetaHS.DataModel.Utils.NameResolution
Description : The MetaHS extractor for uses relations
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

NameResolution functions
-}
module MetaHS.DataModel.Utils.NameResolution
    ( NameResolutionMap
    , NameResolutionMaps
    , createNameResolutionMaps
    , resolveType
    , resolveValue
    ) where

import Data.Maybe (fromMaybe)
import Data.Set (filter)
import qualified Data.Map as Map
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.DataModel.Utils.Name


-- | A NameResolutionMap is a Data.Map that map from plain identifiers to resolved MetaModel.Element (e.g. "sum" -> Function "M.N.sum").
type NameResolutionMap = Map.Map String MetaModel.Element

-- |  The NameResolutionMaps. The first Data.Map is applicable to types; the second Data.Map is applicable to values.
type NameResolutionMaps = (NameResolutionMap, NameResolutionMap)


-- | Creates the NameResolution Data.Map instances.
createNameResolutionMaps :: String              -- ^ Module name.
                         -> MetaModel.Relation  -- ^ The "Module mn `Contains` Element" Relations.
                         -> NameResolutionMaps  -- ^ The resulting NameResolutionMaps.
createNameResolutionMaps mn r = foldr f empty fcrs
      where
        fcrs = Data.Set.filter (isModuleNameContains mn) r                      -- fcrs = filtered contains relations. List of "Module mn `Contains` Element" Relations that are applicable to the current Module.
        empty = (Map.empty, Map.empty) :: NameResolutionMaps
        unqualifiedName = snd . split
        f (_,mmdt@(MetaModel.DataType mmdtn)) nrms =                            -- mmdt = MetaModel DataType, mmdtn = MetaModel DataType name, nrms = NameResolutionMaps
            (Map.insert key value $ fst nrms, snd nrms)
              where
                key = unqualifiedName mmdtn
                value = mmdt
        f (_,mmts@(MetaModel.TypeSynonym mmtsn)) nrms =                         -- mmts = MetaModel TypeSynonym, mmtsn = MetaModel TypeSynonym name, nrms = NameResolutionMaps
            (Map.insert key value $ fst nrms, snd nrms)
              where
                key = unqualifiedName mmtsn
                value = mmts
        f (_,mmf@(MetaModel.Function mmfn)) nrms =                              -- mmf = MetaModel Function, mmfn = MetaModel Function name, nrms = NameResolutionMaps
            (fst nrms, Map.insert key value $ snd nrms)
              where
                key = unqualifiedName mmfn
                value = mmf
        f _ nrms = nrms


-- | Resolves a simple type name to the corresponding qualified Element.
--   The simple type name will be qualified with "?" and wrapped in an
--   MetaModel.UnknownType Element  if the mapping is not known by the
--   provided NameResolutionMaps.
resolveType :: String            -- ^ The simple value name.
            -> NameResolutionMaps -- ^ The NameResolutionMaps.
            -> MetaModel.Element  -- ^ The corresponding Element.
resolveType s nrms = fromMaybe unknown lookupResult
  where
    lookupResult = Map.lookup s $ fst nrms                                      -- lookupResult = result from the lookup in the types NameResolutionMap
    unknown = MetaModel.UnknownType $ qn "?" s                                  -- unknown = UnknownType object in case lookupResult == Nothing


-- | Resolves a simple value name to the corresponding qualified Element.
--   The simple value name will be qualified with "?" if the mapping is not
--   known by the provided NameResolutionMaps.
resolveValue :: String            -- ^ The simple value name.
            -> NameResolutionMaps -- ^ The NameResolutionMaps.
            -> MetaModel.Element  -- ^ The corresponding Element.
resolveValue s nrms = fromMaybe unknown lookupResult
  where
    lookupResult = Map.lookup s $ snd nrms                                      -- lookupResult = result from the lookup in the value NameResolutionMap
    unknown = MetaModel.Function $ qn "?" s                                     -- unknown = Function object in case lookupResult == Nothing


-- | Returns True if the Relation is of the form (Module "mn",Element)
--   where mn is the provided Module name.
isModuleNameContains :: String                                -- ^ Module name.
                     -> (MetaModel.Element,MetaModel.Element) -- ^ The (Element,Element) pair to check.
                     -> Bool                                  -- ^ Check result
isModuleNameContains mn (MetaModel.Module n,_) = n == mn
isModuleNameContains _ _ = False
