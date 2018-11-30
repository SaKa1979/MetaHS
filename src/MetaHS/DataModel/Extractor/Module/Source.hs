{-|
Module      : MetaHS.DataModel.Extractor.Module.Source
Description : The MetaHS extractor for source relations
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

MetaHS extractor for module level source relations
-}
module MetaHS.DataModel.Extractor.Module.Source
    ( source
    ) where

import Data.Maybe (fromMaybe)
import Data.Set (fromList)
import Language.Haskell.Exts
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.DataModel.Utils
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Module
    as Module
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Decl
    as Decl
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.DeclHead
    as DeclHead
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.SrcLoc
    as SrcLoc

-- | Creates Location relations for all supported top-level declarations in the
-- provided Module.
source :: Module SrcSpanInfo  -- ^ The Module to analyze.
       -> MetaModel.Relation  -- ^ The resulting MetaModel.Relation items.
source m = fromList $ mlr : concat [locationDecl mn d | d <- ds]
  where
    mlr = (me,mle)                                                              -- mlr = Module-Location-Relation
    me  = MetaModel.Module mn                                                   -- me = Module Element
    mle = SrcLoc.srcSpanInfoToLocationElement $ ann m                           -- mle = Module Location Element
    mn  = fromMaybe "?" $ Module.name m                                         -- mn = Module name
    ds  = Module.declarations m                                                 -- ds = declarations

-- | Creates a list of (Module "mn",Location) pairs for a given top-level
-- declaration.
locationDecl :: String                                  -- ^ The module name.
             -> Decl SrcSpanInfo                        -- ^ The Decl to process.
             -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "mn",Location) pairs
locationDecl mn (TypeDecl l h _) = [(te,tl)]
  where
    te = MetaModel.TypeSynonym $ qn mn $ DeclHead.name h
    tl = SrcLoc.srcSpanInfoToLocationElement l
locationDecl mn dd@DataDecl{} = locationData mn dd
locationDecl mn dd@GDataDecl{} = locationData mn dd
locationDecl mn pb@PatBind{} = locationPattern mn pb
locationDecl mn fb@FunBind{} = locationFunction mn fb
locationDecl _ _ = []

-- | Creates a list of (Module "mn",Location) pairs for data type declarations.
locationData :: String                -- ^ The module name.
             -> Decl SrcSpanInfo      -- ^ The Decl to process.
             -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "mn",Location) pairs
locationData mn decl = case Decl.dataConstructor decl of
    Just d -> [(de,dl)]
      where
        de = MetaModel.DataType $ qn mn $ Decl.dataConstructorName d
        dl = SrcLoc.srcSpanInfoToLocationElement $ ann decl
    Nothing -> []


-- | Creates a list of (Module "mn",Location) pairs for PatBind declarations.
locationPattern :: String                -- ^ The module name.
                -> Decl SrcSpanInfo      -- ^ The Decl to process.
                -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "mn",Location) pairs
locationPattern mn pb@PatBind{} = case Decl.patternName pb of
    Just pn -> [(p,c)]
      where
        p = MetaModel.Function $ qn mn pn
        c = SrcLoc.srcSpanInfoToLocationElement $ ann pb
    Nothing -> []
locationPattern _ _ = []


-- | Creates a list of (Module "mn",Location) pairs for FunBind declarations.
locationFunction :: String                -- ^ The module name.
                 -> Decl SrcSpanInfo      -- ^ The Decl to process.
                 -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "mn",Location) pairs
locationFunction mn fb@FunBind{} = case Decl.functionName fb of
    Just fn -> [(p,c)]
      where
        p = MetaModel.Function $ qn mn fn
        c = SrcLoc.srcSpanInfoToLocationElement $ ann fb
    Nothing -> []
locationFunction _ _ = []
