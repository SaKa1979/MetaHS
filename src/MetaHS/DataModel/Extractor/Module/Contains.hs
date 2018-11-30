{-|
Module      : MetaHS.DataModel.Extractor.Module.Contains
Description : The MetaHS extractor for contains relations
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

MetaHS extractor for module level contains relations
-}
module MetaHS.DataModel.Extractor.Module.Contains
    ( contains
    ) where

import Data.Set (fromList, empty)
import Language.Haskell.Exts
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.DataModel.Utils
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Module
    as Module
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Decl
    as Decl
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.DeclHead
    as DeclHead

-- | Create MetaModel.Relations for top-level declarations of a module.
contains :: Module SrcSpanInfo  -- ^ The module to analyze
         -> MetaModel.Relation  -- ^ list of Element `Contains` Element
contains m = case Module.name m of
  Just mn -> fromList $ concat [containsDecl mn d | d <- Module.declarations m]
  Nothing -> empty

-- | Analyzes a declaration for location information.
containsDecl :: String                                  -- ^ The name of the Module.
             -> Decl SrcSpanInfo                        -- ^ The declaration to analyze.
             -> [(MetaModel.Element,MetaModel.Element)] -- ^ The resulting list of (Element,Element) pairs.
containsDecl mn td@TypeDecl{} = containsType mn td
containsDecl mn dd@DataDecl{} = containsData mn dd
containsDecl mn dd@GDataDecl{} = containsData mn dd
containsDecl mn pb@PatBind{} = containsPattern mn pb
containsDecl mn fb@FunBind{} = containsFunction mn fb
containsDecl _ _ = []


-- | Creates a list of (Module "mn",TypeSynonym "tsn") pairs for a top-level
-- type (TypeDecl) declarations.
containsType :: String                                  -- ^ Name of the module
             -> Decl SrcSpanInfo                        -- ^ The top-level declaration
             -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "mn",TypeSynonym "tsn") pairs
containsType mn d = case d of
  (TypeDecl _ h _) -> [(p,c)]
    where
      p = MetaModel.Module mn
      c = MetaModel.TypeSynonym $ qn mn $ DeclHead.name h
  _ -> []

-- | Creates a list of (Element,Element) pairs for a top-level data (DataDecl)
-- declaration.
containsData :: String                                  -- ^ Name of the module
             -> Decl SrcSpanInfo                        -- ^ The top-level declaration
             -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Element,Element) pairs
containsData mn d =  case Decl.dataConstructor d of
    Just a -> cdc a
      where
        me  = MetaModel.Module mn                                               -- module element
        dcn = Decl.dataConstructorName a                                        -- dcn  = data constructor name
        dce = MetaModel.DataType $ qn mn dcn                                    -- dce  = data constructor element

        -- Creates `Contain` relation for the data constructor.
        cdc :: Decl.DataConstructor -> [(MetaModel.Element,MetaModel.Element)]  -- cdc = contains data constructor
        cdc dc = (me,dce) : cvcs
          where
            vcs  = Decl.valueConstructors dc                                    -- vcs  = value constructors
            cvcs = concat [cvc vc | vc <- vcs]                                  -- cvcs = `Contains` relation for sub value constructors

        -- Creates `Contain` relation for the value constructor.
        cvc :: Decl.ValueConstructor -> [(MetaModel.Element,MetaModel.Element)] -- cvc = contains value constructor
        cvc vc = mvcr : dvcr : cfs
          where
            mvcr = (me,vce)                                                     -- Module `Contains` value constructor relation
            dvcr = (dce,vce)                                                    -- Data `Contains` value constructor relation
            vce = MetaModel.Function $ qn mn vcn                                -- vce = value constructor element
            vcn = Decl.valueConstructorName vc                                  -- vcn = value constructor name
            cfs = concat [cf f | f <- Decl.valueConstructorFields vc]           -- cfs = `Contains` relation for sub fields (if present)

        -- Creates `Contain` relation for the supplied field
        cf :: Decl.Field -> [(MetaModel.Element,MetaModel.Element)]
        cf f = mfr ++ dfr
          where
            mfr = [(me,fe) | fe <- fes]                        -- Module `Contains` field relation
            dfr = [(dce,fe) | fe <- fes]                       -- Data `Contains` field relation
            fes = [MetaModel.Function (qn mn n) | n <- Decl.fieldNames f]       -- fes = field elements
    Nothing -> []


-- | Creates a list of (Module "mn",Function "fn") pairs for a top-level pattern
-- (PatBind) declaration.
containsPattern :: String                                   -- ^ Name of the module
                -> Decl SrcSpanInfo                         -- ^ The top-level declaration
                -> [(MetaModel.Element,MetaModel.Element)]  -- ^ list of (Module "mn",Function "fn") pairs
containsPattern mn pb@PatBind{} = case Decl.patternName pb of
    Just pn -> [(p,c)]
      where
        p = MetaModel.Module mn
        c = MetaModel.Function $ qn mn pn
    Nothing -> []
containsPattern _ _ = []


-- | Creates a list of (Module "mn",Function "fn") pairs for a top-level
-- function (FunBind) declaration.
containsFunction :: String                -- ^ Name of the module
                 -> Decl SrcSpanInfo      -- ^ The top-level declaration
                 -> [(MetaModel.Element,MetaModel.Element)]  -- ^ list of (Module "mn",Function "fn") pairs
containsFunction mn fb@FunBind{} = case Decl.functionName fb of
    Just pn -> [(p,c)]
      where
        p = MetaModel.Module mn
        c = MetaModel.Function $ qn mn pn
    Nothing -> []
containsFunction _ _ = []
