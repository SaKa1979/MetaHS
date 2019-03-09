{-|
Module      : MetaHS.DataModel.Extractor.Module.Contains
Description : The MetaHS extractor for contains relations
License     : <to-be-determined>
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
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.QName
    as QName
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.DeclHead
    as DeclHead

-- | Create MetaModel.Relations for top-level declarations of a module.
contains :: Module SrcSpanInfo  -- ^ The module to analyze
         -> MetaModel.Relation  -- ^ list of Element `Contains` Element
contains m = case Module.name m of
  Just mn -> fromList $ mlhr ++ mli ++ mle ++ dlr
    where
      mlhr = containsModuleHead mn m                                          -- mlhr = ModuleHead-Location Relation
      mli = concat [containsImportDecl mn d | d <- Module.imports m]          -- mli = ImportDecl-Location Relation
      mle = concat [containsExportSpec mn d | d <- Module.getModuleExports m] -- mle = ExportSpec-Location Relation
      dlr = concat [containsDecl mn d | d <- Module.declarations m]           -- dlr = Decl-Location Relation
  Nothing -> empty

-- | Creates a list of (Module "m",Module head "mh") pair for the ModuleHead
containsModuleHead :: String                                  -- ^ The module (head) name.
                   -> Module SrcSpanInfo                      -- ^ The Declaration with var l.
                   -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "m",Module head "mh") pairs
containsModuleHead mn mod = [(m,mh)]
  where m = MetaModel.Module mn
        mh = MetaModel.ModuleHead $ makeQualifiedId mn mn

-- | Creates a list of (Module "m",ModuleImport "mi") pair for the ImportDecl
containsImportDecl :: String                                  -- ^ The module name.
                   -> ImportDecl SrcSpanInfo                  -- ^ The Declaration with var l.
                   -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "m",ModuleImport "mi") pairs
containsImportDecl mn ImportDecl{importModule=im} = [(m,mi)]
  where m = MetaModel.Module mn
        mi = MetaModel.ModuleImport $ makeQualifiedId mn $ hn im
        hn (ModuleName _ x) = x

-- | Creates a (Module "m",ModuleExport "me") pair for the ExportSpec
containsExportSpec :: String                                  -- ^ The module name.
                   -> ExportSpec SrcSpanInfo                  -- ^ The Declaration with var l.
                   -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "m",ModuleExport "me") pairs
containsExportSpec mn (EVar _ qnm) = case QName.name qnm of
  Just es -> [(m,me)]
    where m =  MetaModel.Module mn
          me = MetaModel.ModuleExport $ makeQualifiedId mn es
  Nothing -> []
containsExportSpec _ _ = []

-- | Analyzes a declaration for location information.
containsDecl :: String                                  -- ^ The name of the Module.
             -> Decl SrcSpanInfo                        -- ^ The declaration to analyze.
             -> [(MetaModel.Element,MetaModel.Element)] -- ^ The resulting list of (Element,Element) pairs.
containsDecl mn td@TypeDecl{} = containsType mn td
containsDecl mn dd@DataDecl{} = containsData mn dd
containsDecl mn dd@GDataDecl{} = containsData mn dd
containsDecl mn pb@PatBind{} = containsPattern mn pb
containsDecl mn fb@FunBind{} = containsFunction mn fb
containsDecl mn ts@TypeSig{} = containsTypeSig mn ts
containsDecl mn tc@ClassDecl{} = containsTypeClass mn tc
containsDecl mn id@InstDecl{} = containsInstance mn id
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
      c = MetaModel.TypeSynonym $ makeQualifiedId mn $ DeclHead.name h
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
        dce = MetaModel.DataType $ makeQualifiedId mn dcn                       -- dce  = data constructor element

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
            vce = MetaModel.Function $ makeQualifiedId mn vcn                                -- vce = value constructor element
            vcn = Decl.valueConstructorName vc                                  -- vcn = value constructor name
            cfs = concat [cf f | f <- Decl.valueConstructorFields vc]           -- cfs = `Contains` relation for sub fields (if present)

        -- Creates `Contain` relation for the supplied field
        cf :: Decl.Field -> [(MetaModel.Element,MetaModel.Element)]
        cf f = mfr ++ dfr
          where
            mfr = [(me,fe) | fe <- fes]                                                     -- Module `Contains` field relation
            dfr = [(dce,fe) | fe <- fes]                                                    -- Data `Contains` field relation
            fes = [MetaModel.Function (makeQualifiedId mn n) | n <- Decl.fieldNames f]      -- fes = field elements
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
        c = MetaModel.Function $ makeQualifiedId mn pn
    Nothing -> []

-- | Creates a list of (Module "mn",Function "fn") pairs for a top-level
-- function (FunBind) declaration.
containsFunction :: String                                   -- ^ Name of the module
                 -> Decl SrcSpanInfo                         -- ^ The top-level declaration
                 -> [(MetaModel.Element,MetaModel.Element)]  -- ^ list of (Module "mn",Function "fn") pairs
containsFunction mn fb@FunBind{} = case Decl.functionName fb of
    Just pn -> [(p,c)]
      where
        p = MetaModel.Module mn
        c = MetaModel.Function $ makeQualifiedId mn pn
    Nothing -> []

-- | Creates a list of (Module "m", TypeSignature "ts") pairs for TypeSignature declarations.
containsTypeSig :: String                                  -- ^ The module name.
                -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
                -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "m", TypeSignature "ts") pairs.
containsTypeSig mn tysig@TypeSig{} = case Decl.typeSigName tysig of
  Just tsn -> [(m,ts)]
    where m = MetaModel.Module mn
          ts = MetaModel.TypeSignature $ makeQualifiedId mn tsn
  Nothing -> []

-- | Creates a list of (Module "m",TypeClass "tc") pairs for TypeClass declarations.
containsTypeClass :: String                                  -- ^ The module name.
                  -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
                  -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "m",TypeClass "tc") pairs.
containsTypeClass mn tycla@ClassDecl{} = case Decl.typeClassName tycla of
  Just tcn -> [(m,tc)]
    where m = MetaModel.Module mn
          tc = MetaModel.TypeClass $ makeQualifiedId mn tcn
  Nothing -> []

-- | Creates a list of (Module "m",Instance "i") pairs for Instance declarations.
containsInstance :: String                                   -- ^ The module name.
                  -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
                  -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "m",Instance "i") pairs.
containsInstance mn inst@InstDecl{} = case Decl.instanceName inst of
  Just inm -> [(m,i)]
    where m = MetaModel.Module mn
          i = MetaModel.Instance $ makeQualifiedId mn inm
  Nothing -> []