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
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.QName
    as QName
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.DeclHead
    as DeclHead
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.SrcLoc
    as SrcLoc
import Debug.Trace

-- | Creates Location relations for all supported top-level declarations in the
-- provided Module.
source :: Module SrcSpanInfo  -- ^ The Module to analyze.
       -> MetaModel.Relation  -- ^ The resulting MetaModel.Relation items.
--source m = fromList $ mlr : concat [locationDecl mn d | d <- ms]
source m = fromList $ mlr : mlhr ++ mli ++ mle ++ dlr
  where
    mlr = locationModule mn m                                                   -- mlr = Module-Location Relation
    mlhr = locationModuleHead mn m                                              -- mlhr = ModuleHead-Location Relation
    mli = concat [locationImportDecl mn d | d <- is]                            -- mli = ImportDecl-Location Relation
    mle = concat [locationExportSpec mn d | d <- es]                            -- mle = ExportSpec-Location Relation
    dlr = concat [locationDecl mn d | d <- ms]                                  -- dlr = Decl-Location Relation
    mn = fromMaybe "?" $ Module.name m                                          -- mn = Module name
    ms = Module.declarations m                                                  -- ms = module declaration list
    is = Module.imports m                                                       -- is = import declaration list
    es = getModuleExports $ Module.exports m
getModuleExports :: Maybe (ExportSpecList l) -> [ExportSpec l]
getModuleExports (Just (ExportSpecList _ es)) = es
getModuleExports Nothing = []


-- | Creates a Module "m",Location "ml" pair for the whole module
locationModule :: String                                -- ^ The module (head) name.
               -> Module SrcSpanInfo                    -- ^ The Declaration with var l.
               -> (MetaModel.Element,MetaModel.Element) -- ^ list of (Module "mn",Location) pairs
locationModule mn (Module l _ _ _ _) = (m,ml)
  where m =  MetaModel.Module mn
        ml = SrcLoc.srcSpanInfoToLocationElement $ l

-- | Creates a list of (Module head "mh",Location "mhl") pair for the ModuleHead
locationModuleHead :: String                                  -- ^ The module (head) name.
                   -> Module SrcSpanInfo                      -- ^ The Declaration with var l.
                   -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module head "mh",Location "mhl") pairs
locationModuleHead mn (Module _ (Just (ModuleHead l _ _ _)) _ _ _) = [(mh,mhl)]
                       where mh =  MetaModel.ModuleHead mn
                             mhl = SrcLoc.srcSpanInfoToLocationElement l
locationModuleHead mn (Module _ (Nothing) _ _ _) = []

-- | Creates a list of (imported Module "mi",Location "mil") pair for the ImportDecl
locationImportDecl :: String                                  -- ^ The module name.
                   -> ImportDecl SrcSpanInfo                  -- ^ The Declaration with var l.
                   -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (imported Module "mi",Location "mil") pairs
locationImportDecl mn ImportDecl{importAnn=ia, importModule=im} = [(mi,mil)]
                      where mi = MetaModel.ModuleImport $ makeQualifiedId mn $ hn im
                            mil = SrcLoc.srcSpanInfoToLocationElement ia
                            hn (ModuleName _ x) = x

-- | Creates a (exported Modules "me",Location "mel") pair for the ExportSpec
locationExportSpec :: String                                  -- ^ The module name.
                   -> ExportSpec SrcSpanInfo                  -- ^ The Declaration with var l.
                   -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (exported Modules "me",Location "mel") pairs
locationExportSpec mn (EVar l qnm) = case QName.name qnm of
    Just es -> [(me,mel)]
      where me = MetaModel.ModuleExport $ makeQualifiedId mn es
            mel = SrcLoc.srcSpanInfoToLocationElement l
    Nothing -> []
locationExportSpec _ _ = []

-- | Creates a list of (Element,Location) pairs for a given top-level declaration.
locationDecl :: String                                  -- ^ The module name.
             -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
             -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Element "te", Location "tl") pairs.
locationDecl mn (TypeDecl l h _) = [(te,tl)]
  where
    te = MetaModel.TypeSynonym $ makeQualifiedId mn $ DeclHead.name h
    tl = SrcLoc.srcSpanInfoToLocationElement l
locationDecl mn dd@DataDecl{} = locationData mn dd
locationDecl mn dd@GDataDecl{} = locationData mn dd
locationDecl mn pb@PatBind{} = locationPattern mn pb
locationDecl mn fb@FunBind{} = locationFunction mn fb
locationDecl mn ts@TypeSig{} = locationTypeSig mn ts
locationDecl mn tc@ClassDecl{} = locationTypeClass mn tc
locationDecl mn id@InstDecl{} = locationInstance mn id
locationDecl _ _ = []

-- | Creates a list of ((G)Dataconstructor qname "d", Location "dl") pairs for (g)DataDecl declarations.
locationData :: String                                  -- ^ The module name.
             -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
             -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of ((G)Dataconstructor qname "d", Location "dl") pairs.
locationData mn decl = case Decl.dataConstructor decl of
    Just dc -> [(d,dl)]
      where
        d = MetaModel.DataType $ makeQualifiedId mn $ Decl.dataConstructorName dc
        dl = SrcLoc.srcSpanInfoToLocationElement $ ann decl
    Nothing -> []

-- | Creates a list of (Pattern qname "p", Location "pl") pairs for PatBind declarations.
locationPattern :: String                                   -- ^ The module name.
                -> Decl SrcSpanInfo                         -- ^ The Declaration with var l.
                -> [(MetaModel.Element,MetaModel.Element)]  -- ^ list of (Pattern qname "p", Location "pl") pairs.
locationPattern mn patbin@PatBind{} = case Decl.patternName patbin of
    Just pn -> [(p,pl)]
      where
        p = MetaModel.Function $ makeQualifiedId mn pn
        pl = SrcLoc.srcSpanInfoToLocationElement $ ann patbin
    Nothing -> []
locationPattern _ _ = []

-- | Creates a list of (Module "mn",Location) pairs for FunBind declarations.
locationFunction :: String                                  -- ^ The module name.
                 -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
                 -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Function qname "f", Location "fl") pairs.
locationFunction mn funbin@FunBind{} = case Decl.functionName funbin of
    Just fn -> [(f,fl)]
      where
        f = MetaModel.Function $ makeQualifiedId mn fn
        fl = SrcLoc.srcSpanInfoToLocationElement $ ann funbin
    Nothing -> []
locationFunction _ _ = []

-- | Creates a list of (TypeSignature "ts",Location "tsl") pairs for TypeSignature declarations.
locationTypeSig :: String                                  -- ^ The module name.
                -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
                -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (TypeSignature qname, "ts",Location "tsl") pairs.
locationTypeSig mn tysig@TypeSig{} = case Decl.typeSigName tysig of
  Just tsn -> [(ts,tsl)]
    where ts = MetaModel.TypeSignature $ makeQualifiedId mn tsn
          tsl = SrcLoc.srcSpanInfoToLocationElement $ ann tysig
  Nothing -> []
locationTypeSig _ _ = []

-- | Creates a list of (TypeClass "tc",Location "tcl") pairs for TypeClass declarations.
locationTypeClass :: String                                  -- ^ The module name.
                  -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
                  -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (TypeClass qname, "tc",Location "tcl") pairs.
locationTypeClass mn tycla@ClassDecl{} = case Decl.typeClassName tycla of
  Just tcn -> [(ts,tsl)]
    where ts = MetaModel.TypeClass $ makeQualifiedId mn tcn
          tsl = SrcLoc.srcSpanInfoToLocationElement $ ann tycla
  Nothing -> []
locationTypeClass _ _ = []

-- | Creates a list of (Instance "i",Location "il") pairs for Instance declarations.
locationInstance :: String                                   -- ^ The module name.
                  -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
                  -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Instance qname, "i",Location "il") pairs.
locationInstance mn inst@InstDecl{} = case Decl.instanceName inst of
  Just inm -> [(i,il)]
    where i = MetaModel.Instance $ makeQualifiedId mn inm
          il = SrcLoc.srcSpanInfoToLocationElement $ ann inst
  Nothing -> []
locationInstance _ _ = []