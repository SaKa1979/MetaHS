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

import           Data.Maybe                                                   (fromMaybe)
import           Data.Set                                                     (empty,
                                                                               fromList)
import           Debug.Trace
import           Language.Haskell.Exts
import qualified MetaHS.DataModel.MetaModel                                   as MetaModel
import           MetaHS.DataModel.Utils
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.SrcLoc          as SrcLoc
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Decl     as Decl
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.DeclHead as DeclHead
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Module   as Module
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.QName    as QName

-- | Creates Location relations for all supported top-level declarations in the
-- provided Module.
source :: Module SrcSpanInfo  -- ^ The Module to analyze.
       -> MetaModel.Relation  -- ^ The resulting MetaModel.Relation items.
source m = case Module.name m of
  Just mn -> fromList $ mlr ++ mlhr ++ mli ++ mle ++ dlr
    where
      mlr = locationModule mn m                                               -- mlr = Module-Location Relation
      mlhr = locationModuleHead mn m                                          -- mlhr = ModuleHead-Location Relation
      mli = concat [locationImportDecl mn d | d <- Module.imports m]          -- mli = ImportDecl-Location Relation
      mle = concat [locationExportSpec mn d | d <- Module.getModuleExports m] -- mle = ExportSpec-Location Relation
      dlr = concat [locationDecl mn d | d <- Module.declarations m]           -- dlr = Decl-Location Relation
  Nothing -> empty

-- | Creates a Module "m",Location "ml" pair for the whole module
locationModule :: String                                -- ^ The module (head) name.
               -> Module SrcSpanInfo                    -- ^ The Declaration with var l.
               -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module "mn",Location) pairs
locationModule mn (Module l _ _ _ _) = [(m,ml)]
  where m =  MetaModel.Module mn
        ml = SrcLoc.srcSpanInfoToLocationElement l
locationModule _ _ = []

-- | Creates a list of (Module head "mh",Location "mhl") pair for the ModuleHead
locationModuleHead :: String                                  -- ^ The module (head) name.
                   -> Module SrcSpanInfo                      -- ^ The Declaration with var l.
                   -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Module head "mh",Location "mhl") pairs
locationModuleHead mn (Module _ (Just (ModuleHead l _ _ _)) _ _ _) = [(mh,mhl)]
                       where mh =  MetaModel.ModuleHead $ makeQualifiedId mn mn
                             mhl = SrcLoc.srcSpanInfoToLocationElement l
locationModuleHead _ _ = []

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
locationDecl mn td@TypeDecl{}  = locationType mn td
locationDecl mn dd@DataDecl{}  = locationData mn dd
locationDecl mn dd@GDataDecl{} = locationData mn dd
locationDecl mn pb@PatBind{}   = locationPattern mn pb
locationDecl mn fb@FunBind{}   = locationFunction mn fb
locationDecl mn ts@TypeSig{}   = locationTypeSig mn ts
locationDecl mn tc@ClassDecl{} = locationTypeClass mn tc
locationDecl mn id@InstDecl{}  = locationInstance mn id
locationDecl _ _               = []

-- | Creates a list of (TypeSynonym qname "ts", Location "dl") pairs for (g)DataDecl declarations.
locationType :: String                                  -- ^ The module name.
             -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
             -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (TypeSynonym qname "ts", Location "dl") pairs.
locationType mn decl = case decl of
 (TypeDecl l h _) -> [(ts,tsl)]
    where
      ts = MetaModel.TypeSynonym $ makeQualifiedId mn $ DeclHead.name h
      tsl = SrcLoc.srcSpanInfoToLocationElement l
 _ -> []

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

-- | Creates a list of (TypeSignature "ts",Location "tsl") pairs for TypeSignature declarations.
locationTypeSig :: String                                  -- ^ The module name.
                -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
                -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (TypeSignature qname, "ts",Location "tsl") pairs.
locationTypeSig mn tysig@TypeSig{} = case Decl.typeSigName tysig of
  Just tsn -> [(ts,tsl)]
    where ts = MetaModel.TypeSignature $ makeQualifiedId mn tsn
          tsl = SrcLoc.srcSpanInfoToLocationElement $ ann tysig
  Nothing -> []

-- | Creates a list of (TypeClass "tc",Location "tcl") pairs for TypeClass declarations.
locationTypeClass :: String                                  -- ^ The module name.
                  -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
                  -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (TypeClass qname, "tc",Location "tcl") pairs.
locationTypeClass mn tycla@ClassDecl{} = case Decl.typeClassName tycla of
  Just tcn -> [(tc,tcl)]
    where tc = MetaModel.TypeClass $ makeQualifiedId mn tcn
          tcl = SrcLoc.srcSpanInfoToLocationElement $ ann tycla
  Nothing -> []

-- | Creates a list of (Instance "i",Location "il") pairs for Instance declarations.
locationInstance :: String                                   -- ^ The module name.
                  -> Decl SrcSpanInfo                        -- ^ The Declaration with var l.
                  -> [(MetaModel.Element,MetaModel.Element)] -- ^ list of (Instance qname, "i",Location "il") pairs.
locationInstance mn inst@InstDecl{} = case Decl.instanceName inst of
  Just inm -> [(i,il)]
    where i = MetaModel.Instance $ makeQualifiedId mn inm
          il = SrcLoc.srcSpanInfoToLocationElement $ ann inst
  Nothing -> []
