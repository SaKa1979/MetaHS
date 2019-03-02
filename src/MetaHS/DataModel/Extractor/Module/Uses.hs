{-|
Module      : MetaHS.DataModel.Extractor.Module.Uses
Description : The MetaHS extractor for uses relations
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

MetaHS extractor for module level uses relations
-}
module MetaHS.DataModel.Extractor.Module.Uses
    ( uses
    ) where

import Data.Maybe( fromMaybe )
import Data.List (nub, (\\))
import Data.Set (fromList,empty)
import Language.Haskell.Exts
import qualified MetaHS.DataModel.MetaModel as MetaModel
import MetaHS.DataModel.Utils
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Module
    as Module
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Decl
    as Decl
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Name
    as Name
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.DeclHead
    as DeclHead


-- | Create MetaModel.Contains relations for top-level declarations of a module.
uses :: Module SrcSpanInfo  -- ^ The module to analyze
     -> MetaModel.Relation  -- ^ list of Element `Contains` Element
     -> MetaModel.Relation  -- ^ list of Element `Uses` Element
uses m rs = case Module.name m of
  Just mn -> fromList $ concat [usesDecl mn d nrms | d <- Module.declarations m]
    where
      nrms = createNameResolutionMaps mn rs         -- nrms = name resolution maps
  Nothing -> empty

-- | Analyzes a declaration for `Uses` information.
usesDecl :: String                                  -- ^ The name of the Module.
         -> Decl SrcSpanInfo                        -- ^ The declaration to analyze.
         -> NameResolutionMaps                      -- ^ The name resolution maps.
         -> [(MetaModel.Element,MetaModel.Element)] -- ^ The resulting list of (Element,Element) pairs.
usesDecl mn td@TypeDecl{} nrms  = usesType mn td nrms
usesDecl mn ts@TypeSig{} nrms   = usesTypeSig mn ts nrms
usesDecl mn dd@DataDecl{} nrms  = usesData mn dd nrms
usesDecl mn dd@GDataDecl{} nrms = usesData mn dd nrms
usesDecl mn pb@PatBind{} nrms  = usesPattern mn pb nrms
usesDecl mn fb@FunBind{} nrms   = usesFunction mn fb nrms
usesDecl _ _ _ = []


-- | Analyzes a type synonym declaration for `Uses` information.
usesType :: String                                  -- ^ The name of the Module.
         -> Decl SrcSpanInfo                        -- ^ The type synonym declaration to analyze.
         -> NameResolutionMaps                      -- ^ The name resolution maps.
         -> [(MetaModel.Element,MetaModel.Element)] -- ^ The resulting list of (Element,Element) pairs.
usesType mn d nrms = case d of
    (TypeDecl _ h t) -> rs
      where
        p = MetaModel.TypeSynonym $ makeQualifiedId mn $ DeclHead.name h        -- p = parent
        es = [resolveType tcns nrms | tcns <- findTyConNames t]                 -- es = elements, tcns = TyCon names
        rs = [(p,c) | c <- es]                                                  -- p = parent, c = child, rs = relations
    _ -> []


-- | Analyzes a type signature for `Uses` information.
usesTypeSig :: String                                   -- ^ The name of the Module.
            -> Decl SrcSpanInfo                         -- ^ The type signature to analyze.
            -> NameResolutionMaps                       -- ^ The name resolution maps.
            -> [(MetaModel.Element,MetaModel.Element)]  -- ^ The resulting list of (Element,Element) pairs.
usesTypeSig _ d nrms = case d of
    (TypeSig _ ns t) -> rs
      where
        ps = [resolveValue (Name.name n) nrms | n <- ns]                        -- ps = parents
        es = [resolveType tcns nrms | tcns <- findTyConNames t]                 -- es = elements, tcns = TyCon names
        rs = [(p,c) | p <- ps, c <- es]                                         -- p = parent, c = child, rs = relations
    _ -> []


-- | Analyzes a data declaration for `Uses` information.
usesData :: String                                  -- ^ The name of the Module.
         -> Decl SrcSpanInfo                        -- ^ The data declaration to analyze.
         -> NameResolutionMaps                      -- ^ The name resolution maps.
         -> [(MetaModel.Element,MetaModel.Element)] -- ^ The resulting list of (Element,Element) pairs.
usesData mn d nrms = case Decl.dataConstructor d of
    Just a -> udc a
      where
        dcn = Decl.dataConstructorName a                                        -- dcn  = data constructor name
        dce = MetaModel.DataType $ makeQualifiedId mn dcn                       -- dce  = data constructor element

        -- Creates `Uses` relation for the data constructor.
        udc :: Decl.DataConstructor -> [(MetaModel.Element,MetaModel.Element)]  -- udc = uses data constructor
        udc dc = uvcs
          where
            vcs  = Decl.valueConstructors dc                                    -- vcs  = value constructors
            uvcs = concat [uvc vc | vc <- vcs]                                  -- uvcs = `Uses` relation for sub value constructors

        -- Creates `Uses` relation for the value constructor.
        uvc :: Decl.ValueConstructor-> [(MetaModel.Element,MetaModel.Element)]  -- uvc = uses value constructor
        uvc vc = vcud : (vcrs ++ frs)
          where
            vcn = Decl.valueConstructorName vc                                  -- vcn = value constructor name
            vce = MetaModel.Function $ makeQualifiedId mn vcn                                -- vce = value constructor element
            vcud = (vce,dce)                                                    -- Value Constructor `Uses` DataType

            vcts = Decl.valueConstructorTypes vc                                -- vcts = value Constructor Types
            vces = [resolveType tcns nrms | tcns <- findTyConNames vcts]        -- vces = value Constructor elements, tcns = TyCon names
            vcrs = [(vce,c) | c <- vces]                                        -- c = child, rs = relations

            frs = concat [uf vce f | f <- Decl.valueConstructorFields vc]       -- frs = `Uses` relation for sub fields (if present)

        -- Creates `Uses` relation for the supplied field
        uf :: MetaModel.Element
           -> Decl.Field
           -> [(MetaModel.Element,MetaModel.Element)]
        uf vce f = rs
          where
            ps = vce : [resolveValue n nrms | n <- Decl.fieldNames f]           -- ps = parents
            es = dce : [resolveType tcns nrms | tcns <- findTyConNames fts]     -- es = elements, tcns = TyCon names
            rs = [(p,c) | p <- ps, c <- es]                                     -- p = parent, c = child, rs = relations
            fts = Decl.fieldTypes f                                             -- fts = field Types
    Nothing -> []


-- | Analyzes a PatBind declaration for `Uses` information.
usesPattern :: String                                   -- ^ The name of the Module.
            -> Decl SrcSpanInfo                         -- ^ The function declaration to analyze.
            -> NameResolutionMaps                       -- ^ The name resolution maps.
            -> [(MetaModel.Element,MetaModel.Element)]  -- ^ The resulting list of (Element,Element) pairs.
usesPattern mn pb@(PatBind _ (PVar _ _) rhs wheres) nrms = trs ++ vrs
  where
    pn = fromMaybe "" $ Decl.patternName pb                                     -- pn = pattern name
    p = MetaModel.Function $ makeQualifiedId mn pn                              -- p = parent
    tycons = findTyConNames rhs ++ findTyConNames wheres                        -- tycons = TyCon objects found
    qnames = findQNames rhs ++ findQNames wheres                                -- qnames = QName objects
    locals = findPatVars rhs ++ findPatVars wheres ++                           -- locals = local definitions
             findFunctionNames rhs ++ findFunctionNames wheres

    exts = nub (qnames \\ tycons) \\ locals                                     -- exts = external names found
    trs = [(p,resolveType c nrms) | c <- tycons]                                -- trs = types relations
    vrs = [(p,resolveValue c nrms) | c <- exts]                                 -- trs = values relations
usesPattern _ _ _ = []


-- | Analyzes a PatBind declaration for `Uses` information.
usesFunction :: String                                  -- ^ The name of the Module.
             -> Decl SrcSpanInfo                        -- ^ The function declaration to analyze.
             -> NameResolutionMaps                      -- ^ The name resolution maps.
             -> [(MetaModel.Element,MetaModel.Element)] -- ^ The resulting list of (Element,Element) pairs.
usesFunction mn fb@(FunBind _ matches) nrms = trs ++ vrs
  where
    fn = fromMaybe "" $ Decl.functionName fb                                    -- fn = function name
    p = MetaModel.Function $ makeQualifiedId mn fn                              -- p = parent
    tycons = findTyConNames matches                                             -- tycons = TyCon objects found
    qnames = findQNames matches                                                 -- qnames = QName objects
    locals = findPatVars matches ++ (nub (findFunctionNames matches) \\ [fn])   -- locals = local definitions

    exts = nub (qnames \\ tycons) \\ locals                      -- exts = external names found
    trs = [(p,resolveType c nrms) | c <- tycons]                 -- trs = types relations
    vrs = [(p,resolveValue c nrms) | c <- exts]                  -- trs = values relations
usesFunction _ _ _ = []
