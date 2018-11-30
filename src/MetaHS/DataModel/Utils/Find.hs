{-|
Module      : MetaHS.DataModel.Utils.Find
Description : Generic SYB functions for finding certain nodes
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Generic SYB functions for finding certain nodes
-}
module MetaHS.DataModel.Utils.Find
    ( findTyConNames
    , findQNames
    , findPatVars
    , findFunctionNames
    ) where

import Data.List ((\\))
import Data.Generics( Data, everything, mkQ )
import Language.Haskell.Exts
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Decl
    as Decl
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Name
    as Name
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.QName
    as QName


-- | SYB function that searches for TyCon nodes and returns their names.
findTyConNames :: Data a => a -- ^ The data structure to search.
               -> [String]    -- ^ The names of the TyCon nodes found.
findTyConNames = everything (++) ([] `mkQ` f)
  where
    f :: Type SrcSpanInfo -> [String]
    f (TyCon _ n) = case QName.name n of
        Just s -> [s]
        Nothing -> []
    f _ = []


-- | SYB function that searches for QName nodes (without TyCons) and returns
--   their names.
findQNames :: Data a => a -- ^ The data structure to search.
           -> [String]    -- ^ The names of the QNames nodes found.
findQNames d = qnames \\ tyconNames
  where
    qnames = everything (++) ([] `mkQ` f) d
    tyconNames = findTyConNames d
    f :: QName SrcSpanInfo -> [String]
    f x = case QName.name x of
        Just a -> [a]
        Nothing -> []


-- | SYB function that searches for Pat nodes that represent variables and
--   returns their names.
findPatVars :: Data a => a -- ^ The data structure to search.
            -> [String]    -- ^ The names of the Pat variables found.
findPatVars = everything (++) ([] `mkQ` f)
  where
    f :: Pat SrcSpanInfo -> [String]
    f (PVar _ x)     = [Name.name x]
    f (PAsPat _ x _) = [Name.name x]
    f _              = []


-- | SYB function that searches for functions and returns their names.
findFunctionNames :: Data a => a -- ^ The data structure to search.
                  -> [String]    -- ^ The function names found.
findFunctionNames = everything (++) ([] `mkQ` f)
  where
    f :: Decl SrcSpanInfo -> [String]
    f fb@FunBind{} = case Decl.functionName fb of
        Just fn -> [fn]
        Nothing -> []
    f _ = []
