{-|
Module      : MetaHS.DataModel.Extractor.Module.Imports
Description : The MetaHS extractor for import relations.
License     : None
Maintainer  : sanderkamps79@gmail.com
Stability   : experimental

MetaHS extractor for module level import relations.
Currently only (Module,Module) relations are created from
ImportDecl-importModule.
-}
module MetaHS.DataModel.Extractor.Module.Imports
  (imports)
  where

import           Language.Haskell.Exts
import qualified MetaHS.DataModel.MetaModel as MetaModel
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Module
    as Module
import MetaHS.DataModel.Utils.Name (makeQualifiedId)
import Data.Set (empty, fromList)

-- | Creates Location relations for all supported top-level declarations in the
-- | provided Module.
imports :: Module SrcSpanInfo  -- ^ The Module to analyze.
        -> MetaModel.Relation  -- ^ The resulting MetaModel.Relation items.
imports m = case Module.name m of
  Just mn -> fromList [importsModule mn id | id <- Module.imports m]
  Nothing -> empty

importsModule::String
             -> ImportDecl SrcSpanInfo
             -> (MetaModel.Element,MetaModel.Element)
importsModule mn ImportDecl{importModule=im} = (mp,mc) where
  mp = MetaModel.Module mn
  mc = MetaModel.Module $ makeQualifiedId mn $ hn im
  hn (ModuleName _ x) = x