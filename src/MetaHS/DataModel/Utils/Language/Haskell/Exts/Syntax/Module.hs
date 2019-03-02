{-|
Module      : MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Module
Description : Utility functions for Modules objects.
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Utility functions for Modules objects.
-}
module MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Module
    where

import Prelude hiding (head)
import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)

-- | Returns the location information of a Module.
srcSpanInfo :: Module SrcSpanInfo -- ^ The Module to analyze.
            -> SrcSpanInfo        -- ^ The SrcSpanInfo for this Module.
srcSpanInfo = ann

-- | Returns the ModuleHead of a Module.
head :: Module l              -- ^ The Module to analyze.
     -> Maybe (ModuleHead l)  -- ^ The ModuleHead for this Module.
head (Module _ x _ _ _)            = x
head (XmlHybrid _ x _ _ _ _ _ _ _) = x
head _                             = Nothing

-- | Returns the name of the module contained in the ModuleHead.
headName :: ModuleHead l  -- ^ The ModuleHead to analyze.
         -> String        -- ^ The name of this ModuleHead.
headName (ModuleHead _ (ModuleName _ x) _ _) = x

-- | Returns the WarningText contained in the ModuleHead
headWarningText :: ModuleHead l           -- ^ The ModuleHead to analyze.
                -> Maybe (WarningText l)  -- ^ The WarningText for this ModuleHead.
headWarningText (ModuleHead _ _ x _) = x

-- | Return the ExportSpecList contained in the ModuleHead.
headExports :: ModuleHead l             -- ^ The ModuleHead to analyze.
            -> Maybe (ExportSpecList l) -- ^ The ExportSpecList for this ModuleHead.
headExports (ModuleHead _ _ _ x) = x

-- | Returns the name of a Module.
name :: Module l      -- ^ The Module to analyze.
     -> Maybe String  -- ^ The Name of this Module.
name m = headName <$> head m

-- | Returns the WarningText of a Module.
warningText :: Module l               -- ^ The Module to analyze.
            -> Maybe (WarningText l)  -- ^ The WarningText for this Module.
warningText m = f $ headWarningText <$> head m
  where
    f = fromMaybe Nothing

-- | Returns the ExportSpecList of a Module.
exports :: Module l                 -- ^ The Module to analyze.
        -> Maybe (ExportSpecList l) -- ^ The ExportSpecList for this Module.
exports m = f $ headExports <$> head m
  where
    f = fromMaybe Nothing

-- | Returns the pragmas of a module.
pragmas :: Module l         -- ^ The Module to analyze.
        -> [ModulePragma l] -- ^ The list of ModulePragma objects for this Module.
pragmas (Module _ _ x _ _)            = x
pragmas (XmlHybrid _ _ x _ _ _ _ _ _) = x
pragmas _                             = []

-- | Returns the imports of a module
imports :: Module l       -- ^ The Module to analyze.
        -> [ImportDecl l] -- ^ The list of ImportDecl objects for this Module.
imports (Module _ _ _ x _)            = x
imports (XmlHybrid _ _ _ x _ _ _ _ _) = x
imports _                             = []

-- | Returns the Decl of a module
declarations :: Module l  -- ^ The Module to analyze.
             -> [Decl l]  -- ^ The list of Decl objects for this Module.
declarations (Module _ _ _ _ x)            = x
declarations (XmlHybrid _ _ _ _ x _ _ _ _) = x
declarations _                             = []