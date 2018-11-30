{-|
Module      : MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Name
Description : Utility functions for DeclHead objects.
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Utility functions for DeclHead objects.
-}
module MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.DeclHead
    where

import Language.Haskell.Exts.Syntax
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Name as Name

-- | Returns the name for a DeclHead object.
name :: DeclHead l  -- ^ The DeclHead object to analyze.
     -> String      -- ^ The name of the DeclHead object.
name (DHead _ x)     = Name.name x
name (DHInfix _ _ x) = Name.name x
name (DHParen _ x)   = name x
name (DHApp _ x _)   = name x
