{-|
Module      : MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Name
Description : Utility functions for Name objects.
License     : <to-be-determined>
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Utility functions for Name objects.
-}
module MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Name
    where

import Language.Haskell.Exts.Syntax

-- | Returns the name for a Name object.
name :: Name l  -- ^ The Name object to analyze.
     -> String  -- ^ The name of the Name object.
name (Ident _ x)  = x
name (Symbol _ x) = "(" ++ x ++ ")"
