{-|
Module      : MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.QName
Description : Utility functions for QName objects.
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Utility functions for QName objects.
-}
module MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.QName
    where

import Language.Haskell.Exts.Syntax
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Name as Name

-- | Returns the name for a QName object.
name :: QName l       -- ^ The QName object to analyze.
     -> Maybe String  -- ^ The name of the QName object.
name (Qual _ (ModuleName _ m) n) = Just $ m ++ "." ++ Name.name n
name (UnQual _ n)                = Just $ Name.name n
name _                           = Nothing
