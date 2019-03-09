{-|
Module      : MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.QName
Description : Utility functions for ConDecl objects.
License     : <to-be-determined>
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Utility functions for ConDecl objects.
-}
module MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.ConDecl
    where

import Language.Haskell.Exts.Syntax
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Name as Name

-- | Returns the name for a ConDecl object.
name :: ConDecl l -- ^ The ConDecl object to analyze.
     -> String    -- ^ The name of the ConDecl object.
name (ConDecl _ x _)        = Name.name x
name (InfixConDecl _ _ x _) = Name.name x
name (RecDecl _ x _)        = Name.name x
