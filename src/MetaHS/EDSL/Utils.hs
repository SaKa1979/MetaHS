{-|
Module      : MetaHS.EDSL.Utils
Description : The MetaHS EDSL Utils part
License     : <to be determined>
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

MetaHS EDSL Utils part
-}
module MetaHS.EDSL.Utils
    ( split
    , isLocal
    , locationToQuery
    ) where

import System.FilePath.Posix (joinPath, splitPath)
import MetaHS.DataModel.Utils (split, isLocal)
import qualified MetaHS.DataModel.MetaModel as MetaModel


-- | Converts a Location Element to a query String suitable for URLs.
--   An empty String is returned if the Element is not a Location Element.
locationToQuery :: MetaModel.Element  -- ^ The Location Element.
                -> String             -- ^ The resulting URL query String or an empty String.
locationToQuery loc@MetaModel.Location{} = concat
    ["?path="           , joinPath . drop 1 . splitPath $ MetaModel.locationPath loc
    ,"&amp;startLine="  , show $ MetaModel.locationStartLine loc
    ,"&amp;startColumn=", show $ MetaModel.locationStartColumn loc
    ,"&amp;endLine="    , show $ MetaModel.locationEndLine loc
    ,"&amp;endColumn="  , show $ MetaModel.locationEndColumn loc
    ]
locationToQuery _ = ""
