{-|
Module      : MetaHS.DataModel.Utils.Language.Haskell.Exts.SrcLoc
Description : Utility functions for SrcLoc and related objects.
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Utility functions for SrcLoc and related objects.
-}
module MetaHS.DataModel.Utils.Language.Haskell.Exts.SrcLoc
    where

import Language.Haskell.Exts.SrcLoc
import MetaHS.DataModel.MetaModel

-- | Creates a Location Element containing the information
--   of the SrcLoc object.
srcLocToLocationElement :: SrcLoc   -- ^ The SrcLoc object to analyze.
                        -> Element  -- ^ The resulting Element.
srcLocToLocationElement s = Location
    { locationPath        = srcFilename s
    , locationStartLine   = srcLine s
    , locationStartColumn = srcColumn s
    , locationEndLine     = srcLine s
    , locationEndColumn   = srcColumn s
    }

-- | Creates a Location Element containing the information
--   of the SrcSoan object.
srcSpanToLocationElement :: SrcSpan -- ^ The SrcSpan object to analyze.
                         -> Element -- ^ The resulting Element.
srcSpanToLocationElement s = Location
    { locationPath        = srcSpanFilename s
    , locationStartLine   = srcSpanStartLine s
    , locationStartColumn = srcSpanStartColumn s
    , locationEndLine     = srcSpanEndLine s
    , locationEndColumn   = srcSpanEndColumn s
    }

-- | Creates a Location Element containing the information
--   of the SrcSpanInfo object.
srcSpanInfoToLocationElement :: SrcSpanInfo -- ^ The SrcSpanInfo object to analyze.
                             -> Element     -- ^ The resulting Element.
srcSpanInfoToLocationElement = srcSpanToLocationElement .srcInfoSpan
