{-|
Module      : MetaHS.DataModel.Extractor.Program.Contains
Description : The MetaHS extractor for contains relations
License     : <to-be-determined>
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

MetaHS extractor for application level contains relations
-}
module MetaHS.DataModel.Extractor.Program.Contains
    ( contains
    ) where

import Data.Maybe (fromMaybe)
import Data.Set (fromList)
import Language.Haskell.Exts
import qualified MetaHS.DataModel.MetaModel as MetaModel
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Module
    as Module

-- | Creates MetaModel.Relation set for the modules contained within the
-- program.
contains :: String                -- ^ The program name.
         -> [Module SrcSpanInfo]  -- ^ List of parsed Modules.
         -> MetaModel.Relation    -- ^ The resulting relation.
contains progName modules =
    fromList [createRelationPair progName m | m <- modules]

-- | Creates a (Program "pn",Module "m") pair.
createRelationPair :: String                                -- ^ The program name.
                   -> Module SrcSpanInfo                    -- ^ The parsed Module.
                   -> (MetaModel.Element,MetaModel.Element) -- ^ The resulting (Element,Element) pair.
createRelationPair pn m = (mmProg,mmM)
  where
    mmProg = MetaModel.Program pn
    mmM    = MetaModel.Module mn
    mn     = fromMaybe "?" (Module.name m)
