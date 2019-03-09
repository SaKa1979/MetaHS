{-|
Module      : MetaHS.DataModel.Utils.Name
Description : Utility functions for the MetaHS data model layer.
License     : <to-be-determined>
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Name utility functions for the MetaHS data model layer.
-}
module MetaHS.DataModel.Utils.Name
    ( makeQualifiedId
    , split
    , isLocal
    ) where

import Data.List (intercalate, isSuffixOf, elemIndex)
import Data.List.Split (splitOn)

-- Qualify identifier
makeQualifiedId :: String  -- ^ The qualifier String (e.g. "MetaHS.DataModel.Extractor.Module.Source").
   -> String  -- ^ The local identifier String (e.g. "qn").
   -> String  -- ^ The resulting qualified String (e.g. "MetaHS.DataModel.Extractor.Module.Source.qn").
makeQualifiedId a b = a ++ "." ++ b


-- | Splits an indentifier in the qualifier and name parts.
split :: String           -- ^ The qualified String to split.
      -> (String, String) -- ^ The qualified and name parts.
split qualName
    | isSymbol      = symbolSplit                                               -- qualName contains a symbol as identified by the presence of an '(' character.
    | unqualified   = ("",qualName)                                             -- qualName does not contain dots, therefore it is regarded as an unqualified name
    | otherwise     = (intercalate "." $ init dotParts, last dotParts)          -- split qualName over the qualifier and the unqualified name
  where
    isSymbol      = '(' `elem` qualName                                         -- isSymbol is True is qualName contains an '(' character.
    symbolSplit   = case '(' `elemIndex` qualName of                            -- split function suitable for symbols.
        Just n -> do
            let (a,b) = splitAt n qualName
            if "." `isSuffixOf` a
            then (init a, b)
            else (a,b)
        Nothing -> ("","")
    dotParts      = splitOn "." qualName                                        -- dotParts = splitted parts of qualName
    unqualified   = 1 == length dotParts                                        -- unqualified is True is qualName does not contain dots



-- | Determines whether a given qualified name is local to the given qualifier.
isLocal :: String -- ^ The qualifier.
        -> String -- ^ The qualified name.
        -> Bool   -- ^ True if the qualified name is local to the qualifier.
isLocal q qualName = (q ==) . fst $ split qualName
