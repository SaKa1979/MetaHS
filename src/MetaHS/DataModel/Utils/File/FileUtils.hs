{-|
Module      : MetaHS.DataModel.Utils.File.FileUtils
Description : Utility functions for files and directories.
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Utility functions for files and directories.
-}
module MetaHS.DataModel.Utils.File.FileUtils
    ( filesInHierarchy
    , modulesInHierarchy
    ) where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List (partition)
import Language.Haskell.Exts
import Debug.Trace
-- | Flattens the hierarchy of a directory to a list of files.
filesInHierarchy :: FilePath      -- ^ The directory hierarchy to analyze.
                 -> IO [FilePath] -- ^ The files found in the directory hierarchy.
filesInHierarchy dir = do
    rawDirList <- listDirectory dir
    let dirList = map (dir </>) $ rawDirList
    files <- filterM doesFileExist dirList
    let pureHaskellModuleFiles = filter (\p -> takeExtension p == ".hs") dirList
    subDirs <- filterM doesDirectoryExist dirList
    subDirFiles <- mapM filesInHierarchy subDirs
    return $ pureHaskellModuleFiles ++ concat subDirFiles

-- | Attempts to extract modules for each file in the directory hierarchy
--   The returned tuple contains the successfully extracted modules and the
--   ParseResult information for those files where this extraction failed.
modulesInHierarchy :: FilePath                                                      -- ^ The directory hierarchy to analyze.
                   -> IO ([Module SrcSpanInfo], [ParseResult (Module SrcSpanInfo)]) -- ^ The successfully and unsuccessfully  parsed Module .
modulesInHierarchy directoryPath = do
    allFiles <- filesInHierarchy directoryPath
    parseResults <- mapM parseFile allFiles
    let (parsed, failed) = partition isParseOk parseResults
--    let modules = trace ("" ++ show (map fromParseResult parsed)) (map fromParseResult parsed)
    let modules = map fromParseResult parsed
    return (modules, failed)
      where
        isParseOk :: ParseResult (Module SrcSpanInfo) -> Bool
        isParseOk ParseOk{}     = True
        isParseOk ParseFailed{} = False