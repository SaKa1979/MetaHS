{-# LANGUAGE BangPatterns #-}

{-|
Module      : Test
Description : Main test module for LCOM.
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Main test module for LCOM.
-}
module Test
    where

import Text.Printf
import System.FilePath.Posix ((</>))
import Data.Time (getCurrentTime, diffUTCTime)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive
                        ,createDirectory, doesFileExist, removeFile)
import Control.Monad (when, forM, forM_)
import MetaHS.EDSL
import MetaHS.Extensions.LCOM
import Data.GraphViz.Commands
import qualified MetaHS.DataModel.MetaModel as MetaModel
import Types
import Report


-- | Settings for the ampersand-3.0.2 project.
settings_ampersand_3_0_2 :: Settings
settings_ampersand_3_0_2 = Settings
    { programName        = "ampersand-3.0.2"
    , sourceSubDirectory = "src"
    , isGraphDirected    = Directed
    , graphvizCommands   = [Dot, Circo]
    }


-- | Settings for the helium-1.8 project.
settings_helium_1_8 :: Settings
settings_helium_1_8 = Settings
    { programName        = "helium-1.8"
    , sourceSubDirectory = "src"
    , isGraphDirected    = Directed
    , graphvizCommands   = [Dot]
    }


-- | Settings for the ideas-1.2 project.
settings_ideas_1_2 :: Settings
settings_ideas_1_2 = Settings
    { programName        = "ideas-1.2"
    , sourceSubDirectory = "src"
    , isGraphDirected    = Directed
    , graphvizCommands   = [Dot, Circo]
    }


-- | Settings for the ideas-1.6 project.
settings_ideas_1_6 :: Settings
settings_ideas_1_6 = Settings
    { programName        = "ideas-1.6"
    , sourceSubDirectory = "src"
    , isGraphDirected    = Directed
    , graphvizCommands   = [Dot, Circo]
    }


-- | Settings for the pandoc-1.13.1 project.
settings_pandoc_1_13_1 :: Settings
settings_pandoc_1_13_1 = Settings
    { programName        = "pandoc-1.13.1"
    , sourceSubDirectory = "src"
    , isGraphDirected    = Directed
    , graphvizCommands   = [Dot, Circo]
    }


-- | Settings for the parsec-3.1.7 project.
settings_parsec_3_1_7 :: Settings
settings_parsec_3_1_7 = Settings
    { programName        = "parsec-3.1.7"
    , sourceSubDirectory = "Text"
    , isGraphDirected    = Directed
    , graphvizCommands   = [Dot, Circo]
    }


-- | Settings for the QuickCheck-2.7.6 project.
settings_QuickCheck_2_7_6 :: Settings
settings_QuickCheck_2_7_6 = Settings
    { programName        = "QuickCheck-2.7.6"
    , sourceSubDirectory = "Test"
    , isGraphDirected    = Directed
    , graphvizCommands   = [Dot, Circo]
    }


-- | Settings for the uulib-0.9.16 project.
settings_uulib_0_9_16 :: Settings
settings_uulib_0_9_16 = Settings
    { programName        = "uulib-0.9.16"
    , sourceSubDirectory = "src"
    , isGraphDirected    = Directed
    , graphvizCommands   = [Dot, Circo]
    }


-- | Settings for the metaHS project.
settings_metaHS :: Settings
settings_metaHS = Settings
    { programName        = "metaHS"
    , sourceSubDirectory = "."
    , isGraphDirected    = Directed
    , graphvizCommands   = [Dot, Circo]
    }

-- | Settings for the Example project.
settings_example :: Settings
settings_example = Settings
    { programName        = "example"
    , sourceSubDirectory = "src"
    , isGraphDirected    = Directed
    , graphvizCommands   = [Dot]
    }

-- | Cleans the output directory based on the provided Settings.
clean :: Settings -- ^ Test settings.
      -> IO ()    -- ^ Nothing is returned.
clean settings = do
    do
        let d = outputDirectory settings
        needsRemoval <- doesDirectoryExist d
        Control.Monad.when needsRemoval $ removeDirectoryRecursive d
    do
        let f = reportFile settings
        needsRemoval <- doesFileExist f
        Control.Monad.when needsRemoval $ removeFile f

    printf "test\n"

    createDirectory $ outputDirectory settings
    createDirectory $ graphsDirectory settings
    forM_ (graphvizCommands settings)
        (createDirectory . snd . graphsOutputDirectory settings)
    return ()


-- | Generates the MetaModel based on the provided Settings.
generate :: Settings                -- ^ Test settings.
         -> IO MetaModel.MetaModel  -- ^ The generated MetaModel.
generate settings = generateMetaModel pn sd pef
  where
    pn = programName settings
    sd = sourcesDirectory settings
    pef = parseErrorsFile settings


-- | Stores the MetaModel to two files based on the provided Settings.
toFile :: Settings            -- ^ Test settings.
       -> MetaModel.MetaModel -- ^ The MetaModel to write.
       -> IO ()               -- ^ Nothing is returned.
toFile settings metaModel = do
    writeMetaModel metaModel $ metaModelFile settings
    writeMetaModelPretty metaModel $ metaModelPrettyFile settings
    return ()


-- | Reads the MetaModel from a file based on the provided Settings.
fromFile :: Settings                -- ^ Test settings.
         -> IO MetaModel.MetaModel  -- ^ The MetaModel.
fromFile settings = readMetaModel $ metaModelFile settings


-- | Calculates the LCOM value for the specified Module Element and generates
--   the corresponding graph images.
generateLcom :: Settings            -- ^ Test settings.
             -> MetaModel.MetaModel -- ^ The MetaModel.
             -> MetaModel.Element   -- ^ The Module Element to analyze.
             -> IO LcomInfo         -- ^ The LCOM information and graphs for the specified Module Element.
generateLcom settings metaModel moduleElement = do
    let mn = MetaModel.name moduleElement
    let directed = isGraphDirected settings
    let editorLink = "../../../../editor.html"                                  -- editorPath is the prefix for the links in the generated SVG images to the correct HTML editor for display the source code. Should probably become a hard link if a web server is used.

    let (lv, graph, params) = lcomGraph metaModel moduleElement
                                        directed editorLink

    gs <- forM (graphvizCommands settings) ( \cmd -> do
        let (dn, dir) = graphsOutputDirectory settings cmd
        let path = dir </> mn ++ ".svg"
        imagePath <- graphToImage cmd Svg path graph params
        if not (null imagePath)
            then return (dn, imagePath)
            else return ("","")
        )

    return LcomInfo { moduleName  = mn
                    , lcomValue   = lv
                    , usesGraph   = (graph, params)
                    , graphImages = gs
                    }


-- | Generates a simple report for the calculated LCOM values and graphs.
report :: Settings    -- ^ Test settings.
       -> [LcomInfo]  -- ^ The LCOM information.
       -> IO FilePath -- ^ The FilePath of the generated report.
report settings lcomInfos = do
    let pn = programName settings
    let nrOfGraphs = length $ graphvizCommands settings
    let cssHref = "../css/main.css"
    let path = reportFile settings

    writeReport path $ generateReport pn cssHref nrOfGraphs lcomInfos
    return path



-- | Timed test run.
--   This will:
--     - clean the output directory
--     - generate the metaModel
--     - store the metaModel to two files
--     - generates the LCOM graphs and values
--     - create the report
--     - displays the recorded time information.
--     - return the path of the report and the collected LCOM information.
timedTest :: Settings                  -- ^ Test settings.
          -> IO (FilePath, [LcomInfo]) -- ^ The FilePath of the generated report and the collected LCOM information.
timedTest settings = do
    !t0 <- getCurrentTime

    clean settings

    !t1 <- getCurrentTime

    metaModelRaw <- generate settings
    let metaModel = lcomAggregator metaModelRaw

    printf "The Meta-Model contains %d items.\n" $ numberOfItems metaModel
    printf "The Meta-Model contains %d modules.\n" $ length $ modules metaModel
    printf "\n"

    !t2 <- getCurrentTime

    toFile settings metaModel

    !t3 <- getCurrentTime

    lcomInfo <- mapM (generateLcom settings metaModel) $ modules metaModel

    !t4 <- getCurrentTime

    reportPath <- report settings lcomInfo

    !t5 <- getCurrentTime

    printf "\n"
    printf "t0 = %s.\n" $ show t0
    printf "t1 = %s.\n" $ show t1
    printf "t2 = %s.\n" $ show t2
    printf "t3 = %s.\n" $ show t3
    printf "t4 = %s.\n" $ show t4
    printf "t5 = %s.\n" $ show t5
    printf "\n"
    printf "clean    (t0 -> t1) = %s.\n" $ show $ diffUTCTime t1 t0
    printf "generate (t1 -> t2) = %s.\n" $ show $ diffUTCTime t2 t1
    printf "toFile   (t2 -> t3) = %s.\n" $ show $ diffUTCTime t3 t2
    printf "lCOM     (t3 -> t4) = %s.\n" $ show $ diffUTCTime t4 t3
    printf "report   (t4 -> t5) = %s.\n" $ show $ diffUTCTime t5 t4
    printf "\n"
    printf "total    (t0 -> t5) = %s.\n" $ show $ diffUTCTime t5 t0
    printf "\n"
    printf "The LCOM report path is: %s.\n" reportPath

    return (reportPath, lcomInfo)


-- | Run all timedTest tests.
runAll :: IO ()
runAll = do
    printf "ampersand-3.0.2\n\n"
    _ <- timedTest settings_ampersand_3_0_2
    _ <- separator

    printf "helium-1.8\n\n"
    _ <- timedTest settings_helium_1_8
    _ <- separator

    printf "ideas-1.2\n\n"
    _ <- timedTest settings_ideas_1_2
    _ <- separator

    printf "ideas-1.6\n\n"
    _ <- timedTest settings_ideas_1_6
    _ <- separator

    printf "pandoc-1.13.1\n\n"
    _ <- timedTest settings_pandoc_1_13_1
    _ <- separator

    printf "parsec-3.1.7\n\n"
    _ <- timedTest settings_parsec_3_1_7
    _ <- separator

    printf "QuickCheck-2.7.6\n\n"
    _ <- timedTest settings_QuickCheck_2_7_6
    _ <- separator

    printf "uulib-0.9.16\n\n"
    _ <- timedTest settings_uulib_0_9_16
    _ <- separator

    printf "metaHS\n\n"
    _ <- timedTest settings_metaHS
    _ <- separator

    printf "example\n\n"
    _ <- timedTest settings_example
    _ <- separator

    printf "Finished\n\n"
    return ()
      where
        separator :: IO [()]
        separator = sequence [ printf "\n"
                             , printf "--------------------"
                             , printf "\n"
                             ]
