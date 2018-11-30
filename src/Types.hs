{-|
Module      : Types
Description : Types used by the Test module.
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Types used by the Test module.
-}
module Types
    where

import System.FilePath.Posix ((</>), joinPath, splitPath)
import Data.GraphViz.Attributes.Complete (GraphvizCommand(..))
import MetaHS.EDSL (Directed)
import MetaHS.EDSL.Graph (GraphType, ParamsType)


-- | Contains the LCOM information for a Module Element.
data LcomInfo = LcomInfo
    { moduleName        :: String
    -- ^ The name of the Module.
    , usesGraph         :: (GraphType, ParamsType)
    -- ^ The generated internalUsesGraph.
    , lcomValue         :: Int
    -- ^ The calculated LCOM value.
    , graphImages       :: [(String, FilePath)]
    -- ^ The corresponding graph images generated. Each item consist of the display name and the filePath for an image. Both items will be empty strings if the generation failed.
    }


-- | The settings required for the Test file.
data Settings = Settings
    { programName        :: String
    -- ^ The program name.
    , sourceSubDirectory :: FilePath
    -- ^ The relative location of the sources in projects/programName/ (e.g. "src").
    , isGraphDirected    :: Directed
    -- ^ Determines whether the LCOM graph should be Directed or Undirected.
    , graphvizCommands   :: [GraphvizCommand]
    -- ^ The GraphvizCommands to run in order to generate graph images.
    }


-- | Returns the project directory.
projectDirectory :: FilePath  -- ^ The FilePath for the projects directory.
projectDirectory  = "src/projects"


-- | Returns the input directory for the program based on the provided Settings.
inputDirectory :: Settings  -- ^ Test settings.
                 -> FilePath  -- ^ The FilePath for the projects directory.
inputDirectory settings =
    projectDirectory </> "input" </> programName settings


-- | Determines the FilePath for the sources directory.
sourcesDirectory :: Settings  -- ^ Test settings.
                 -> FilePath  -- ^ The FilePath for the sources sub-directory of the projects directory.
sourcesDirectory settings =
    inputDirectory settings </> sourceSubDirectory settings


-- | Determines the FilePath for the graphs output directory.
outputDirectory :: Settings -- ^ Test settings.
                -> FilePath -- ^ The FilePath for the output directory.
outputDirectory settings =
    projectDirectory </> "output" </> programName settings


-- | Determines the FilePath for the MetaModel file.
metaModelFile :: Settings -- ^ Test settings.
              -> FilePath -- ^ The FilePath for the MetaModel file.
metaModelFile settings =
    outputDirectory settings </> "metaModel.txt"


-- | Determines the FilePath for the pretty printed MetaModel file.
metaModelPrettyFile :: Settings -- ^ Test settings.
                    -> FilePath -- ^ The FilePath for the pretty printed MetaModel file.
metaModelPrettyFile settings =
    outputDirectory settings </> "metaModel_pretty.txt"


-- | Determines the FilePath for the parse error file.
parseErrorsFile :: Settings -- ^ Test settings.
                -> FilePath -- ^ The FilePath for the parse errors file.
parseErrorsFile settings =
    outputDirectory settings </> "parseErrors.txt"


-- | Determines the FilePath for the graphs output directory.
graphsDirectory :: Settings -- ^ Test settings.
                -> FilePath -- ^ The FilePath for the graphs output directory.
graphsDirectory settings =
    outputDirectory settings </> "graphs"


-- | Determines the display name and the FilePath for the graphs output
--   directory specific for the specified GraphvizCommand.
graphsOutputDirectory :: Settings           -- ^ Test settings.
                      -> GraphvizCommand    -- ^ The GraphvizCommand
                      -> (String, FilePath) -- ^ The FilePath for the dot graphs directory.
graphsOutputDirectory settings command = case command of
    Dot       -> ("Dot",       graphsDirectory settings </> "dot")
    Neato     -> ("Neato",     graphsDirectory settings </> "neato")
    TwoPi     -> ("TwoPi",     graphsDirectory settings </> "twopi")
    Circo     -> ("Circo",     graphsDirectory settings </> "circo")
    Fdp       -> ("Fdp",       graphsDirectory settings </> "fdp")
    Sfdp      -> ("Sfdp",      graphsDirectory settings </> "sfdp")
    Osage     -> ("Osage",     graphsDirectory settings </> "osage")
    Patchwork -> ("Patchwork", graphsDirectory settings </> "patchwork")

-- | Determines the FilePath for the HTML report file.
reportFile :: Settings  -- ^ Test settings.
           -> FilePath  -- ^ The FilePath for the HTML report file.
reportFile settings =
    projectDirectory </> "report" </> (programName settings ++ ".html")


-- | Corrects graphs link in the HTML report.
correctGraphLink :: FilePath  -- ^ The uncorrected FilePath for graph links in the HTML report.
                 -> FilePath  -- ^ The correct FilePath for graph links in the HTML report.
correctGraphLink  p = "../" ++ joinPath (drop 1 $ splitPath p)
