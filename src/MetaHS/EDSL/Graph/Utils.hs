{-|
Module      : MetaHS.EDSL.Graph.Utils
Description : Utilities for handling graphs
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Utilities for handling graphs
-}
module MetaHS.EDSL.Graph.Utils
    ( graphToImage
    ) where

import Data.Text.Lazy (Text)
import Control.Exception (try,SomeException)
import System.IO (hPutStrLn,stderr)
import Data.Graph.Inductive (Node)
import Data.GraphViz
import Data.GraphViz.Printing (renderDot)
import MetaHS.EDSL.Graph.Types


-- | Generates an image from a graph and writes this to a file in the specified
--   format using the specified Graphviz layouter.
graphToImage :: GraphvizCommand -- ^ The Graphviz layouter to use.
             -> GraphvizOutput  -- ^ The output format.
             -> FilePath        -- ^ The FilePath to use for the generated image.
             -> GraphType       -- ^ The graph to render.
             -> ParamsType      -- ^ The graph parameters to use.
             -> IO FilePath     -- ^ The FilePath used for the generated image.
graphToImage cmd outp path graph params =
    writeImage cmd outp path $ graphToDotGraph graph params


-- | Converts a graph and parameters to a DotGraph Node.
graphToDotGraph :: GraphType      -- ^ The graph to convert.
                -> ParamsType     -- ^ The graph parameters to use.
                -> DotGraph Node  -- ^ The resulting DotGraph Node.
graphToDotGraph graph params = adjustStrict $ graphToDot params graph


-- | Generates an image from a DotGraph Node and writes this to a file in the
--   specified format using the specified Graphviz layouter.
writeImage :: GraphvizCommand -- ^ The Graphviz layouter to use.
           -> GraphvizOutput  -- ^ The output format.
           -> FilePath        -- ^ The FilePath to use for the generated image.
           -> DotGraph Node   -- ^ The graph to generate.
           -> IO FilePath     -- ^ The FilePath used for the generated image.
writeImage cmd outp path dg = do
  result <- try (runGraphvizCommand cmd dg' outp path)
                 :: IO (Either SomeException FilePath)
  case result of
      Left _ -> do
          hPutStrLn stderr $ "Failed to write image " ++ path ++ "."
          return ""
      Right _ -> return path
    where
      dg' :: DotGraph Text
      dg' = parseDotGraph . renderDot . toDot $ adjustStrict dg


-- | Adjusts the DOT strict setting for a DotGraph Node. This option will be
--   enabled if the graph is undirected.
adjustStrict :: DotGraph Node -> DotGraph Node
adjustStrict a = setStrictness (not $ graphIsDirected a) a
