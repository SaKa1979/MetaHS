{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Report
Description : Report module for generating the HTML LCOM report.
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Report module for generating the HTML LCOM report.
-}
module Report
    ( generateReport
    , writeReport
    ) where

import Prelude (($), String, Int, FilePath, IO, writeFile, show)
import Control.Monad (mapM_)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Types


-- | Generates the report.
generateReport :: String      -- ^ The name of the project.
               -> FilePath    -- ^ The link to the CSS stylesheet.
               -> Int         -- ^ The number of graps (this determines the width of the table header).
               -> [LcomInfo]  -- ^ The LcomInfo list.
               -> Html        -- ^ The HTML report.
generateReport projectName cssHref nrOfGraphs lcomInfos = docTypeHtml $ do
    head $ do
        title $ toHtml projectName
        link ! A.rel "stylesheet" ! A.href (stringValue cssHref)
    body $ do
        h1 $ toHtml projectName
        table $ do
            tr $ do
                th "Module:"
                th "LCOM:"
                th ! A.colspan cs $ "Graphs:"
            rows lcomInfos
      where
        cs = stringValue $ show nrOfGraphs


-- | Generates rows for the LcomInfo list.
rows :: [LcomInfo]  -- ^ The LcomInfo list.
     -> Html        -- ^ The HTML table rows.
rows = mapM_ f
  where
    f :: LcomInfo -> Html
    f lcomInfo = tr $ do
        td $ toHtml $ moduleName lcomInfo
        td $ toHtml $ lcomValue lcomInfo
        mapM_ g $ graphImages lcomInfo

    g :: (String, FilePath) -> Html
    g ("",  _)   = td ""
    g ( _, "")   = td ""
    g (dn, path) = td $ a ! A.href cp $ cn
      where
        cp = stringValue $ correctGraphLink path
        cn = toHtml dn


-- | Writes a report to a file.
writeReport :: FilePath -- ^ The FilePath to write to.
            -> Html     -- ^ The HTML report to write.
            -> IO ()    -- ^ Nothing is returned.
writeReport path report = writeFile path $ renderHtml report
