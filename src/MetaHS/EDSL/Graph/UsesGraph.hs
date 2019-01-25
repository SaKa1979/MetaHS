{-|
Module      : MetaHS.EDSL.Graph.UsesGraph
Description : Generates a Uses graph for a module
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Generates a Uses graph for a module
-}
module MetaHS.EDSL.Graph.UsesGraph
    ( internalUses
    , internalUsesGraph
    , internalUsesParams
    ) where

import Data.Maybe (fromMaybe)
import Data.List (nub, elemIndex)
import Data.Text.Lazy (Text, pack)
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import MetaHS.DataModel.MetaModel
import MetaHS.EDSL.MetaModel
import MetaHS.EDSL.Graph.Types
import MetaHS.EDSL.Utils

-- | Generates the internal uses graph for the specificed module and returns
--   the associated default GraphvizParams.
internalUses :: MetaModel               -- ^ The meta-model.
             -> Element                 -- ^ The module.
             -> Directed                -- ^ This option will determine whether the resulting graph is directed or undirected.
             -> String                  -- ^ The prefix for the links in the generated SVG images to the correct HTML editor for displaying the source code. Should probably become a hard link if a web server is used.
             -> (GraphType,ParamsType)  -- ^ The generated Graph.
internalUses metaModel moduleElement directed editorLink = (graph,params)
  where
    graph = internalUsesGraph metaModel moduleElement directed
    params = internalUsesParams metaModel moduleElement directed editorLink


-- | Generates the internal uses graph for the specified module.
internalUsesGraph :: MetaModel  -- ^ The meta-model.
                  -> Element    -- ^ The module.
                  -> Directed   -- ^ This option will determine whether the resulting graph is directed or undirected.
                  -> GraphType  -- ^ The generated Graph.
internalUsesGraph metaModel moduleElement directed = graph
  where
    graph = mkGraph ns es
    elements = moduleContains metaModel moduleElement

    ns :: [LNode Element]
    ns = zipWith f [0..] elements                                               -- ns = nodes
      where
        f :: Int -> Element -> LNode Element
        f i e = (i, e)

    es :: [LEdge Text]                                                          -- es = edges
    es = concat [f a b | a <- elements, b <- elementUses metaModel a]
      where
        f :: Element -> Element -> [LEdge Text]
        f a b = nub $ fromMaybe [] $ mkLEdge <$> ia <*> ib
          where
            mkLEdge :: Int -> Int -> [LEdge Text]
            mkLEdge x y = case directed of
                Directed   -> [(x, y, pack "")]                                 -- directed graphs
                Undirected -> [(x, y, pack ""), (y, x, pack "")]                -- undirected graphs require A -> B AND B -> A in order to come to A -- B
            ia = elemIndex a elements
            ib = elemIndex b elements


-- | Generates the default GraphvizParams for the internal uses graph for the
--   specified module.
internalUsesParams :: MetaModel    -- ^ The meta-model.
                   -> Element      -- ^ The module.
                   -> Directed     -- ^ This option will determine whether the resulting graph is directed or undirected.
                   -> String       -- ^ The prefix for the links in the generated SVG images to the correct HTML editor for display the source code. Should probably become a hard link if a web server is used.
                   -> ParamsType   -- ^ The generated Graph.
internalUsesParams metaModel moduleElement directed editorLink =
    nonClusteredParams
        { isDirected       = case directed of                                   -- undirected graphs require A -> B AND B -> A in order to come to A -- B
            Directed   -> True
            Undirected -> False
        , globalAttributes = globAttr
        , fmtNode          = formatNode
        , fmtEdge          = const []
        }
  where
    globAttr =
        [GraphAttrs
            [Label . StrLabel . pack $ name moduleElement
            ,Overlap ScaleXYOverlaps
            ]
        ]
    formatNode (_, fe@(Function qn)) =
        [qnToLabel qn
        ,shape BoxShape
        ,styles [bold, filled]
        ,FillColor [toWC (RGBA 0xFF 0xFF 0xFF 0x40)]                            -- White, alpha=0.25
        ,Color [toWC (RGBA 0x00 0x00 0x00 0xFF)]                                -- Black, alpha=1.00
        ,URL $ urlQuery metaModel fe
        ]
    formatNode (_, dte@(DataType qn)) =
        [qnToLabel qn
        ,shape BoxShape
        ,styles [bold, filled]
        ,FillColor [toWC (RGBA 0x41 0xA3 0x17 0x40)]                            -- LimeGreen, alpha=0.25
        ,Color [toWC (RGBA 0x41 0xA3 0x17 0xFF)]                                -- LimeGreen, alpha=1.00
        ,URL $ urlQuery metaModel dte
        ]
    formatNode (_, tse@(TypeSynonym qn)) =
        [qnToLabel qn
        ,shape BoxShape
        ,styles [bold, filled]
        ,FillColor [toWC (RGBA 0x2B 0x65 0xEC 0x40)]                            -- OceanBlue; alpha=0.25
        ,Color [toWC (RGBA 0x2B 0x65 0xEC 0xFF)]                                -- OceanBlue, alpha=1.00
        ,URL $ urlQuery metaModel tse
        ]
    formatNode (_, e) = [qnToLabel $ name e]

    qnToLabel qn = Label $ StrLabel . pack . snd $ split qn

    urlQuery mm e = pack $ case elementSource mm e of
        Just s -> editorLink ++ locationToQuery s
        _ -> ""
