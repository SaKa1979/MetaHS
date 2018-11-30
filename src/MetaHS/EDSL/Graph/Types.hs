{-|
Module      : MetaHS.EDSL.Graph.Types
Description : The MetaHS EDSL Graph types
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

MetaHS EDSL Graph types
-}
module MetaHS.EDSL.Graph.Types
    ( GraphType
    , ParamsType
    , Directed (..)
    ) where

import Data.Text.Lazy (Text)
import Data.Graph.Inductive
import Data.GraphViz
import MetaHS.DataModel.MetaModel (Element)

-- | Type synonym for generated LCOM graph.
type GraphType  = Gr Element Text

-- | Type synonym for generated GraphvizParams.
type ParamsType = GraphvizParams Node Element Text () Element

-- | Determines whether the graph should be directed or undirected.
data Directed = Directed | Undirected
