{-|
Module      : MetaHS.EDSL.Graph.Types
Description : The MetaHS EDSL Graph types
Copyright   : Copyright (C) 2017-2019 H.H.R.F. Vos, S. Kamps
License     : MIT
Maintainer  : hhrf.vos@studie.ou.nl, sanderkamps79@gmail.com
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