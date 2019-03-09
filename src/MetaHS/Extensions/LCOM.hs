{-|
Module      : MetaHS.Extensions.LCOM
Description : The MetaHS EDSL LCOM part
License     : <to-be-determined>
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Metamodel Elements that are interesting (those that are used to determine nodes in
MetaHS.EDSL.Graph.UsesGraph module ) are : TypeSynonym, DataType, Function.
All other Elements that are  part of the _contains relations are ignored.

LCOM4 - Number of connected components in the graph that represents each method as a node and the sharing of at least
        one attribute/method as an edge.
        ref. Hitz and Montazeri (1995)
-}
module MetaHS.Extensions.LCOM
    ( lcom
    , lcomGraph
    , keyLcom
    , lcomAggregator
    ) where

import qualified Data.Set as Set
import Data.Graph.Inductive
import MetaHS.DataModel.MetaModel
import MetaHS.EDSL.MetaModel
import MetaHS.EDSL.Graph


-- | Calculates the LCOM metric value for a specified module name.
lcom :: MetaModel -- ^ The meta-model.
     -> Element   -- ^ The Module Element.
     -> Int       -- ^ The calculated LCOM metric value.
lcom metaModel moduleElement = do
    let (value, _, _) = lcomGraph metaModel moduleElement Directed ""
    value


-- | Generates the internalUsesGraph for the specified Module Element and
-- calculates the corresponding LCOM metric value.
lcomGraph :: MetaModel                    -- ^ The meta-model.
          -> Element                      -- ^ The Module Element.
          -> Directed                     -- ^ Create a directed or undirected graph.
          -> String                       -- ^ The prefix for the links in the generated SVG images to the correct HTML editor for display the source code. Should probably become a hard link if a web server is used.
          -> (Int, GraphType, ParamsType) -- ^ The calculated LCOM metric value and the generated Graph and default parameters.
lcomGraph metaModel moduleElement directed editorLink = do
    let (graph, params) = internalUses metaModel moduleElement directed editorLink
    (noComponents graph, graph, params)


-- | MetaModel key used for the LCOM relation.
keyLcom :: RelationKey
keyLcom = "LCOM"


-- | LCOM aggregator that adds the LCOM relation to the metamodel.
-- key = keyLcom
-- value = (p,c) where p is a Module Element and c is an IntValue Element that
-- contains the LCOM value for the module.
lcomAggregator :: MetaModel
               -> MetaModel
lcomAggregator mm = setRelation keyLcom r mm
  where
    r = foldr f Set.empty $ filter (\x -> x /= Module {name = "?"}) $ getModules mm -- r = relation
    f m = Set.insert (m, lv m) -- f = foldr function
    lv m = IntValue $ lcom mm m                                                        -- lv = LCOM value