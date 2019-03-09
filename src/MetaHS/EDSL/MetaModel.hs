{-|
Module      : MetaHS.EDSL.MetaModel
Description : The MetaHS EDSL MetaModel part
License     : <to be determined>
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

MetaHS EDSL MetaModel part
-}
module MetaHS.EDSL.MetaModel
    ( generateMetaModel
    , writeMetaModel
    , readMetaModel
    , writeMetaModelPretty
    , pretty
    , numberOfItems
    , getPrograms
    , getModules
    , programContains
    , moduleContains
    , moduleImports
    , elementContains
    , elementImports
    , elementSource
    , elementUses
    , getRelation
    , setRelation
    , domain
    , range
    , RelationKey
    ) where
import qualified Data.Map.Strict                             as Map
import           Data.Maybe                                  (listToMaybe)
import qualified Data.Set                                    as Set
import qualified MetaHS.DataModel.Extractor.Module.Contains  as ModuleContains
import qualified MetaHS.DataModel.Extractor.Module.Imports   as ModuleImports
import qualified MetaHS.DataModel.Extractor.Module.Source    as ModuleSource
import qualified MetaHS.DataModel.Extractor.Module.Uses      as ModuleUses
import qualified MetaHS.DataModel.Extractor.Program.Contains as ProgramContains
import qualified MetaHS.DataModel.MetaModel                  as MetaModel
import           MetaHS.DataModel.Utils.File.FileUtils
import           Text.PrettyPrint

type RelationKey = String

keyContains :: RelationKey
keyContains = "_contains"

keyUses :: RelationKey
keyUses = "_uses"

keySource :: RelationKey
keySource = "_source"

keyImports :: RelationKey
keyImports = "_imports"


-- | Generates a meta-model.
generateMetaModel :: String                 -- ^ The program name.
                  -> String                 -- ^ The program path.
                  -> String                 -- ^ The parse error output path.
                  -> IO MetaModel.MetaModel -- ^ The resulting meta-model.
generateMetaModel programName programPath parseErrorsPath = do
  (mods, failed) <- modulesInHierarchy programPath -- mods = modules, failed = ParseFailed
  writeFile parseErrorsPath $ show $ map show failed
  let pc = ProgramContains.contains programName mods -- pc = program contains
  let mc = Set.unions [ModuleContains.contains m | m <- mods] -- mc = module contains
  let mu = Set.unions [ModuleUses.uses m mc | m <- mods] -- mu = module uses
  let ms = Set.unions [ModuleSource.source m | m <- mods] -- ms = modules source
  let im = Set.unions [ModuleImports.imports m | m <- mods] -- im = module imports
  return . MetaModel.MetaModel $
    Map.insert keyContains (Set.union pc mc)
    $ Map.insert keyUses mu
    $ Map.insert keySource ms
    $ Map.insert keyImports im Map.empty


-- | Write a meta-model to a file.
writeMetaModel :: MetaModel.MetaModel -- ^ The meta-model to write.
               -> String              -- ^ The path of the file to write.
               -> IO ()               -- ^ No result.
writeMetaModel mm path = writeFile path $ show mm


-- | Read a meta-model from a file.
readMetaModel :: String                 -- ^ The path of the file to read.
              -> IO MetaModel.MetaModel -- ^ The meta-model read.
readMetaModel path = do
    s <- readFile path
    return $ read s


-- | Write a meta-model to a file.
writeMetaModelPretty :: MetaModel.MetaModel -- ^ The meta-model to write.
                     -> String              -- ^ The path of the file to write.
                     -> IO ()               -- ^ No result.
writeMetaModelPretty mm path = writeFile path $ renderStyle s $ pretty mm
  where
    s = style { mode = LeftMode }


-- | Returns a pretty printed representation of the meta-model.
pretty :: MetaModel.MetaModel -- ^ The meta-model to pretty print.
       -> Doc                 -- ^ The resulting Doc.
pretty = MetaModel.pPrint


-- | Returns the number of relation items in the meta-model.
numberOfItems :: MetaModel.MetaModel -- ^ The meta-model.
              -> Int                 -- ^ The number of relation items in the meta-model.
numberOfItems mm = Map.foldr f 0 $ MetaModel.getMetaModelImpl mm
  where
    f :: MetaModel.Relation -> Int -> Int
    f r t = t + Set.size r


-- | Returns a list of Programs contained in the metamodel.
getPrograms :: MetaModel.MetaModel -- ^ The metamodel.
         -> [MetaModel.Element] -- ^ The Programs contained in the metamodel.
getPrograms mm = Set.elems $ Set.foldr f Set.empty pcs
  where
    f (p@MetaModel.Program{},_) es = Set.insert p es
    f _ es = es
    pcs = getRelation keyContains mm


-- | Returns a list of Modules contained in the metamodel.
getModules :: MetaModel.MetaModel  -- ^ The metamodel.
           -> [MetaModel.Element]  -- ^ The Modules contained by the specified Program.
getModules mm = Set.elems $ Set.foldr f Set.empty pcs
  where
    f (p,c) ms
        | isProgram p && isModule c = Set.insert c ms
        | otherwise = ms
    pcs = getRelation keyContains mm


-- | Returns a list of Elements contained by the specified Program.
programContains :: MetaModel.MetaModel  -- ^ The meta-model.
                -> MetaModel.Element    -- ^ The specified Program.
                -> [MetaModel.Element]  -- ^ The Elements contained by the specified Program.
programContains mm e
    | isProgram e = elementContains mm e
    | otherwise = error "Function MetaHS.EDSL.MetaModel.programContains only works for MetaHS.DataModel.MetaModel.Program Elements!"


-- | Returns a list of Elements contained by the specified Module.
moduleContains :: MetaModel.MetaModel  -- ^ The meta-model.
               -> MetaModel.Element    -- ^ The specified Module.
               -> [MetaModel.Element]  -- ^ The Elements contained by the specified Module.
moduleContains mm e
    | isModule e = elementContains mm e
    | otherwise = error "Function MetaHS.EDSL.MetaModel.moduleContains only works for MetaHS.DataModel.MetaModel.Module Elements!"


-- | Returns a list of Elements imported by the specified Module.
moduleImports :: MetaModel.MetaModel   -- ^ The meta-model.
               -> MetaModel.Element    -- ^ The specified Module.
               -> [MetaModel.Element]  -- ^ The Elements contained by the specified Module.
moduleImports mm e
    | isModule e = elementImports mm e
    | otherwise = error "Function MetaHS.EDSL.MetaModel.moduleContains only works for MetaHS.DataModel.MetaModel.Module Elements!"


-- | Returns a list of Elements contained by the specified Element.
elementContains :: MetaModel.MetaModel  -- ^ The meta-model.
                -> MetaModel.Element    -- ^ The specified Element.
                -> [MetaModel.Element]  -- ^ The Elements contained by the specified Element.
elementContains mm e = Set.foldr f [] pcs
  where
    f (p,c) es
        | p == e = c : es
        | otherwise = es
    pcs = getRelation keyContains mm


-- | Returns a list of Elements imported by the specified Element.
elementImports :: MetaModel.MetaModel  -- ^ The meta-model.
                -> MetaModel.Element    -- ^ The specified Element.
                -> [MetaModel.Element]  -- ^ The Elements contained by the specified Element.
elementImports mm e = Set.foldr f [] pcs
  where
    f (p,c) es
        | p == e = c : es
        | otherwise = es
    pcs = getRelation keyImports mm


-- | Returns a the source location for the specified Element.
elementSource :: MetaModel.MetaModel      -- ^ The meta-model.
              -> MetaModel.Element        -- ^ The specified Element.
              -> Maybe MetaModel.Element  -- ^ The source location for the specified Element.
elementSource mm e = listToMaybe $ Set.foldr f [] pcs
  where
    f (p,c) es
        | p == e = c : es
        | otherwise = es
    pcs = getRelation keySource mm


-- | Returns a list of Elements used by the specified Element.
elementUses :: MetaModel.MetaModel  -- ^ The meta-model.
            -> MetaModel.Element    -- ^ The specified Element.
            -> [MetaModel.Element]  -- ^ The Elements used by the specified Element.
elementUses mm e = Set.foldr f [] pcs
  where
    f (p,c) es
        | p == e = c : es
        | otherwise = es
    pcs = getRelation keyUses mm


-- | Returns the relation corresponding to the supplied key or an empty Set.
getRelation :: String               -- ^ The metamodel key string.
            -> MetaModel.MetaModel  -- ^ The metamodel.
            -> MetaModel.Relation   -- ^ The corresponding Relation or an empty set.
getRelation k mm =
    Map.findWithDefault Set.empty k $ MetaModel.getMetaModelImpl mm


-- Returns a MetaModel that is appended with a new or replaced Relation set.
setRelation :: String               -- ^ The metamodel key string.
            -> MetaModel.Relation   -- ^ The Relation to add.
            -> MetaModel.MetaModel  -- ^ The metamodel.
            -> MetaModel.MetaModel  -- ^ The resulting metamodel.
setRelation k rs mm = MetaModel.MetaModel $ Map.insert k rs mmi
  where
    mmi = MetaModel.getMetaModelImpl mm


-- Returns the domain for a specified relation in the metamodel.
domain :: String                    -- ^ The metamodel key string.
       -> MetaModel.MetaModel       -- ^ The metamodel.
       -> Set.Set MetaModel.Element -- ^ Set of Elements that form the domain of the binary relation.
domain k mm = foldr f Set.empty r
  where
    f a = Set.insert (fst a)
    r = getRelation k mm

-- Returns the range for a specified relation in the metamodel.
range :: String                    -- ^ The metamodel key string.
      -> MetaModel.MetaModel       -- ^ The metamodel.
      -> Set.Set MetaModel.Element -- ^ Set of Elements that form the range of the binary relation.
range k mm = foldr f Set.empty r
  where
    f a = Set.insert (snd a)
    r = getRelation k mm


-- | Checks that the provided MetaModel.Element is a MetaModel.Program
isProgram :: MetaModel.Element
          -> Bool
isProgram MetaModel.Program{} = True
isProgram _ = False


-- | Checks that the provided MetaModel.Element is a MetaModel.Module
isModule :: MetaModel.Element
         -> Bool
isModule MetaModel.Module{} = True
isModule _ = False