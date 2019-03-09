{-|
Module      : MetaHS.DataModel.MetaModel
Description : The MetaHS metamodel
License     : <to be determined>
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

MetaHS metamodel
-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module MetaHS.DataModel.MetaModel
    ( Element(..)
    , Pair
    , Relation
    , MetaModel(..)
    , pPrint
    ) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Prelude hiding ((<>))

-- | The various elements that can be used in the metamodel.
data Element
    = Program
        { name :: !String
        -- ^ The name of the program.
        }
    -- ^ Top level program
    | Module
        { name :: !String
        -- ^ The qualified name of the module.
        }
    -- ^ Represents a module
    | ModuleHead
        { name :: !String
        -- ^ The qualified name of the module.
        }
    -- ^ Represents a module head
    | ModuleImport
        { name :: !String
        -- ^ The qualified name of the imported module.
        }
    -- ^ Represents a module import
    | ModuleExport
        { name :: !String
        -- ^ The qualified name of the exported module.
        }
    -- ^ Represents a module export
    | Function
        { name :: !String
        -- ^ The qualified name of the function.
        }
    -- ^ Represents a function
    | DataType
        { name :: !String
        -- ^ The qualified name of the data declaration.
        }
    -- ^ Represents a datatype declaration.
    | TypeSynonym
        { name :: !String
        -- ^ The qualified name of the type synonym.
        }
    -- ^ Represents a type synonym.
    | TypeSignature
        { name :: !String
        -- ^ The qualified name of the type signature.
        }
        -- ^ Represents a type signature.
    | TypeClass
        { name :: !String
        -- ^ The qualified name of the type instance.
        }
        -- ^ Represents a type instance.
    | Instance
        { name :: !String
        -- ^ The qualified name of the type instance.
        }
        -- ^ Represents a type instance.
    | UnknownType
        { name :: !String
        -- ^ The "?" qualified name of the unknown type.
        }
    -- ^ Represents a type which is unknown.
    | Location
        { locationPath        :: !String
        -- ^ The path to the file containing the source code.
        , locationStartLine   :: !Int
        -- ^ The start line.
        , locationStartColumn :: !Int
        -- ^ The start column.
        , locationEndLine     :: !Int
        -- ^ The end line.
        , locationEndColumn   :: !Int
        -- ^ The end column.
        }
    -- ^ Represents a source code location.
    | StringValue
        { stringValue :: !String
        -- ^ The String value.
        }
    -- ^ Contains a generic String value.
    | IntValue
        { intValue :: !Int
        -- ^ The Int value.
        }
    -- ^ Contains a generic Int value.
    deriving (Show, Read, Eq, Ord)

-- | A Pair is defined as a tuple of Elements
type Pair = (Element,Element)

-- | A relation is defined as a set of Pair types.
--   E.g., _contains, _source, _uses, LCOM
type Relation = Set.Set (Element,Element)

-- | A metamodel is implemented as a mapping between a String and Relation.
--   The key string will denote the type of relation between the pairs in the
--   value relation.
type MetaModelImpl = Map.Map String Relation

-- | The MetaModel type.
newtype MetaModel = MetaModel { getMetaModelImpl :: MetaModelImpl }
    deriving (Read,Show)

-- | Pretty print instance for Elements.
instance Pretty Element where
    pPrint Program { name = n } = text "Program" <+> qt n
    pPrint Module { name = n } = text "Module" <+> qt n
    pPrint ModuleHead { name = n } = text "ModuleHead" <+> qt n
    pPrint ModuleImport { name = n } = text "ModuleImport" <+> qt n
    pPrint ModuleExport { name = n } = text "ModuleExport" <+> qt n
    pPrint Function { name = n } = text "Function" <+> qt n
    pPrint DataType { name = n } = text "DataType" <+> qt n
    pPrint TypeSynonym { name = n } = text "TypeSynonym" <+> qt n
    pPrint TypeSignature { name = n } = text "TypeSignature" <+> qt n
    pPrint TypeClass { name = n } = text "TypeClass" <+> qt n
    pPrint Instance { name = n } = text "Instance" <+> qt n
    pPrint UnknownType { name = n } = text "UnknownType" <+> qt n
    pPrint Location { locationPath = p
                    , locationStartLine = sl
                    , locationStartColumn = sc
                    , locationEndLine = el
                    , locationEndColumn = ec
                    } = text "Location" <+> qt p <+> int sl <+> int sc
                        <+> int el <+> int ec
    pPrint StringValue { stringValue = v } = text "StringValue" <+> qt v
    pPrint IntValue { intValue = v } = text "IntValue" <+> qt (show v)

-- | Pretty print instance for the MetaModel.
instance Pretty MetaModel where
    pPrint metaModel =
        if null metaModelMap
        then
            text "MetaModel"
        else
            text "MetaModel" $$
            vcat ( map f (Map.keys metaModelMap) ) $$
            text ""
        where
            metaModelMap = getMetaModelImpl metaModel

            f :: String -> Doc
            f key =                                                             -- function f iterates through the keys
                text "  " <> text (show key) <> char ':' $$
                g ( fromMaybe Set.empty $ Map.lookup key metaModelMap)

            g :: Relation -> Doc
            g relation =
                if null relation
                then
                    text ""
                else
                    vcat . map (docLine "    ") $ Set.elems relation
                where
                    docLine prefix v = text prefix <> pPrint v


-- | Returns the provided String as a quoted Doc
qt :: String -> Doc
qt t = char '"' <> text t <> char '"'
