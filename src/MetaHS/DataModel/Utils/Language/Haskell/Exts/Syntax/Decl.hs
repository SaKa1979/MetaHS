{-|
Module      : MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Decl
Description : Utility functions for Decl objects.
License     : <to-be-determined>
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Utility functions for Decl objects.
-}
module MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Decl
    where

import Data.Maybe ()
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.Name as Name
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.QName as QName
import qualified MetaHS.DataModel.Utils.Language.Haskell.Exts.Syntax.DeclHead as DeclHead

-- | Returns the SrcSpanInfo object associated with the Decl object.
srcSpanInfo :: Decl SrcSpanInfo -> SrcSpanInfo
srcSpanInfo = ann


-- | Represents the (simplified) information for a data constructor.
data DataConstructor
    = DataConstructor
        { dataConstructorName :: String
        -- ^ The name of the data constructor.
        , valueConstructors   :: [ValueConstructor]
        -- ^ The value constructors for this data constructor.
        }
    deriving (Show)

-- | Represents the information for a value constructor.
data ValueConstructor
    = ValueConstructor
        { valueConstructorName  :: String
        -- ^ The name of the value constructor.
        , valueConstructorTypes :: [Type SrcSpanInfo]
        -- ^ Contains the types used by this value constructor if this is not
        --   defined using record notation.
        --   For GADT data type declarations, this field will contain the final
        --   type irrespective of whether record notation is used for the
        --   preceeding types.
        , valueConstructorFields :: [Field]
        -- ^ In case the value constructor is defined using record notation,
        --   this field will contain the fields used by this value constructor.
        }
    deriving (Show)

-- | Represents a record field of a value constructor.
data Field
    = Field
        { fieldNames :: [String]
        -- ^ The name(s) for this field.
        , fieldTypes :: Type SrcSpanInfo
        -- ^ The types used by this field.
        }
    deriving (Show)

-- | Converts a Decl to a DataConstructor object if possible (DataDecl or
--   GDataDecl).
dataConstructor :: Decl SrcSpanInfo -> Maybe DataConstructor
dataConstructor (DataDecl _ _ _ dh qcds _) = Just DataConstructor
    { dataConstructorName = DeclHead.name dh
    , valueConstructors   = vcs
    }
  where
    vcs = [ createVC ds | (QualConDecl _ _ _ ds) <- qcds]
    createVC :: ConDecl SrcSpanInfo -> ValueConstructor
    createVC (ConDecl _ n ts) = ValueConstructor
        { valueConstructorName = Name.name n
        , valueConstructorTypes = ts
        , valueConstructorFields = []
        }
    createVC (InfixConDecl _ t1 n t2) = ValueConstructor
        { valueConstructorName = Name.name n
        , valueConstructorTypes = [t1, t2]
        , valueConstructorFields = []
        }
    createVC (RecDecl _ n fds) = ValueConstructor
        { valueConstructorName = Name.name n
        , valueConstructorTypes = []
        , valueConstructorFields = [createField fd | fd <- fds]
        }
    createField (FieldDecl _ ns t) = Field
        { fieldNames = [Name.name n | n <- ns]
        , fieldTypes = t
        }
dataConstructor (GDataDecl _ _ _ dh _ gds _) = Just DataConstructor             -- dh = (DeclHead l), gds = [GadtDecl l]
    { dataConstructorName = DeclHead.name dh
    , valueConstructors   = vcs
    }
  where
    vcs = [ createVC gd | gd <- gds]
    createVC :: GadtDecl SrcSpanInfo -> ValueConstructor
    createVC (GadtDecl _ n Nothing ts) = ValueConstructor                       -- n = (Name l), ts = (Type l)
        { valueConstructorName = Name.name n
        , valueConstructorTypes = [ts]
        , valueConstructorFields = []
        }
    createVC (GadtDecl _ n (Just fds) ts) = ValueConstructor                    -- n = (Name l), fds = [FieldDecl l], ts = (Type l)
        { valueConstructorName = Name.name n
        , valueConstructorTypes = [ts]
        , valueConstructorFields = [createField fd | fd <- fds]
        }
    createField (FieldDecl _ ns t) = Field
        { fieldNames = [Name.name n | n <- ns]
        , fieldTypes = t
        }
dataConstructor _ = Nothing


-- | Returns the name of the PatBind if possible.
patternName :: Decl SrcSpanInfo
            -> Maybe String
patternName (PatBind _ (PVar _ n) _ _) = Just $ Name.name n
patternName _ = Nothing


-- | Returns the name of the FunBind if possible.
functionName :: Decl SrcSpanInfo
             -> Maybe String
functionName (FunBind _ []) = Nothing
functionName (FunBind _ ms) = Just . fn $ head ms
  where
    fn :: Match l -> String
    fn (Match _ x _ _ _)        = Name.name x
    fn (InfixMatch _ _ x _ _ _) = Name.name x
functionName _ = Nothing

-- | Returns the name of the TypeSig if possible.
typeSigName :: Decl SrcSpanInfo
            -> Maybe String
typeSigName (TypeSig _ [] _) = Nothing
typeSigName (TypeSig _ n _) = Just $ Name.name $ head n

-- | Returns the name of the typeClass if possible.
typeClassName :: Decl SrcSpanInfo
              -> Maybe String
typeClassName (ClassDecl _ _ (DHApp _ (DHead _ n) _) _ _) = Just $ Name.name n
typeClassName _ = Nothing

-- | Returns the name of the Instance (as "<type-class> <data-type>") if possible.
instanceName :: Decl SrcSpanInfo
              -> Maybe String
instanceName (InstDecl _ _ (IRule _ _ _ (IHApp _ ih t)) _) = fn ih t
  where fn :: InstHead l -> Type l -> Maybe String
        fn (IHCon _ qntc) ty = do
          qnt <- isTycon ty
          n1 <- QName.name qntc
          n2 <- QName.name qnt
          return $ "(" ++ n1 ++ " " ++ n2 ++ ")"
        fn _ _ = Nothing
        isTycon (TyCon _ qnt) = Just qnt
        isTycon (TyParen _ (TyCon _ qnt)) = Just qnt
        isTycon _ = Nothing


instanceName _ = Nothing