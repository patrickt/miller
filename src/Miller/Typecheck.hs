{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, LambdaCase, OverloadedLists, TypeApplications, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Miller.Typecheck where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Doors
import Data.Coerce
import GHC.Generics

import Control.Effect
import Control.Effect.Reader
import Control.Effect.Error
import Control.Effect.Fresh
import Control.Effect.State

import Data.Text.Prettyprint.Doc (Doc, Pretty (..), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty

data Exp = EVar String
         | ELit Lit
         | EApp Exp Exp
         | EAbs String Exp
         | ELet String Exp Exp
           deriving (Eq, Show, Generic, Hashable)

instance Pretty Exp where
  pretty = \case
    EVar v -> pretty v
    ELit l -> pretty l
    EApp f x -> Pretty.parens (pretty f <+> pretty x)
    EAbs s e -> "λ" <> pretty s <+> "→" <+> pretty e
    ELet n e b -> "let" <+> pretty n <+> "=" <+> pretty e <+> "in" <+> pretty b

data Lit = LInt Integer
         | LBool Bool
           deriving (Eq, Show, Generic, Hashable)

instance Pretty Lit where
  pretty = \case
    LInt i -> pretty i
    LBool b -> pretty b

data Type = TVar String
          | TInt
          | TBool
          | TFun Type Type
            deriving (Eq, Show, Generic, Hashable)

instance Pretty Type where
  pretty = \case
    TVar s   -> pretty s
    TInt     -> "Int"
    TBool    -> "Bool"
    TFun a b -> Pretty.parens (pretty a <+> "→" <+> pretty b)

data Scheme = Scheme [String] Type deriving (Eq, Show)

instance Pretty Scheme where
  pretty (Scheme tv t) = "∀" <+> Pretty.parens (Pretty.hcat (Pretty.punctuate ", " (pretty <$> tv))) <+> "." <+> pretty t

type Subst = HashMap String Type

composeSubst :: Subst -> Subst -> Subst
composeSubst x y = mappend x (fmap (apply x) y)

class Types a where
  ftv :: a -> HashSet String
  apply :: Subst -> a -> a

instance Types a => Types [a] where
  apply s = fmap (apply s)
  ftv = foldMap ftv

instance Types Type where
  ftv = \case
    TVar s -> [s]
    TInt -> mempty
    TBool -> mempty
    TFun f x  -> ftv f <> ftv x
  apply s = \case
    TVar n -> fromMaybe (TVar n) (HashMap.lookup n s)
    TFun f x -> TFun (apply s f) (apply s x)
    other -> other

instance Types Scheme where
  ftv (Scheme vars t) = HashSet.difference (ftv t) (HashSet.fromList vars)
  apply s (Scheme vars t) = Scheme vars (apply (foldr HashMap.delete s vars) t)

newtype TypeEnv = TypeEnv { unTypeEnv :: HashMap String Scheme }

remove :: String -> TypeEnv -> TypeEnv
remove = coerce (HashMap.delete @String @Scheme)

instance Types TypeEnv where
  ftv = ftv . HashMap.elems . unTypeEnv
  apply s (TypeEnv e) = TypeEnv (fmap (apply s) e)

newTyVar :: (Member Fresh sig, Carrier sig m) => String -> m Type
newTyVar s = fmap (TVar . mappend s . show) fresh

data Env = Env deriving (Eq, Show)

instance Lower Env where
  lowerBound = Env

infer :: _ a -> Either String a
infer = run . runFresh . runReader @Env lowerBound . runError
