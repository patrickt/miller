{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving,
             KindSignatures, LambdaCase, OverloadedLists, OverloadedStrings, StrictData, TemplateHaskell, TypeFamilies,
             TypeSynonymInstances, DerivingStrategies #-}

module Miller.Expr where

import Data.Foldable
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List.NonEmpty (NonEmpty)
import Data.String
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

newtype Name = Name { unName :: Text }
  deriving newtype (Eq, IsString, Pretty, Show)

data Rec = Non | Rec deriving (Eq, Show)

instance Pretty Rec where
  pretty Rec = "letrec"
  pretty Non = "let"

data Expr a
  = Var Name
  | Num Int
  | Constr Int Int
  | Ap (Expr a) (Expr a)
  | Let Rec [(a, Expr a)] (Expr a)
  | Case (Expr a) Int [a] (Expr a)
  | Lam [a] (Expr a)
    deriving (Eq, Show, Functor)

makeBaseFunctor ''Expr

instance Semigroup (Expr a) where
  (<>) = Ap

instance Pretty CoreExpr where
  pretty = cata $ \case
    VarF n         -> pretty n
    NumF i         -> pretty i
    ApF f x        -> parens (f <+> x)
    LetF r binds e -> parens (pretty r <+> prettyDefns binds <+> "in" <+> e)
    _              -> "unimplemented"
    where
      prettyDefns = vsep . fmap (\(a, b) -> pretty a <+> "=" <+> b)

isAtomic :: Expr a -> Bool
isAtomic = \case
  Var{} -> True
  Num{} -> True
  _     -> False

type CoreExpr = Expr Name

data Defn a = Defn Name [a] (Expr a) deriving (Eq, Show, Functor)

type CoreDefn = Defn Name

instance Pretty CoreDefn where
  pretty (Defn n vars e) = pretty n <+> hsep (pretty <$> vars) <+> "=" <+> pretty e

newtype Program a = Program (NonEmpty (Defn a))
  deriving newtype (Eq, Show)
  deriving stock Functor

type CoreProgram = Program Name

instance Pretty CoreProgram where
  pretty (Program a) = vsep (toList (pretty <$> a))

xPlusY :: CoreExpr
xPlusY = Ap (Ap (Var "+") (Var "x")) (Var "y")

prog :: CoreProgram
prog = Program [ Defn "main" [] (Ap (Var "double") (Num 21))
               , Defn "double" ["x"] (Ap (Ap (Var "add") (Var "x")) (Var "x"))
               ]

