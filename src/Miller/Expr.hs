{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving,
             KindSignatures, LambdaCase, OverloadedLists, OverloadedStrings, TemplateHaskell, TypeFamilies,
             TypeSynonymInstances, DerivingStrategies #-}

module Miller.Expr
  ( Name (..)
  , Rec (..)
  , Expr (..)
  , CoreExpr
  , isAtomic
  , Defn (..)
  , CoreDefn
  , isCAF
  , Program (..)
  , CoreProgram
  , preludeDefs
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.String
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Exts (IsList (..))
import Data.Hashable

newtype Name = Name Text
  deriving newtype (Eq, IsString, Pretty, Show, Ord, Hashable)

data Rec = Non | Rec deriving (Eq, Show)

instance Pretty Rec where
  pretty Rec = "letrec"
  pretty Non = "let"

data Expr a
  = Var Name
  | Num Int
  | Constr Int Int
  | Ap (Expr a) (Expr a)
  | Let Rec (NonEmpty (a, Expr a)) (Expr a)
  | Case (Expr a) (NonEmpty (Int, [a], Expr a))
  | Lam [a] (Expr a)
    deriving (Eq, Show, Functor)

instance Semigroup (Expr a) where
  (<>) = Ap

isAtomic :: Expr a -> Bool
isAtomic = \case
  Var{} -> True
  Num{} -> True
  _     -> False

type CoreExpr = Expr Name

data Defn a = Defn Name [a] (Expr a) deriving (Eq, Show, Functor)

isCAF :: Defn a -> Bool
isCAF (Defn _ xs _) = null xs

type CoreDefn = Defn Name

newtype Program a = Program { unProgram :: NonEmpty (Defn a) }
  deriving newtype (Eq, Show, Semigroup)
  deriving stock (Functor)

instance IsList (Program a) where
  type Item (Program a) = Defn a
  fromList = Program . fromList
  toList  = toList . unProgram

type CoreProgram = Program Name

preludeDefs :: CoreProgram
preludeDefs = [ Defn "I" ["x"] (Var "x")
              , Defn "K" ["y", "z"] (Var "y")
              , Defn "K1" ["y", "z"] (Var "z")
              , Defn "S" ["a", "b", "c"] ((Var "a" <> Var "c") <> (Var "b" <> Var "c"))
              , Defn "D" ["f", "g", "h"] (Var "f" <> (Var "g" <> Var "h"))
              ]
