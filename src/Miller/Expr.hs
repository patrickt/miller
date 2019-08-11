{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving,
             KindSignatures, LambdaCase, OverloadedLists, OverloadedStrings, TemplateHaskell, TypeFamilies,
             TypeSynonymInstances, DerivingStrategies #-}

module Miller.Expr
  ( Name (..)
  , Rec (..)
  , Expr (..)
  , ($$)
  , ($+)
  , ($*)
  , BinOp (..)
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

data BinOp
  = Add
  | Sub
  | Mul
    deriving (Eq, Show)

data Expr a
  = Var Name
  | Num Int
  | Constr Int Int
  | Ap (Expr a) (Expr a)
  | Let Rec (NonEmpty (a, Expr a)) (Expr a)
  | Case (Expr a) (NonEmpty (Int, [a], Expr a))
  | Lam [a] (Expr a)
  | Binary BinOp (Expr a) (Expr a)
    deriving (Eq, Show, Functor)

infixl 8 $$
($$), ($+), ($*) :: Expr a -> Expr a -> Expr a
($$) = Ap
($+) = Binary Add
($*) = Binary Mul



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
              , Defn "compose" ["f", "g", "x"] (Var "f" <> (Var "g" <> Var "x"))
              , Defn "twice" ["f"] ((Var "compose" <> Var "f") <> Var "f")
              ]
