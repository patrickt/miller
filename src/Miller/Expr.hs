{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Miller.Expr
  ( Name (..),
    Rec (..),
    Expr (..),
    ($$),
    ($+),
    ($*),
    ($-),
    BinOp (..),
    UnOp (..),
    binOpToName,
    unOpToName,
    CoreExpr,
    isAtomic,
    Defn (..),
    CoreDefn,
    isCAF,
    Program (..),
    CoreProgram,
    preludeDefs,
  )
where

import Data.Hashable
import Data.List.NonEmpty (NonEmpty)
import Data.String
import Data.Text (Text)
import Prettyprinter
import GHC.Exts (IsList (..))

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

binOpToName :: BinOp -> Name
binOpToName b = Name $ case b of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"

data UnOp = Neg deriving (Eq, Show)

unOpToName :: UnOp -> Name
unOpToName Neg = Name "~"

data Expr a
  = Var Name
  | Num Int
  | Constr Int Int
  | Ap (Expr a) (Expr a)
  | Let Rec (NonEmpty (a, Expr a)) (Expr a)
  | Case (Expr a) (NonEmpty (Int, [a], Expr a))
  | Lam [a] (Expr a)
  | Binary BinOp (Expr a) (Expr a)
  | Unary UnOp (Expr a)
  deriving (Eq, Show, Functor)

infixl 8 $$

infixl 7 $*

infixl 6 $-

infixl 6 $+

($$), ($+), ($*), ($-) :: Expr a -> Expr a -> Expr a
($$) = Ap
($+) = Binary Add
($*) = Binary Mul
($-) = Binary Sub

instance Semigroup (Expr a) where
  (<>) = Ap

isAtomic :: Expr a -> Bool
isAtomic = \case
  Var {} -> True
  Num {} -> True
  _ -> False

type CoreExpr = Expr Name

data Defn a = Defn Name [a] (Expr a) deriving (Eq, Show, Functor)

isCAF :: Defn a -> Bool
isCAF (Defn _ xs _) = null xs

type CoreDefn = Defn Name

newtype Program a = Program {unProgram :: NonEmpty (Defn a)}
  deriving newtype (Eq, Show, Semigroup)
  deriving stock (Functor)

instance IsList (Program a) where
  type Item (Program a) = Defn a
  fromList = Program . fromList
  toList = toList . unProgram

type CoreProgram = Program Name

preludeDefs :: CoreProgram
preludeDefs =
  [ Defn "I" ["x"] (Var "x"),
    Defn "K" ["y", "z"] (Var "y"),
    Defn "K1" ["y", "z"] (Var "z"),
    Defn "S" ["a", "b", "c"] ((Var "a" <> Var "c") <> (Var "b" <> Var "c")),
    Defn "compose" ["f", "g", "x"] (Var "f" <> (Var "g" <> Var "x")),
    Defn "twice" ["f"] ((Var "compose" <> Var "f") <> Var "f")
  ]
