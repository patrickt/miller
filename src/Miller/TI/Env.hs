{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings #-}

module Miller.TI.Env
  ( Env
  , lookup
  , insert
  , union
  , fromBindings
  , fromList
  ) where

import Prelude hiding (lookup)
import           Doors

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text.Prettyprint.Doc as Pretty

import           Miller.Expr

newtype Env a = Env (HashMap Name a) deriving (Eq, Show, Semigroup, Monoid, Functor, Foldable, Traversable, Lower)

instance Pretty a => Pretty (Env a) where
  pretty (Env e) = do
    let pair (a, b) = pretty a <> "=" <> pretty b
    let elements    = fmap pair (HM.toList e)
    Pretty.list elements

fromList :: Foldable f => f (Name, a) -> Env a
fromList = Env . HM.fromList . toList

union :: Env a -> Env a -> Env a
union (Env a) (Env b) = Env (HM.union a b)

lookup :: Name -> Env a -> Maybe a
lookup n (Env e) = HM.lookup n e

insert :: Name -> a -> Env a -> Env a
insert n v (Env e) = Env (HM.insert n v e)

fromBindings :: [Name] -> [a] -> Env a
fromBindings n v = Env (HM.fromList (zip n v))
