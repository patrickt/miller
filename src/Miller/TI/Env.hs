{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Miller.TI.Env
  ( Env
  , lookup
  , insert
  , fromBindings
  ) where

import Prelude hiding (lookup)
import           Doors

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Prettyprint.Doc as Pretty

import           Miller.Expr

newtype Env a = Env (HashMap Name a) deriving (Eq, Show, Semigroup, Monoid, Lower)

instance Pretty a => Pretty (Env a) where
  pretty (Env e) = do
    let pair (a, b) = pretty a <> "=" <> pretty b
    let elements    = fmap pair (HM.toList e)
    Pretty.list elements

lookup :: Name -> Env a -> Maybe a
lookup n (Env e) = HM.lookup n e

insert :: Name -> a -> Env a -> Env a
insert n v (Env e) = Env (HM.insert n v e)

fromBindings :: [Name] -> [a] -> Env a
fromBindings n v = Env (HM.fromList (zip n v))
