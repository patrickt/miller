{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miller.TI.Env
  ( Env,
    bindings,
    lookup,
    fromBindings,
    fromList,
    boundNames,
    introduce,
    isNameIntroduced,
  )
where

import Data.HashMap.Lazy qualified as HM
import Data.HashMap.Lazy qualified as Lazy (HashMap)
import Doors
import GHC.Generics
import Miller.Expr
import Optics (Ixed (ix), has, makeLenses, set, (%), (^.))
import Prettyprinter qualified as Pretty
import Prelude hiding (lookup)

newtype Env a = Env
  { _bindings :: Lazy.HashMap Name a
  }
  deriving stock (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via Generically (Env a)

makeLenses ''Env

instance (Pretty a) => Pretty (Env a) where
  pretty (Env e) = do
    let pair (a, b) = pretty a <> "=" <> pretty b
    let elements = fmap pair (HM.toList e)
    Pretty.list elements

fromList :: (Foldable f) => f (Name, a) -> Env a
fromList bs = Env { _bindings = HM.fromList (toList bs) }

lookup :: Name -> Env a -> Maybe a
lookup n (Env e) = HM.lookup n e

fromBindings :: [Name] -> [a] -> Env a
fromBindings n v = Env (HM.fromList (zip n v))

boundNames :: Env a -> [Name]
boundNames = HM.keys . _bindings

introduce :: Foldable f => f Name -> Env () -> Env ()
introduce names e = set bindings updated e
  where
    updated = foldr (\name acc -> HM.insert name mempty acc) (e ^. bindings) names

isNameIntroduced :: Name -> Env () -> Bool
isNameIntroduced n = has (bindings % ix n)
