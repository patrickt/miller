{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Miller.Stats
  ( Stats,
    steps,
    step,
    allocation,
    update,
    reduction,
    depth,
  )
where

import Control.Effect.Accum
import Data.Monoid
import Data.Monoid.Generic
import Data.Semigroup
import GHC.Generics
import Prettyprinter

data Stats = Stats
  { steps :: Sum Int,
    allocations :: Sum Int,
    reductions :: Sum Int,
    updates :: Sum Int,
    maxDepth :: Max Int
  }
  deriving (Show, Generic)
  deriving (Semigroup) via GenericSemigroup Stats
  deriving (Monoid) via GenericMonoid Stats

step, allocation, reduction, update :: (Has (Accum Stats) sig m) => m ()
step = add mempty {steps = 1}
allocation = add mempty {allocations = 1}
reduction = add mempty {reductions = 1}
update = add mempty {updates = 1}

depth :: (Has (Accum Stats) sig m) => Int -> m ()
depth n = add mempty {maxDepth = Max n}

instance Pretty Stats where
  pretty Stats {..} =
    vcat
      [ "Steps:" <+> pretty (getSum steps),
        "Allocations:" <+> pretty (getSum allocations),
        "Reductions:" <+> pretty (getSum reductions),
        "Updates:" <+> pretty (getSum updates),
        "Maximum stack depth: " <+> pretty (getMax maxDepth)
      ]
