{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Miller.Stats
  ( Stats,
    steps,
    step,
    allocation,
    reduction,
    depth,
  )
where

import Control.Carrier.Writer.Strict
import Data.Monoid
import Data.Monoid.Generic
import Data.Semigroup
import Data.Text.Prettyprint.Doc
import Doors
import GHC.Generics

data Stats = Stats
  { steps :: Sum Int,
    allocations :: Sum Int,
    reductions :: Sum Int,
    maxDepth :: Max Int
  }
  deriving (Show, Generic)
  deriving (Semigroup) via GenericSemigroup Stats
  deriving (Monoid) via GenericMonoid Stats

step, allocation, reduction :: (Has (Writer Stats) sig m) => m ()
step = tell mempty {steps = 1}
allocation = tell mempty {allocations = 1}
reduction = tell mempty {reductions = 1}

depth :: (Has (Writer Stats) sig m) => Int -> m ()
depth n = tell mempty {maxDepth = Max n}

instance Pretty Stats where
  pretty Stats {..} =
    vcat
      [ "Steps:" <+> pretty (getSum steps),
        "Allocations:" <+> pretty (getSum allocations),
        "Reductions:" <+> pretty (getSum reductions),
        "Maximum stack depth: " <+> pretty (getMax maxDepth)
      ]
