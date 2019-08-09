{-# LANGUAGE DeriveGeneric, DerivingVia, OverloadedStrings, RecordWildCards #-}

module Miller.Stats
  ( Stats
  , steps
  , step
  , allocation
  , reduction
  , depth
  ) where

import Doors

import Control.Effect
import Control.Effect.Writer
import Data.Monoid
import Data.Semigroup
import Data.Monoid.Generic
import Data.Text.Prettyprint.Doc
import GHC.Generics

data Stats = Stats
  { steps       :: Sum Int
  , allocations :: Sum Int
  , reductions  :: Sum Int
  , maxDepth    :: Max Word
  } deriving (Show, Generic)
    deriving Semigroup via GenericSemigroup Stats
    deriving Monoid    via GenericMonoid    Stats

step, allocation, reduction :: (Member (Writer Stats) sig, Carrier sig m) => m ()
step       = tell mempty { steps = 1 }
allocation = tell mempty { allocations = 1 }
reduction  = tell mempty { reductions = 1 }

depth :: (Member (Writer Stats) sig, Carrier sig m) => Max Word -> m ()
depth n = tell mempty { maxDepth = n }

instance Pretty Stats where
  pretty Stats{..} = vcat [ "Steps:" <+> pretty (getSum steps)
                          , "Allocations:" <+> pretty (getSum allocations)
                          , "Reductions:" <+> pretty (getSum reductions)
                          , "Maximum stack depth: " <+> pretty (getMax maxDepth)
                          ]
