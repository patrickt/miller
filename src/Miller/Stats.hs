{-# LANGUAGE DeriveGeneric, DerivingVia, OverloadedStrings, RecordWildCards #-}

module Miller.Stats
  ( Stats
  , step
  , allocation
  ) where

import Doors

import Control.Effect
import Control.Effect.Writer
import Data.Monoid
import Data.Monoid.Generic
import Data.Text.Prettyprint.Doc
import GHC.Generics

data Stats = Stats
  { steps       :: Sum Int
  , allocations :: Sum Int
  } deriving (Show, Generic)
    deriving Semigroup via GenericSemigroup Stats
    deriving Monoid    via GenericMonoid    Stats

step, allocation :: (Member (Writer Stats) sig, Carrier sig m) => m ()
step = tell mempty{ steps = 1 }
allocation = tell mempty { allocations = 1 }

instance Pretty Stats where
  pretty Stats{..} = vcat [ "Steps:" <+> pretty (getSum steps)
                          , "Allocations:" <+> pretty (getSum allocations)
                          ]
