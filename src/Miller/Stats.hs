{-# LANGUAGE DeriveGeneric, DerivingVia, OverloadedStrings, RecordWildCards #-}

module Miller.Stats where

import Data.Monoid
import Data.Monoid.Generic
import Data.Text.Prettyprint.Doc
import GHC.Generics

data Stats = Stats
  { _steps      :: Sum Int
  , _reductions :: Sum Int
  } deriving (Show, Generic)
    deriving Semigroup via GenericSemigroup Stats
    deriving Monoid    via GenericMonoid    Stats

instance Pretty Stats where
  pretty Stats{..} = vcat [ "Steps:" <+> pretty (getSum _steps)
                          , "Reductions:" <+> pretty (getSum _reductions)
                          ]
