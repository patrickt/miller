{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Miller.TI.Heap
  ( Heap,
    Addr,
    unallocated,
    count,
    initial,
    alloc,
    update,
    release,
    lookup,
    addresses,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Stream.Infinite (Stream (..))
import Data.Stream.Infinite qualified as Stream
import Doors
import Prettyprinter qualified as Pretty
import Prelude hiding (lookup)
import Optics.At
import Optics.Optic
import Optics.Label ()
import Optics.TH

newtype Addr = Addr Int deriving (Eq, Ord, Lower)

unallocated :: Addr
unallocated = Addr (negate 1)

instance Show Addr where show (Addr a) = '#' : show a

instance Pretty Addr where pretty = viaShow

data Heap a = Heap
  { count :: Int,
    fresh :: Stream Int,
    entries :: IntMap a
  }

makeFieldLabelsNoPrefix ''Heap  

instance (Eq a) => Eq (Heap a) where (==) = (==) `on` entries

instance (Show a) => Show (Heap a) where show = show . entries

instance (Pretty a) => Pretty (Heap a) where
  pretty (Heap _ _ e) = do
    let pair (a, b) = pretty a <> ": " <> pretty b
    let elements = fmap pair (IM.toList e)
    Pretty.align (Pretty.list elements)

type instance Index (Heap a) = Addr
type instance IxValue (Heap a) = a

instance Ixed (Heap a) where
  ix (Addr i) = #entries % ix i

instance Semigroup (Heap a) where
  a <> b =
    Heap
      { count = (max `on` count) a b,
        fresh = fresh (if Stream.head (fresh a) > Stream.head (fresh b) then a else b),
        entries = (mappend `on` entries) a b
      }

instance Monoid (Heap a) where mempty = initial

initial :: Heap a
initial = Heap 0 (Stream.iterate succ 1) mempty

alloc :: a -> Heap a -> (Heap a, Addr)
alloc x (Heap c (next :> rest) es) = (Heap (succ c) rest new, Addr next)
  where
    new = IM.insert next x es

update :: Addr -> a -> Heap a -> Heap a
update (Addr a) n h = h {entries = IM.insert a n (entries h)}

release :: Addr -> Heap a -> Heap a
release (Addr a) (Heap c next es) = Heap (pred c) (a :> next) (IM.delete a es)

lookup :: Addr -> Heap a -> Maybe a
lookup (Addr a) h = IM.lookup a (entries h)

addresses :: Heap a -> [Addr]
addresses = fmap Addr . IM.keys . entries
