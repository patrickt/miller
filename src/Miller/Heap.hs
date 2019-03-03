{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Miller.Heap
  ( Heap
  , Addr
  , count
  , initial
  , alloc
  , update
  , release
  , lookup
  , addresses
  ) where

import Prelude hiding (lookup)
import Doors

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.Stream.Infinite (Stream (..))
import qualified Data.Stream.Infinite as Stream

newtype Addr = Addr Int deriving (Eq, Lower)

instance Show Addr where show (Addr a) = '#' : show a
instance Pretty Addr where pretty = viaShow

data Heap a = Heap
  { count   :: Int
  , _fresh  :: Stream Int
  , entries :: IntMap a
  }

instance Show a => Show (Heap a) where show = show . entries
instance Show a => Pretty (Heap a) where pretty = viaShow

instance Lower (Heap a) where lowerBound = initial

initial :: Heap a
initial = Heap 0 (Stream.iterate succ 1) mempty

alloc :: a -> Heap a -> (Heap a, Addr)
alloc x (Heap c (next:>rest) es) = (Heap (succ c) rest new, Addr next) where
  new = IM.insert next x es

update :: Addr -> a -> Heap a -> Heap a
update (Addr a) n h = h { entries = IM.insert a n (entries h) }

release :: Addr -> Heap a -> Heap a
release (Addr a) (Heap c next es) = Heap (pred c) (a :> next) (IM.delete a es)

lookup :: Addr -> Heap a -> Maybe a
lookup (Addr a) h = IM.lookup a (entries h)

addresses :: Heap a -> [Addr]
addresses = fmap Addr . IM.keys . entries
