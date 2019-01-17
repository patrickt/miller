module Miller.Heap
  ( Heap
  , Addr
  , count
  , initial
  , alloc
  , update
  , release
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

newtype Addr = Addr { unAddr :: Int } deriving Eq

instance Show Addr where show (Addr a) = '#' : show a

null :: Addr
null = Addr 0

data Heap a = Heap
  { count   :: Int
  , fresh   :: [Int]
  , entries :: IntMap a
  }

initial :: Heap a
initial = Heap 0 [1..] mempty

alloc :: a -> Heap a -> (Heap a, Addr)
alloc x (Heap c (next:rest) es) = (Heap (succ c) rest new, Addr next) where
  new = IM.insert next x es

update :: Addr -> a -> Heap a -> Heap a
update (Addr a) n h = h { entries = IM.insert a n (entries h) }

release :: Addr -> Heap a -> Heap a
release (Addr a) (Heap c next es) = Heap (pred c) (a:next) (IM.delete a es)

lookup :: Addr -> Heap a -> Maybe a
lookup (Addr a) h = IM.lookup a (entries h)

addresses :: Heap a -> [Addr]
addresses = fmap Addr . IM.keys . entries

