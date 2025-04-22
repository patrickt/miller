module Miller.TI.Stack where

import Prelude qualified
import Prelude hiding (length)
import Prettyprinter qualified as Pretty
import Miller.TI.Heap (Addr)
import Doors

newtype Stack a = Stack { contents :: [a] }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid, Functor, Applicative)

instance Pretty.Pretty a => Pretty.Pretty (Stack a) where
  pretty (Stack st) = Pretty.align (Pretty.list (fmap Pretty.pretty st))

push :: a -> Stack a -> Stack a
push x (Stack s) = Stack (x : s)

replace :: a -> Stack a -> Stack a
replace x (Stack s) = Stack (x : drop 1 s)

length :: Stack a -> Int
length (Stack s) = Prelude.length s

nth :: Int -> Stack a -> a
nth x (Stack s) = s !! x

first :: Stack a -> Maybe a
first (Stack s) = listToMaybe s

pop :: Int -> Stack a -> Stack a
pop n (Stack s) = Stack (drop n s)
