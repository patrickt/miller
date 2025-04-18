module Miller.TI.Stack where

import Prelude qualified
import Prelude hiding (length)
import Prettyprinter qualified as Pretty
import Miller.TI.Heap (Addr)
import Doors

newtype Stack = Stack { contents :: [Addr] }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

instance Pretty.Pretty Stack where
  pretty (Stack st) = Pretty.align (Pretty.list (fmap Pretty.pretty st))

push :: Addr -> Stack -> Stack
push x (Stack s) = Stack (x : s)

replace :: Addr -> Stack -> Stack
replace x (Stack s) = Stack (x : drop 1 s)

length :: Stack -> Int
length (Stack s) = Prelude.length s

nth :: Int -> Stack -> Addr
nth x (Stack s) = s !! x

first :: Stack -> Maybe Addr
first (Stack s) = listToMaybe s

pop :: Int -> Stack -> Stack
pop n (Stack s) = Stack (drop n s)
