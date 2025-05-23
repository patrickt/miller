{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miller.TI.Stack where

import Doors
import Optics.Empty
import Optics.Label ()
import Optics.Optic
import Optics.TH
import Prettyprinter qualified as Pretty
import Prelude hiding (drop, length, take)
import Prelude qualified

newtype Stack a = Stack {contents :: [a]}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid, Functor, Applicative, Foldable)

makeFieldLabelsNoPrefix ''Stack

instance AsEmpty (Stack a) where
  _Empty = #contents % _Empty

instance (Pretty.Pretty a) => Pretty.Pretty (Stack a) where
  pretty (Stack st) = Pretty.align (Pretty.list (fmap Pretty.pretty st))

solo :: Stack a -> Maybe a
solo (Stack [s]) = Just s
solo _ = Nothing

push :: a -> Stack a -> Stack a
push x (Stack s) = Stack (x : s)

replace :: a -> Stack a -> Stack a
replace x (Stack s) = Stack (x : Prelude.drop 1 s)

length :: Stack a -> Int
length (Stack s) = Prelude.length s

nth :: Int -> Stack a -> a
nth x (Stack s) = s !! x

first :: Stack a -> Maybe a
first (Stack s) = listToMaybe s

drop :: Int -> Stack a -> Stack a
drop n (Stack s) = Stack (Prelude.drop n s)

popFront :: Stack a -> (Stack a, Maybe a)
popFront (Stack (a : as)) = (Stack as, Just a)
popFront a = (a, Nothing)

pop :: Int -> Stack a -> (Stack a, Stack a)
pop n (Stack s) = (Stack rest, Stack prefix) where (prefix, rest) = splitAt n s

take :: Int -> Stack a -> Stack a
take n (Stack s) = Stack (Prelude.take n s)

isEmpty :: Stack a -> Bool
isEmpty = null . contents
