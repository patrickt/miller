module Doors
  ( module X,
    maybeM,
    withinState,
  )
where

import Control.Monad as X
import Control.Monad.IO.Class as X
import Data.Bool as X
import Data.Coerce as X
import Data.Foldable as X
import Data.Function as X
import Data.Maybe as X
import Data.Semilattice.Lower as X
import Prettyprinter as X (Pretty (..), viaShow)
import Data.Traversable as X
import Debug.Trace as X

import Optics (Is, A_Setter, A_Getter, Optic')
import Control.Effect.State
import Control.Effect.Optics

maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure

withinState :: (Is k A_Setter, Is k A_Getter, Has (State s) sig m) => Optic' k is s a -> (a -> (a, b)) -> m b
withinState lens fn = do
  (new, item) <- uses lens fn
  item <$ assign lens new


-- One day you'll go
-- Right now you're here
-- Don't leave just yet
-- Don't disappear
