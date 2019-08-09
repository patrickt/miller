module Doors
  ( module X
  , maybeM
  ) where

import Control.Monad as X
import Control.Monad.IO.Class as X
import Data.Coerce as X
import Data.Foldable as X
import Data.Maybe as X
import Data.Traversable as X
import Data.Semilattice.Lower as X
import Data.Text.Prettyprint.Doc as X (Pretty (..), viaShow)
import Debug.Trace as X

maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure

-- One day you'll go
-- Right now you're here
-- Don't leave just yet
-- Don't disappear
