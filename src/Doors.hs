module Doors
  ( module X
  , maybeM
  ) where

import Control.Monad as X
import Data.Semilattice.Lower as X
import Data.Text.Prettyprint.Doc as X (Pretty (..), viaShow)
import Data.Maybe as X

maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure

-- One day you'll go
-- Right now you're here
-- Don't leave just yet
-- Don't disappear
