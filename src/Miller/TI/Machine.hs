{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Miller.TI.Machine where

import GHC.Generics (Generic, Generically (..))
import Optics
import Prettyprinter qualified as Pretty
import Prettyprinter ((<+>))

import Doors hiding (find)
import Miller.Expr as Expr
import Miller.Stats (Stats)
import Miller.Stats qualified as Stats
import Miller.TI.Env qualified as Env
import Miller.TI.Heap (Addr, Heap)
import Miller.TI.Heap qualified as Heap
import Miller.TI.Stack qualified as Stack
import Miller.TI.Stack (Stack)
import Miller.TI.Node

data TIMachine = TIMachine
  { _stack :: Stack Addr,
    _heap :: Heap Node,
    _dump :: Stack (Stack Addr)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Monoid, Semigroup) via Generically TIMachine

makeLenses ''TIMachine

instance Pretty.Pretty TIMachine where
  pretty m =
    Pretty.vcat
      [ "stack" <+> "=" <+> m ^. stack % to pretty,
        "heap " <+> "=" <+> m ^. heap % to pretty,
        "dump"  <+> "=" <+> m ^. dump % to pretty
      ]


type Env = Env.Env Addr

-- Machines can be crashed, stopped, or active.
data Status
  = Crashed
  | Stopped
  | Active
  deriving (Eq, Show)

stoppedMachine :: TIMachine
stoppedMachine =
  TIMachine
    { _stack = Stack.Stack [lowerBound],
      _heap = Heap.update lowerBound (NNum 1) Heap.initial,
      _dump = mempty
    }

isFinal :: TIMachine -> Bool
isFinal m = machineStatus m /= Active

machineStatus :: TIMachine -> Status
machineStatus m =
  let decide x = if isDataNode x then Stopped else Active
   in case m ^. stack % to Stack.contents of
        [] -> Crashed
        [sole] | Stack.isEmpty (m ^. dump)
                 -> maybe Crashed decide (m ^. heap % to (Heap.lookup sole))
        _ -> Active
