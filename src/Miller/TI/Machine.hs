{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Miller.TI.Machine where

import Control.Effect.Reader
import Control.Effect.State
import GHC.Generics (Generic, Generically (..))
import Optics
import Prettyprinter qualified as Pretty
import Prettyprinter ((<+>))
import System.IO

import Doors hiding (find)
import Miller.Expr as Expr
import Miller.Pretty (renderPrint)
import Miller.TI.Heap (Addr, Heap)
import Miller.TI.Heap qualified as Heap
import Miller.TI.Stack qualified as Stack
import Miller.TI.Stack (Stack)
import Miller.TI.Node

data Machine = Machine
  { _stack :: Stack Addr,
    _heap :: Heap Node,
    _dump :: Stack (Stack Addr)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Monoid, Semigroup) via Generically Machine

makeLenses ''Machine

instance Pretty.Pretty Machine where
  pretty m =
    Pretty.vcat
      [ "stack" <+> "=" <+> m ^. stack % to pretty,
        "heap " <+> "=" <+> m ^. heap % to pretty,
        "dump"  <+> "=" <+> m ^. dump % to pretty
      ]


-- Machines can be crashed, stopped, or active.
data Status
  = Crashed
  | Stopped
  | Active
  deriving (Eq, Show)

stoppedMachine :: Machine
stoppedMachine =
  Machine
    { _stack = Stack.Stack [lowerBound],
      _heap = Heap.update lowerBound (NNum 1) Heap.initial,
      _dump = mempty
    }

isFinal :: Machine -> Bool
isFinal m = machineStatus m /= Active

machineStatus :: Machine -> Status
machineStatus m =
  let decide x = if isDataNode x then Stopped else Active
   in case m ^. stack % to Stack.contents of
        [] -> Crashed
        [sole] | Stack.isEmpty (m ^. dump)
                 -> maybe Crashed decide (m ^. heap % to (Heap.lookup sole))
        _ -> Active

-- All operators supported in the TI machine.
operators :: [(Name, Either Expr.UnOp Expr.BinOp)]
operators = [("*", Right Expr.Mul),
             ("+", Right Expr.Add),
             ("-", Right Expr.Sub),
             ("~", Left Expr.Neg)
             ]

data DebugMode = Run | Debug
  deriving (Show, Eq)

debugger ::
  (MonadIO m, Has (Reader DebugMode) sig m, Has (State Machine) sig m) =>
  String ->
  m ()
debugger msg = do
  dbg <- ask
  mach <- get @Machine
  when (dbg == Debug) $ liftIO $ do
    putStrLn ("Debug: " <> msg)
    renderPrint mach
    hFlush stdout
    putStr ">>> "
    hFlush stdout
    void getLine
