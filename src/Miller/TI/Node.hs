module Miller.TI.Node where

import Prettyprinter qualified as Pretty

import Miller.TI.Heap (Addr, Heap)
import Miller.Expr (CoreExpr, UnOp, BinOp, Name)

-- Nodes in the TI graph.
data Node
  = NAp Addr Addr
  | NSupercomb Name [Name] CoreExpr
  | NNum Int
  | NInd Addr
  | NPrim (Either UnOp BinOp)
  deriving (Eq, Show)


instance Pretty.Pretty Node where pretty = Pretty.viaShow

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _ = False
