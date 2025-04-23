module Miller.TI.Node where

import Miller.Expr (BinOp, CoreExpr, Name, UnOp)
import Miller.TI.Heap (Addr)
import Prettyprinter qualified as Pretty

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
