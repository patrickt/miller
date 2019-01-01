{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, OverloadedLists, OverloadedStrings,
             TypeApplications #-}

module Miller.TI where

import           Control.Effect
import           Control.Effect.Error
import           Control.Effect.State
import           Control.Effect.Writer
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Traversable
import           Miller.Expr
import           Miller.Heap as Heap
import           Miller.Stats

data Node
  = Ply Addr Addr
  | Super Name [Name] CoreExpr
  | Leaf Int

type TIHeap = Heap Node

type Stack = [Addr]

type Globals = HashMap Name Addr

data Dump = Dump

data Fatal
  = MainNotDefined

type Machine sig m
  = ( Member (State Stack)   sig
    , Member (State TIHeap)  sig
    , Member (State Dump)    sig
    , Member (State Globals) sig
    , Member (Writer Stats)  sig
    , Member (Error Fatal)   sig
    , Carrier sig m
    , Monad m
    )

buildInitialHeap :: Machine sig m => CoreProgram -> m ()
buildInitialHeap defns = for_ (unProgram defns) $ \(Defn name args body) -> do
  (newHeap, addr) <- gets (Heap.alloc (Super name args body))
  put @TIHeap newHeap
  modify @Globals (HM.insert name addr)

compile :: Machine sig m => CoreProgram -> m ()
compile program = do
  let preludeDefs = [] :: CoreProgram
      scDefs = program <> preludeDefs

  buildInitialHeap scDefs
  gets @Globals (HM.lookup "main") >>= maybe (throwError MainNotDefined) (put @Stack . pure)


