{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, LambdaCase, OverloadedLists, OverloadedStrings,
             TypeApplications, BlockArguments #-}

module Miller.TI where

import           Control.Effect
import           Control.Effect.Error
import           Control.Effect.State
import           Control.Effect.Writer
import           Control.Monad
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

isDataNode :: Node -> Bool
isDataNode (Leaf _) = True
isDataNode _ = False

type TIHeap = Heap Node

type Stack = [Addr]

type Globals = HashMap Name Addr

data Dump = Dump

data Fatal
  = MainNotDefined
  | EmptyStack
  | AddrNotFound Addr
  | NumAppliedAsFunction Int
  | ExpectedSCPly Node

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

eval :: Machine sig m => m ()
eval = do
  final <- isFinal
  unless final $ do
    tell (Stats 1 0)
    step *> eval

step :: Machine sig m => m ()
step = do
  stack <- get @Stack
  when (null stack) (throwError EmptyStack)
  lookupHeap (head stack) >>= \case
    Leaf n  -> throwError (NumAppliedAsFunction n)
    Ply a b -> modify @Stack (a:)
    Super name args body -> do
      bindings <- zip args <$> pendingArguments
      (newHeap, result) <- instantiate body bindings
      put @Stack (result : drop (succ (length args)) stack)
      put @TIHeap newHeap

pendingArguments :: Machine sig m => m [Addr]
pendingArguments = do
  pending <- tail <$> get @Stack
  forM pending $ \a -> do
    res <- lookupHeap a
    case res of
      Ply _ arg -> pure a
      _         -> throwError (ExpectedSCPly res)
      
instantiate :: Machine sig m => CoreExpr -> [(Name, Addr)] -> m (TIHeap, Addr)
instantiate ex env = case ex of
  Num i  -> gets (alloc (Leaf i))
  Ap f x -> do
    (h, n) <- instantiate f env
    put @TIHeap h
    (_, m) <- instantiate x env
    gets (alloc (Ply n m))
  

lookupHeap :: Machine sig m => Addr -> m Node
lookupHeap a = gets (Heap.lookup a) >>= maybe (throwError (AddrNotFound a)) pure

isFinal :: Machine sig m => m Bool
isFinal = get @Stack >>= \case
  []  -> throwError EmptyStack
  [a] -> isDataNode <$> lookupHeap a
  _   -> pure False
