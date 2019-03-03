{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments, ConstraintKinds, FlexibleContexts, FlexibleInstances, LambdaCase, OverloadedLists,
             OverloadedStrings, TupleSections, TypeApplications #-}

module Miller.TI where

import Doors

import           Control.Effect
import           Control.Effect.Error
import           Control.Effect.State
import           Control.Effect.Writer
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text.Prettyprint.Doc
import           Miller.Expr
import           Miller.Heap as Heap
import           Miller.Stats

data Node
  = Ply Addr Addr
  | Super Name [Name] CoreExpr
  | Leaf Int
    deriving (Eq, Show)

isDataNode :: Node -> Bool
isDataNode (Leaf _) = True
isDataNode _        = False

type TIHeap = Heap Node

type Stack = [Addr]

type Globals = HashMap Name Addr

data Dump = Dump

data Fatal
  = NotDefined Name
  | EmptyStack
  | AddrNotFound Addr
  | NumAppliedAsFunction Int
  | ExpectedSCPly Node
    deriving (Eq, Show)

instance Pretty Fatal where pretty = viaShow

instance Pretty a => Pretty (Either Fatal a) where
  pretty (Left f)  = "ERROR:" <+> pretty f
  pretty (Right v) = "SUCCESS:" <+> pretty v

notDefined :: (Member (Error Fatal) sig, Carrier sig m) => Name -> m a
notDefined = throwError . NotDefined

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

data Result a = Result
  { _stack   :: Stack
  , _heap    :: TIHeap
  , _globals :: Globals
  , _stats   :: Stats
  , _result  :: Either Fatal a

  } deriving (Show)

instance Pretty a => Pretty (Result a) where
  pretty Result {..} = vcat [ "result:  " <> pretty _result
                            , "stack:   " <> pretty _stack
                            , "heap:    " <> pretty _heap
                            , "globals: " <> viaShow _globals
                            , "***"
                            , pretty _stats
                            ]

assemble :: (Stack, (TIHeap, (Globals, (Stats, Either Fatal a)))) -> Result a
assemble (a, (b, (c, (d, e)))) = Result a b c d e

runTemplate :: CoreProgram -> Doc ()
runTemplate
  = pretty
  . assemble
  . run
  . runState @Stack lowerBound
  . runState @TIHeap lowerBound
  . evalState Dump
  . runState @Globals mempty
  . runWriter @Stats
  . runError @Fatal
  . compile

buildInitialHeap :: Machine sig m => CoreProgram -> m ()
buildInitialHeap defns = for_ (unProgram defns) $ \(Defn name args body) -> do
  (newHeap, addr) <- gets (Heap.alloc (Super name args body))
  put @TIHeap newHeap
  modify @Globals (HM.insert name addr)

preludeDefs :: CoreProgram
preludeDefs = [Defn "identity" ["x"] (Var "x")]

compile :: Machine sig m => CoreProgram -> m ()
compile program = do
  buildInitialHeap (program <> preludeDefs)
  gets @Globals (HM.lookup "main") >>= maybe (throwError (NotDefined "main")) (put @Stack . pure)

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
    Leaf n                -> throwError (NumAppliedAsFunction n)
    Ply a _b              -> modify @Stack (a:)
    Super _name args body -> do
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
      Ply _ arg -> pure arg
      _         -> throwError (ExpectedSCPly res)

instantiate :: Machine sig m => CoreExpr -> [(Name, Addr)] -> m (TIHeap, Addr)
instantiate ex env = case ex of
  Num i  -> gets (alloc (Leaf i))
  Var n  -> do
    addr <- gets @Globals (HM.lookup n) >>= maybeM (notDefined n)
    heap <- get
    pure (heap, addr)
  Ap f x -> do
    (h, n) <- instantiate f env
    put @TIHeap h
    (_, m) <- instantiate x env
    gets (alloc (Ply n m))
  _ -> error ("instantiate: got " <> show ex)


lookupHeap :: Machine sig m => Addr -> m Node
lookupHeap a = gets (Heap.lookup a) >>= maybe (throwError (AddrNotFound a)) pure

isFinal :: Machine sig m => m Bool
isFinal = get @Stack >>= \case
  []  -> throwError EmptyStack
  [a] -> isDataNode <$> lookupHeap a
  _   -> pure False
