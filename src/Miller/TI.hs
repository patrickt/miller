{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Miller.TI where

import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.State.Lazy
import Control.Carrier.Writer.Strict
import Control.Monad.Fix
import Data.Functor.Identity
import Data.List.NonEmpty qualified as NonEmpty
import Prettyprinter ((<+>))
import Prettyprinter qualified as Pretty
import Doors hiding (find)
import Miller.Expr as Expr
import Miller.Stats (Stats)
import Miller.Stats qualified as Stats
import Miller.TI.Env qualified as Env
import Miller.TI.Heap (Addr, Heap)
import Miller.TI.Heap qualified as Heap
import Miller.TI.Stack qualified as Stack
import Prelude hiding (lookup)

-- The dump records the state of the spine prior to the evaluation of
-- an argument of a strict primitive. Currently unused.
data Dump = Dump deriving (Eq, Show)

type Env = Env.Env Addr

data TIMachine = TIMachine
  { stack :: Stack.Stack,
    heap :: Heap Node -- maps Addr to Node
  }
  deriving (Eq, Show)

instance Pretty.Pretty TIMachine where
  pretty TIMachine {stack, heap} =
    Pretty.vcat
      [ "stack" <+> "=" <+> pretty stack,
        "heap " <+> "=" <+> pretty heap
      ]

instance Lower TIMachine where
  lowerBound = TIMachine mempty lowerBound

onStack :: (Stack.Stack -> Stack.Stack) -> TIMachine -> TIMachine
onStack f m = m { stack = f (stack m) }

data Status
  = Crashed
  | Stopped
  | Active
  deriving (Eq, Show)

stoppedMachine :: TIMachine
stoppedMachine =
  lowerBound
    { stack = Stack.Stack [lowerBound],
      heap = Heap.update lowerBound (NNum 1) Heap.initial
    }

isFinal :: TIMachine -> Bool
isFinal m = machineStatus m /= Active

machineStatus :: TIMachine -> Status
machineStatus TIMachine {stack, heap} =
  let decide x = if isDataNode x then Stopped else Active
   in case Stack.contents stack of
        [] -> Crashed
        [sole] -> maybe Crashed decide (Heap.lookup sole heap)
        _ -> Active

data Node
  = NAp Addr Addr
  | NSupercomb Name [Name] CoreExpr
  | NNum Int
  | NInd Addr
  | NPrim BinOp
  deriving (Eq, Show)

instance Pretty.Pretty Node where pretty = Pretty.viaShow

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _ = False

data TIFailure
  = EmptyStack
  | NumberAppliedAsFunction
  | UnboundName Name
  | DeadPointer Addr
  | TooFewArguments Int Int
  | BadArgument Node
  | Unimplemented String
  deriving (Eq, Show)

type TI sig m =
  ( Has (State TIMachine) sig m,
    Has (Reader Env) sig m,
    Has (Writer Stats) sig m,
    Has (Error TIFailure) sig m,
    MonadFix m
  )

runTI :: ReaderC Env (ErrorC TIFailure (WriterC Stats (StateC TIMachine Identity))) a -> (Either TIFailure a, TIMachine, Stats)
runTI = runTI' lowerBound

runTI' :: TIMachine -> ReaderC Env (ErrorC TIFailure (WriterC Stats (StateC TIMachine Identity))) a -> (Either TIFailure a, TIMachine, Stats)
runTI' start go =
  let flatten (a, (b, c)) = (c, a, b)
   in flatten . run . runState start . runWriter . runError . runReader @Env mempty $ go

-- Register an item in the heap
store :: TI sig m => Node -> m Addr
store n = do
  (new, item) <- gets (Heap.alloc n . heap)
  modify (\m -> m {heap = new})
  item <$ Stats.allocation

-- Register a supercombinator in the heap.
allocateSC :: TI sig m => Expr.CoreDefn -> m (Name, Addr)
allocateSC (Expr.Defn name args body) = do
  addr <- store (NSupercomb name args body)
  pure (name, addr)

-- Evaluate the loaded program, returning the set of all seen states.
eval :: TI sig m => m [TIMachine]
eval = execWriter @[TIMachine] go
  where
    go = do
      curr <- get
      tell @[TIMachine] [curr]
      case machineStatus curr of
        Crashed -> throwError EmptyStack
        Stopped -> pure ()
        Active -> step *> go

stackHead :: TI sig m => m Addr
stackHead = gets (Stack.first . stack) >>= maybeM (throwError EmptyStack)

step :: TI sig m => m ()
step =
  stackHead >>= find >>= \case
    NNum n -> numStep n
    NAp f x -> apStep f x
    NSupercomb name args body -> scStep name args body
    NInd addr -> indStep addr
    NPrim op -> primStep op

indStep :: TI sig m => Addr -> m ()
indStep a = modify (onStack (Stack.replace a))

numStep :: TI sig m => Int -> m ()
numStep _ = throwError NumberAppliedAsFunction

-- As per the unwind rule, we look leftwards and downwards
-- to get the current thing to apply, so we only look in 'f'.
apStep :: TI sig m => Addr -> Addr -> m ()
apStep f _x = modify (onStack (Stack.push f))

lookup :: TI sig m => Name -> m Addr
lookup n = asks (Env.lookup n) >>= maybeM (throwError (UnboundName n))

find :: TI sig m => Addr -> m Node
find a = gets (Heap.lookup a . heap) >>= maybeM (throwError (DeadPointer a))

argumentAddresses :: TI sig m => m [Addr]
argumentAddresses = do
  Stack.Stack s <- gets stack
  case s of
    [] -> throwError EmptyStack
    (_sc : items) -> traverse go items
      where
        go addr =
          find addr >>= \case
            NAp _fun arg -> pure arg
            NInd arg -> pure arg
            n -> throwError (BadArgument n)

primStep :: TI sig m => BinOp -> m ()
primStep = throwError . Unimplemented . show

-- UPDATES WILL BE PERFORMED HERE
scStep :: TI sig m => Name -> [Name] -> CoreExpr -> m ()
scStep _name args body = do
  given <- gets (pred . Stack.length . stack)
  let needed = length args
  when (given < needed) (throwError (TooFewArguments given needed))
  Stats.depth given

  root <- gets ((Stack.nth (length args)) . stack)
  -- Bind argument names to addresses
  argBindings <- Env.fromBindings args <$> argumentAddresses
  newEnviron <- mappend argBindings <$> ask
  h <- gets heap
  let instantiated = run $ runError @TIFailure $ runReader newEnviron $ runState h $ instantiateWithUpdate body root
  (newHeap, result) <- either throwError pure instantiated
  modify (\m -> m {heap = newHeap})

  -- Discard arguments from stack (including root)
  Stack.Stack st <- gets stack
  let (currArgs, rest) = splitAt (length args + 1) st

  -- Push result onto stack
  modify
    ( \m ->
        m
          { stack = Stack.Stack (result : rest)
          }
    )

  Stats.reduction

compile :: TI sig m => CoreProgram -> m [TIMachine]
compile ps = do
  toplevels <- Env.fromList <$> traverse allocateSC (unProgram (preludeDefs <> ps))
  local (mappend toplevels) $ do
    main <- lookup "main"
    modify (onStack (Stack.push main))
    eval

execute :: TI sig m => CoreProgram -> m Node
execute p = do
  void $ compile p
  stackHead >>= find

-- | This function diverges if it is called inside a strict monad,
-- so we can't use it with Writer.
instantiate ::
  ( Has (Reader Env) sig m,
    Has (State (Heap Node)) sig m,
    MonadFix m
  ) =>
  CoreExpr ->
  m Addr
instantiate e = case e of
  Expr.Num i -> state (Heap.alloc (NNum i))
  Expr.Var n -> do
    item <- Env.lookup n <$> ask
    pure (fromMaybe (error (show n)) item)
  Expr.Ap f x -> do
    fore <- instantiate f
    aft <- instantiate x
    state (Heap.alloc (NAp fore aft))
  Let Non binds bod -> do
    -- Straightforward, nonrecursive lets
    newVals <- traverse (instantiate . snd) binds
    let newBindings = Env.fromList (NonEmpty.zip (fmap fst binds) newVals)
    local (newBindings <>) (instantiate bod)
  Let Rec binds bod -> mdo
    -- newVals is defined in terms of newBindings, and vice versa
    -- laziness is absolute witchcraft and I hate/love it
    newVals <- traverse (local (newBindings <>) . instantiate . snd) binds
    let newBindings = Env.fromList (NonEmpty.zip (fmap fst binds) newVals)
    local (newBindings <>) (instantiate bod)
  other -> error ("unimplemented: " <> show other)

instantiateWithUpdate ::
  ( Has (Reader Env) sig m,
    Has (State (Heap Node)) sig m,
    Has (Error TIFailure) sig m,
    MonadFix m
  ) =>
  CoreExpr ->
  Addr ->
  m Addr
instantiateWithUpdate e dest = do
  case e of
    Expr.Num i -> modify (Heap.update dest (NNum i))
    Expr.Ap a b -> do
      fore <- instantiate a
      aft <- instantiate b
      modify (Heap.update dest (NAp fore aft))
    Expr.Var n -> do
      item <- Env.lookup n <$> ask @Env
      case item of
        Nothing -> throwError (UnboundName n)
        Just addr -> modify (Heap.update dest (NInd addr))
    Expr.Let {} -> do
      addr <- instantiate e
      modify (Heap.update dest (NInd addr))
    other -> error ("unimplemented: " <> show other)
  pure dest
