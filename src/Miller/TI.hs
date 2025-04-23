{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Miller.TI where

import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.State.Lazy
import Control.Carrier.Writer.Strict
import Control.Effect.Optics
import Control.Carrier.Lift
import Control.Monad.Fix
import Data.List.NonEmpty qualified as NonEmpty
import Data.Sequence (Seq)

import Doors hiding (find)
import Miller.Expr as Expr
import Miller.Stats (Stats)
import Miller.Stats qualified as Stats
import Miller.TI.Env (Env)
import Miller.TI.Env qualified as Env
import Miller.TI.Heap (Addr)
import Miller.TI.Heap qualified as Heap
import Miller.TI.Stack qualified as Stack
import Miller.TI.Error qualified as Error
import Miller.TI.Error (TIFailure)
import Miller.TI.Machine (Machine, DebugMode (..), debugger)
import Miller.TI.Machine qualified as Machine
import Miller.TI.Node

import Prelude hiding (lookup)

type TI sig m =
  ( Has (State Machine) sig m,
    Has (Reader (Env Addr)) sig m,
    Has (Reader DebugMode) sig m,
    Has (Writer Stats) sig m,
    Has (Error Error.TIFailure) sig m,
    MonadFix m,
    MonadIO m
  )

-- Keeps track of an immutable environment, a lazy machine state,
-- stats, and failure (this last is hinky due to laziness).
type TIMonad = ReaderC (Env Addr)
               (ReaderC DebugMode
                (ErrorC TIFailure
                  (WriterC Stats
                   (StateC Machine
                    (LiftC IO)
                   )
                  )
                )
               )

-- Run a template instantiation invocation with an empty starting state.
runTI :: MonadIO m => TIMonad a -> m (Either TIFailure a, Machine, Stats)
runTI = runTI' mempty Run

-- Run a template instantiation invocation with an empty starting state.
debugTI :: MonadIO m => TIMonad a -> m (Either TIFailure a, Machine, Stats)
debugTI = runTI' mempty Debug

-- Run a template instantiation invocation with a specified starting state.
runTI' :: MonadIO m =>
  Machine ->
  DebugMode ->
  TIMonad a ->
  m (Either TIFailure a, Machine, Stats)
runTI' start dbg go =
  let flatten (a, (b, c)) = (c, a, b)
   in fmap flatten
      . liftIO
      . runM
      . runState start
      . runWriter
      . runError
      . runReader dbg
      . runReader (mempty :: Env Addr)
      $ go

-- Register a node on the heap, returning its new address.
store :: TI sig m => Node -> m Addr
store n = withinState Machine.heap (Heap.alloc n) <* Stats.allocation

-- Look up the address bound to a given name, or throw 'UnboundName'.
lookup :: TI sig m => Name -> m Addr
lookup n = asks (Env.lookup n) >>= maybeM (Error.unboundName n)

-- Find a node in the heap by address, or throw 'DeadPointer'.
find :: TI sig m => Addr -> m Node
find a = uses Machine.heap (Heap.lookup a) >>= maybeM (Error.deadPointer a)

-- Register a supercombinator in the heap.
allocateSC :: TI sig m => Expr.CoreDefn -> m (Name, Addr)
allocateSC (Expr.Defn name args body) = do
  addr <- store (NSupercomb name args body)
  pure (name, addr)

-- Get the node on the top of the stack, or throw 'EmptyStack'/'DeadPointer'.
stackHead :: TI sig m => m Node
stackHead = uses Machine.stack Stack.first >>= maybeM Error.emptyStack >>= find

-- Register a primitive operator in the heap and store.
allocatePrim :: TI sig m => Name -> Either UnOp BinOp -> m (Name, Addr)
allocatePrim name op = do
  addr <- store (NPrim op)
  pure (name, addr)

-- For each item on the N most recent stack entries, extract their argument
-- (if an Ap node) or their indirect target (if an Ind node) or throw 'BadArgument'.
stackContents :: TI sig m => Int -> m [Addr]
stackContents needed = do
  s <- uses Machine.stack (Stack.contents . Stack.take needed)
  when (null s) Error.emptyStack
  forM (drop 1 s) $
    find >=> \case
      NAp _fun arg -> pure arg
      NInd arg -> pure arg
      n -> Error.badArgument n

-- Run a machine until it crashes or stops, returning the set of all seen states.
eval :: TI sig m => m (Seq Machine)
eval = execWriter @(Seq Machine) go
  where
    go = do
      curr <- get
      tell (pure @Seq curr)
      case Machine.machineStatus curr of
        Machine.Crashed -> Error.emptyStack
        Machine.Stopped -> pure ()
        Machine.Active -> step *> go

-- Run one step of template instantiation.
step :: TI sig m => m ()
step =
  stackHead >>= \case
    NNum n -> numStep n
    NAp f x -> apStep f x
    NSupercomb name args body -> scStep name args body
    NInd addr -> indStep addr
    NPrim op -> primStep op

-- Indirect step: replace the top value with the value pointed to.
indStep :: TI sig m => Addr -> m ()
indStep a = modifying Machine.stack (Stack.replace a)

-- Num step: crash.
numStep :: TI sig m => Int -> m ()
numStep _ = Error.numberAppliedAsFunction

-- Ap step: push the function onto the stack.
-- As per the unwind rule, we look leftwards and downwards
-- to get the current thing to apply, so we only look in 'f'.
apStep :: TI sig m => Addr -> Addr -> m ()
apStep f _x = modifying Machine.stack (Stack.push f)

-- Primitive step (unimplemented)
primStep :: TI sig m => Either UnOp BinOp -> m ()
primStep (Left x) = Error.unimplemented x
primStep (Right op) = Error.unimplemented op
  -- entries <- uses stack (take 3 . Stack.contents)
  -- given <- traverse find entries
  -- let recur item = find item >>= \case
  --       NNum x' -> pure x'
  --       NInd x' -> recur x'
  --       NAp left right -> do  --         currStack <- uses stack
  --         modifying dump (Stack.push currStack)
  --         assign stack (pure (NAp left right))
  --         pure Nothing
  -- case given of
  --   [_, NAp _op left, NAp _prior right] -> do
  --     x <- recur left
  --     case x of
  --       Nothing -> pure ()
  --       Just ok -> do
  --         y <- recur right
  --         case y of
  --           Nothing -> pure ()
  --           Just ok2 ->  do
  --             let node = NNum (ok + ok2)
  --             added <- store node
  --             modifying stack (Stack.push added . Stack.pop 3)
  --             modifying heap (Heap.update (head entries) node)
  --   _ -> error ("badarg: " <> show given)

-- Supercombinator step: apply a function, given args and body
scStep :: TI sig m => Name -> [Name] -> CoreExpr -> m ()
scStep _name args body = do
  -- record depth (number of dumps)
  Stats.depth =<< uses Machine.dump Stack.length

  -- Check arity and extract the root
  given <- uses Machine.stack (pred . Stack.length)
  let needed = length args
  root <- if (given < needed)
    then Error.tooFewArguments given needed
    else uses Machine.stack (Stack.nth needed)

  debugger "scStep: before instantiation"

  -- Bind argument names to addresses
  argBindings <- Env.fromBindings args <$> stackContents (succ needed)
  local (mappend argBindings) (instantiateWithUpdate body root)

  debugger "scStep: after instantiation"

  -- Discard arguments from stack (including root)
  modifying Machine.stack (Stack.push root . Stack.pop (succ needed))
  Stats.reduction

-- Given a redex, instantiate it and return its address on the stack.
instantiate ::
  TI sig m =>
  CoreExpr ->
  m Addr
instantiate e = case e of
  Expr.Num i -> withinState Machine.heap (Heap.alloc (NNum i))
  Expr.Var n -> do
    item <- Env.lookup n <$> ask
    pure (fromMaybe (error ("unbound name: " <> show n)) item)
  Expr.Ap f x -> do
    fun <- instantiate f
    arg <- instantiate x
    store (NAp fun arg)
  Let Non binds bod -> do
    -- Straightforward, nonrecursive lets
    newVals <- traverse (instantiate . snd) binds
    let newBindings = Env.fromList (NonEmpty.zip (fmap fst binds) newVals)
    local (newBindings <>) (instantiate bod)
  Let Rec binds bod -> mdo
    -- newVals is defined in terms of newBindings, and vice versa
    -- laziness is absolute witchcraft and I hate/love it
    newVals <- traverse (local (mappend newBindings) . instantiate . snd) binds
    let newBindings = Env.fromList (NonEmpty.zip (fmap fst binds) newVals)
    local (newBindings <>) (instantiate bod)
  Binary op left right ->
    instantiate (Expr.Ap (Expr.Ap (Expr.Var (binOpToName op)) left) right)
  Unary op arg ->
    instantiate (Expr.Ap (Expr.Var (unOpToName op)) arg)
  other -> error ("unimplemented: " <> show other)

-- Given a redex and its address in memory, update it with its instantiation.
instantiateWithUpdate ::
  TI sig m =>
  CoreExpr ->
  Addr ->
  m ()
instantiateWithUpdate e dest = do
  case e of
    Expr.Num i -> modifying Machine.heap (Heap.update dest (NNum i))
    Expr.Ap a b -> do
      fore <- instantiate a
      aft <- instantiate b
      modifying Machine.heap (Heap.update dest (NAp fore aft))
    Expr.Var n -> do
      item <- Env.lookup n <$> ask
      case item of
        Nothing -> Error.unboundName n
        Just addr -> modifying Machine.heap (Heap.update dest (NInd addr))
    Expr.Let {} -> do
      addr <- instantiate e
      modifying Machine.heap (Heap.update dest (NInd addr))
    Binary op left right -> do
      addr <- instantiate (Expr.Ap (Expr.Ap (Expr.Var (binOpToName op)) left) right)
      modifying Machine.heap (Heap.update dest (NInd addr))
    Unary op arg -> do
      addr <- instantiate (Expr.Ap (Expr.Var (unOpToName op)) arg)
      modifying Machine.heap (Heap.update dest (NInd addr))
    other -> Error.unimplemented other

-- Compile and run a program, returning the set of all seen states.
compile :: TI sig m => CoreProgram -> m (Seq Machine)
compile ps = do
  toplevels <- Env.fromList <$> traverse allocateSC (unProgram (preludeDefs <> ps))
  builtins <- Env.fromList <$> traverse (uncurry allocatePrim) Machine.operators
  local (mappend toplevels . mappend builtins) $ do
    main <- lookup "main"
    modifying Machine.stack (Stack.push main)
    eval

-- Compile and run a program, returning the top of the stack.
execute :: TI sig m => CoreProgram -> m Node
execute p = compile p *> stackHead
