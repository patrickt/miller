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
import Control.Monad.Fix
import Data.Functor.Identity
import Data.List.NonEmpty qualified as NonEmpty
import Data.Sequence (Seq)

import Doors hiding (find)
import Miller.Expr as Expr
import Miller.Stats (Stats)
import Miller.Stats qualified as Stats
import Miller.TI.Env qualified as Env
import Miller.TI.Heap (Addr, Heap)
import Miller.TI.Heap qualified as Heap
import Miller.TI.Stack qualified as Stack
import Miller.TI.Error qualified as Error
import Miller.TI.Error (TIFailure)
import Miller.TI.Machine
import Miller.TI.Node

import Prelude hiding (lookup)

type TI sig m =
  ( Has (State TIMachine) sig m,
    Has (Reader Env) sig m,
    Has (Writer Stats) sig m,
    Has (Error Error.TIFailure) sig m,
    MonadFix m
  )

-- Keeps track of an immutable environment, a lazy machine state,
-- stats, and failure (this last is hinky due to laziness).
type TIMonad = ReaderC Env (ErrorC TIFailure (WriterC Stats (StateC TIMachine Identity)))

-- Run a template instantiation invocation with an empty starting state.
runTI :: TIMonad a -> (Either TIFailure a, TIMachine, Stats)
runTI = runTI' mempty

-- Run a template instantiation invocation with a specified starting state.
runTI' :: TIMachine -> TIMonad a -> (Either TIFailure a, TIMachine, Stats)
runTI' start go =
  let flatten (a, (b, c)) = (c, a, b)
   in flatten
      . run
      . runState @TIMachine start
      . runWriter
      . runError
      . runReader @Env mempty
      $ go

-- Register a node on the heap, returning its new address.
store :: TI sig m => Node -> m Addr
store n = withinState heap (Heap.alloc n) <* Stats.allocation

-- Look up the address bound to a given name, or throw 'UnboundName'.
lookup :: TI sig m => Name -> m Addr
lookup n = asks (Env.lookup n) >>= maybeM (Error.unboundName n)

-- Find a node in the heap by address, or throw 'DeadPointer'.
find :: TI sig m => Addr -> m Node
find a = uses heap (Heap.lookup a) >>= maybeM (Error.deadPointer a)

-- Register a supercombinator in the heap.
allocateSC :: TI sig m => Expr.CoreDefn -> m (Name, Addr)
allocateSC (Expr.Defn name args body) = do
  addr <- store (NSupercomb name args body)
  pure (name, addr)

-- Get the node on the top of the stack, or throw 'EmptyStack'/'DeadPointer'.
stackHead :: TI sig m => m Node
stackHead = uses stack Stack.first >>= maybeM Error.emptyStack >>= find

-- Register a primitive operator in the heap and store.
allocatePrim :: TI sig m => Name -> Either UnOp BinOp -> m (Name, Addr)
allocatePrim name op = do
  addr <- store (NPrim op)
  pure (name, addr)

-- All operators supported in the VM.
operators :: [(Name, Either Expr.UnOp Expr.BinOp)]
operators = [("*", Right Expr.Mul),
             ("+", Right Expr.Add),
             ("-", Right Expr.Sub),
             ("~", Left Expr.Neg)
             ]

-- Evaluate the loaded program, returning the set of all seen states.
eval :: TI sig m => m (Seq TIMachine)
eval = execWriter @(Seq TIMachine) go
  where
    go = do
      curr <- get
      tell (pure @Seq curr)
      case machineStatus curr of
        Crashed -> Error.emptyStack
        Stopped -> pure ()
        Active -> step *> go

step :: TI sig m => m ()
step =
  stackHead >>= \case
    NNum n -> numStep n
    NAp f x -> apStep f x
    NSupercomb name args body -> scStep name args body
    NInd addr -> indStep addr
    NPrim op -> primStep op

indStep :: TI sig m => Addr -> m ()
indStep a = modifying stack (Stack.replace a)

numStep :: TI sig m => Int -> m ()
numStep _ = Error.numberAppliedAsFunction

-- As per the unwind rule, we look leftwards and downwards
-- to get the current thing to apply, so we only look in 'f'.
apStep :: TI sig m => Addr -> Addr -> m ()
apStep f _x = modifying stack (Stack.push f)

primStep :: TI sig m => Either UnOp BinOp -> m ()
primStep (Left x) = Error.unimplemented x
primStep (Right op) = Error.unimplemented op
  -- entries <- uses stack (take 3 . Stack.contents)
  -- given <- traverse find entries
  -- let recur item = find item >>= \case
  --       NNum x' -> pure x'
  --       NInd x' -> recur x'
  --       NAp left right -> do
  --         currStack <- uses stack
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

-- UPDATES WILL BE PERFORMED HERE
scStep :: TI sig m => Name -> [Name] -> CoreExpr -> m ()
scStep _name args body = do
  given <- uses stack (pred . Stack.length)
  let needed = length args
  when (given < needed) (Error.tooFewArguments given needed)
  Stats.depth given

  root <- uses stack (Stack.nth (length args))
  -- Bind argument names to addresses
  argBindings <- Env.fromBindings args <$> argumentAddresses
  newEnviron <- mappend argBindings <$> ask

  result <- instantiateWithUpdate body root
  -- Discard arguments from stack (including root)
  Stack.Stack st <- use stack
  let (currArgs, rest) = splitAt (length args + 1) st
  assign stack (Stack.Stack (result : rest))
  Stats.reduction


argumentAddresses :: TI sig m => m [Addr]
argumentAddresses = do
  Stack.Stack s <- use stack
  case s of
    [] -> Error.emptyStack
    (_sc : items) -> traverse go items
      where
        go addr =
          find addr >>= \case
            NAp _fun arg -> pure arg
            NInd arg -> pure arg
            n -> Error.badArgument n

compile :: TI sig m => CoreProgram -> m (Seq TIMachine)
compile ps = do
  toplevels <- Env.fromList <$> traverse allocateSC (unProgram (preludeDefs <> ps))
  builtins <- Env.fromList <$> traverse (uncurry allocatePrim) operators
  local (mappend toplevels . mappend builtins) $ do
    main <- lookup "main"
    modifying stack (Stack.push main)
    eval



execute :: TI sig m => CoreProgram -> m Node
execute p = compile p *> stackHead

-- | This function diverges if it is called inside a strict monad,
-- so we can't use it with Writer.
instantiate ::
  TI sig m =>
  CoreExpr ->
  m Addr
instantiate e = case e of
  Expr.Num i -> withinState heap (Heap.alloc (NNum i))
  Expr.Var n -> do
    item <- Env.lookup n <$> ask
    pure (fromMaybe (error ("unbound name: " <> show n)) item)
  Expr.Ap f x -> do
    fore <- instantiate f
    aft <- instantiate x
    withinState heap (Heap.alloc (NAp fore aft))
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
  Binary op left right ->
    instantiate (Expr.Ap (Expr.Ap (Expr.Var (binOpToName op)) left) right)
  Unary op arg ->
    instantiate (Expr.Ap (Expr.Var (unOpToName op)) arg)
  other -> error ("unimplemented: " <> show other)

instantiateWithUpdate ::
  TI sig m =>
  CoreExpr ->
  Addr ->
  m Addr
instantiateWithUpdate e dest = do
  case e of
    Expr.Num i -> modifying heap (Heap.update dest (NNum i))
    Expr.Ap a b -> do
      fore <- instantiate a
      aft <- instantiate b
      modifying heap (Heap.update dest (NAp fore aft))
    Expr.Var n -> do
      item <- Env.lookup n <$> ask @Env
      case item of
        Nothing -> error ("unbound name: " <> show n)
        Just addr -> modifying heap (Heap.update dest (NInd addr))
    Expr.Let {} -> do
      addr <- instantiate e
      modifying heap (Heap.update dest (NInd addr))
    Binary op left right -> do
      addr <- instantiate (Expr.Ap (Expr.Ap (Expr.Var (binOpToName op)) left) right)
      modifying heap (Heap.update dest (NInd addr))
    Unary op arg -> do
      addr <- instantiate (Expr.Ap (Expr.Var (unOpToName op)) arg)
      modifying heap (Heap.update dest (NInd addr))
    other -> error ("unimplemented: " <> show other)

  pure dest
