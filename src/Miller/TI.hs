{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Miller.TI where

import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Lazy
import Control.Carrier.Writer.Strict
import Control.Effect.Optics
import Control.Monad.Fix
import Data.List.NonEmpty qualified as NonEmpty
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Set qualified as Set
import Doors hiding (find)
import Miller.Expr as Expr
import Miller.Stats (Stats)
import Miller.Stats qualified as Stats
import Miller.TI.Env (Env)
import Miller.TI.Env qualified as Env
import Miller.TI.Error (TIFailure)
import Miller.TI.Error qualified as Error
import Miller.TI.Heap (Addr)
import Miller.TI.Heap qualified as Heap
import Miller.TI.Machine (DebugMode (..), Machine, debugger)
import Miller.TI.Machine qualified as Machine
import Miller.TI.Node
import Miller.TI.Stack (Stack)
import Miller.TI.Stack qualified as Stack
import Prelude hiding (lookup)

type Bindings = Env ()

type TI sig m =
  ( Has (State Machine) sig m,
    Has (Reader (Env Addr)) sig m,
    Has (Reader Bindings) sig m,
    Has (Reader DebugMode) sig m,
    Has (Writer Stats) sig m,
    Has (Error Error.TIFailure) sig m,
    MonadFix m,
    MonadIO m
  )

-- Keeps track of an immutable environment, a lazy machine state,
-- stats, and failure (this last is hinky due to laziness).
type TIMonad =
  ReaderC
    (Env Addr)
    ( ReaderC
        DebugMode
        ( ReaderC
            Bindings
            ( ErrorC
                TIFailure
                ( WriterC
                    Stats
                    ( StateC
                        Machine
                        (LiftC IO)
                    )
                )
            )
        )
    )

-- Run a template instantiation invocation with an empty starting state.
runTI :: (MonadIO m) => TIMonad a -> m (Either TIFailure a, Machine, Stats)
runTI = runTI' mempty Run

-- Run a template instantiation invocation with an empty starting state.
debugTI :: (MonadIO m) => TIMonad a -> m (Either TIFailure a, Machine, Stats)
debugTI = runTI' mempty Debug

-- Run a template instantiation invocation with a specified starting state.
runTI' ::
  (MonadIO m) =>
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
        . runReader (mempty :: Bindings)
        . runReader dbg
        . runReader (mempty :: Env Addr)
        $ go

-- Register a node on the heap, returning its new address.
store :: (TI sig m) => Node -> m Addr
store n = withinState Machine.heap (Heap.alloc n) <* Stats.allocation

-- Look up the address bound to a given name, or throw 'UnboundName'.
lookup :: (TI sig m) => Name -> m Addr
lookup n = asks (Env.lookup n) >>= maybeM (Error.unboundName n)

-- Find a node in the heap by address, or throw 'DeadPointer'.
find :: (TI sig m) => Addr -> m Node
find a = uses Machine.heap (Heap.lookup a) >>= maybeM (Error.deadPointer a)

-- Introduce, but do not yet bind, names in a context. This is needed to
-- do scope checking (since we don't use de Brujin indices or something like that)
-- because the bindings in the environment may be defined circularly.
introducingNames :: (TI sig m, Foldable f) => f Name -> m a -> m a
introducingNames ns = local @Bindings (Env.introduce @() (toList ns))

-- Register a supercombinator in the heap.
allocateSC :: (TI sig m) => Expr.CoreDefn -> m (Name, Addr)
allocateSC (Expr.Defn name args body) = do
  addr <- store (NSupercomb name args body)
  pure (name, addr)

-- Get the node on the top of the stack, or throw 'EmptyStack'/'DeadPointer'.
stackHead :: (TI sig m) => m Node
stackHead = uses Machine.stack Stack.first >>= maybeM Error.emptyStack >>= find

-- Register a primitive operator in the heap and store.
allocatePrim :: (TI sig m) => Name -> Either UnOp BinOp -> m (Name, Addr)
allocatePrim name op = do
  addr <- store (NPrim op)
  pure (name, addr)

-- For each item on the N most recent stack entries, extract their argument
-- (if an Ap node) or their indirect target (if an Ind node) or throw 'BadArgument'.
stackContents :: (TI sig m) => Int -> m [Addr]
stackContents needed = do
  s <- uses Machine.stack (Stack.contents . Stack.take needed)
  when (null s) Error.emptyStack
  forM (drop 1 s) $
    find >=> \case
      NAp _fun arg -> pure arg
      NInd arg -> pure arg
      n -> Error.badArgument n

-- Run a machine until it crashes or stops, returning the set of all seen states.
eval :: (TI sig m) => m (Seq Machine)
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
step :: (TI sig m) => m ()
step = do
  Stats.step
  stackHead >>= \case
    NNum n -> numStep n
    NAp f x -> apStep f x
    NSupercomb name args body -> scStep name args body
    NInd addr -> indStep addr
    NPrim op -> primStep op

-- Indirect step: replace the top value with the value pointed to.
indStep :: (TI sig m) => Addr -> m ()
indStep a = modifying Machine.stack (Stack.replace a)

-- Num step: crash.
numStep :: (TI sig m) => Int -> m ()
numStep _ = Error.numberAppliedAsFunction

-- Ap step: push the function onto the stack.
-- As per the unwind rule, we look leftwards and downwards
-- to get the current thing to apply, so we only look in 'f'.
apStep :: (TI sig m) => Addr -> Addr -> m ()
apStep f _x = modifying Machine.stack (Stack.push f)

-- Primitive step (unimplemented)
primStep :: (TI sig m) => Either UnOp BinOp -> m ()
primStep (Left Neg) = do
  given <- uses Machine.stack (Stack.contents . Stack.take 2)
  debugger "unop primitive application"
  case given of
    [negAddr, apAddr] -> do
      arg <- find apAddr
      case arg of
        NNum val -> do
          modifying Machine.stack (Stack.drop 1)
          modifying Machine.heap (Heap.update apAddr (NNum (negate val)))
        NAp _func arg' -> do
          debugger ("arg is " <> show arg')
          currStack <- use Machine.stack
          modifying Machine.dump (Stack.push currStack)
          assign Machine.stack (pure arg')
          void eval
          result <- stackHead
          lastStack <- Stack.first <$> withinState Machine.dump (Stack.pop 1)
          debugger ("last stack" <> show lastStack)
          maybeM Error.emptyStack lastStack >>= assign Machine.stack
          case result of
            NNum a -> modifying Machine.heap (Heap.update apAddr (NNum (negate a)))
            other -> Error.badArgument other
          modifying Machine.stack (Stack.drop 1)
          debugger "done modifying"
        other -> Error.unimplemented other
    [] -> Error.emptyStack
    other : _rest -> find other >>= Error.badArgument
primStep (Right op) = do
  given <- uses Machine.stack (Stack.contents . Stack.take 3)
  debugger "bin primitive application"
  case given of
    [opAddr, leftAddr, rightAddr] -> do
      let resolve node = case node of
            NNum val -> pure val
            NAp _func arg' -> do
              currStack <- use Machine.stack
              modifying Machine.dump (Stack.push currStack)
              assign Machine.stack (pure arg')
              void eval
              result <- stackHead
              lastStack <- Stack.first <$> withinState Machine.dump (Stack.pop 1)
              maybeM Error.emptyStack lastStack >>= assign Machine.stack
              case result of
                NNum val -> pure val
                _ -> Error.badArgument result
            _ -> Error.unimplemented ("primStep: " <> show node)
      leftVal <- find leftAddr >>= resolve
      rightVal <- find rightAddr >>= resolve
      let new = NNum (binOpToFunc op leftVal rightVal)
      newNode <- store new
      modifying Machine.stack (Stack.push newNode . Stack.drop 3)
    [] -> Error.emptyStack
    other : _rest -> find other >>= Error.badArgument

-- Supercombinator step: apply a function, given args and body
scStep :: (TI sig m) => Name -> [Name] -> CoreExpr -> m ()
scStep _name args body = introducingNames args $ do
  -- record depth (number of dumps)
  Stats.depth =<< uses Machine.dump Stack.length
  -- Check arity and extract the root
  given <- uses Machine.stack (pred . Stack.length)
  let needed = length args
  root <-
    if (given < needed)
      then Error.tooFewArguments given needed
      else uses Machine.stack (Stack.nth needed)
  debugger "scStep: before instantiation"
  -- Bind argument names to addresses
  argBindings <- Env.fromBindings args <$> stackContents (succ needed)
  local (mappend argBindings) (instantiateWithUpdate body root)
  debugger "scStep: after instantiation"
  -- Discard arguments from stack (including root)
  modifying Machine.stack (Stack.drop needed)
  Stats.reduction

-- Given a redex, instantiate it and return its address on the stack.
instantiate ::
  (TI sig m) =>
  CoreExpr ->
  m Addr
instantiate e = case e of
  Expr.Num i -> store (NNum i)
  Expr.Var n -> do
    present <- asks (Env.isNameIntroduced n)
    unless present (Error.unboundName n)
    item <- Env.lookup n <$> ask
    pure (fromJust item)
  Expr.Ap f x -> do
    fun <- instantiate f
    arg <- instantiate x
    store (NAp fun arg)
  Let Non binds bod -> do
    -- Straightforward, nonrecursive lets
    introducingNames (fmap fst binds) $ do
      newVals <- traverse (instantiate . snd) binds
      let newBindings = Env.fromList (NonEmpty.zip (fmap fst binds) newVals)
      local (newBindings <>) (instantiate bod)
  Let Rec binds bod ->
    introducingNames (fmap fst binds) $ mdo
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
  (TI sig m) =>
  CoreExpr ->
  Addr ->
  m ()
instantiateWithUpdate e dest = do
  let update = modifying Machine.heap . Heap.update dest
  case e of
    Expr.Num i -> update (NNum i)
    Expr.Ap a b -> do
      fore <- instantiate a
      aft <- instantiate b
      update (NAp fore aft)
    Expr.Var _ ->
      instantiate e >>= update . NInd
    Expr.Let {} -> do
      addr <- instantiate e
      update (NInd addr)
    Binary {} -> do
      addr <- instantiate e
      update (NInd addr)
    Unary {} -> do
      addr <- instantiate e
      update (NInd addr)
    other -> Error.unimplemented other

-- Compile and run a program, returning the set of all seen states.
compile :: (TI sig m) => CoreProgram -> m (Seq Machine)
compile ps = do
  toplevels <- Env.fromList <$> traverse allocateSC (unProgram (preludeDefs <> ps))
  builtins <- Env.fromList <$> traverse (uncurry allocatePrim) Machine.operators
  local (mappend toplevels . mappend builtins) $ do
    names <- asks @(Env Addr) Env.boundNames
    introducingNames names $ do
      main <- lookup "main"
      modifying Machine.stack (Stack.push main)
      eval

-- Compile and run a program, returning the top of the stack.
execute :: (TI sig m) => CoreProgram -> m Node
execute p = compile p *> stackHead
