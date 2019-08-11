{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, LambdaCase, OverloadedLists,
             OverloadedStrings, RecordWildCards, TupleSections, RecursiveDo, TypeApplications, NamedFieldPuns, ScopedTypeVariables #-}

module Miller.TI where

import Doors hiding (find)
import Prelude hiding (lookup)

import Control.Effect hiding (StateC)
import Control.Effect.Error
import Control.Effect.Reader
import Control.Effect.State
import Control.Effect.Writer
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc ((<+>))
import Control.Monad.Fix
import qualified Data.List.NonEmpty as NonEmpty

import           Miller.Expr as Expr
import           Miller.Stats (Stats)
import qualified Miller.Stats as Stats
import qualified Miller.TI.Env as Env
import           Miller.TI.Heap (Heap, Addr)
import qualified Miller.TI.Heap as Heap

-- The dump records the state of the spine prior to the evaluation of
-- an argument of a strict primitive. Currently unused.
data Dump = Dump deriving (Eq, Show)

type Stack = [Addr]

prettyStack :: [Addr] -> Pretty.Doc a
prettyStack st = Pretty.align (Pretty.list (fmap pretty st))

type Env = Env.Env Addr

data TIMachine = TIMachine
  { stack   :: Stack
  , heap    :: Heap Node -- maps Addr to Node
  } deriving (Eq, Show)

instance Pretty.Pretty TIMachine where
  pretty TIMachine{stack, heap} =
    Pretty.vcat [ "stack" <+> "=" <+> prettyStack stack
                , "heap " <+> "=" <+> pretty heap
                ]

instance Lower TIMachine where
  lowerBound = TIMachine [] lowerBound

data Status
  = Crashed
  | Stopped
  | Active
    deriving (Eq, Show)

stoppedMachine :: TIMachine
stoppedMachine = lowerBound
  { stack = [lowerBound]
  , heap = Heap.update lowerBound (NNum 1) Heap.initial
  }

isFinal :: TIMachine -> Bool
isFinal m = machineStatus m /= Active

machineStatus :: TIMachine -> Status
machineStatus TIMachine{ stack, heap } =
  let decide x = if isDataNode x then Stopped else Active
  in case stack of
    []     -> Crashed
    [sole] -> maybe Crashed decide (Heap.lookup sole heap)
    _      -> Active

data Node
  = NAp Addr Addr
  | NSupercomb Name [Name] CoreExpr
  | NNum Int
  | NInd Addr
  | NPrim Binary
    deriving (Eq, Show)

instance Pretty.Pretty Node where pretty = Pretty.viaShow

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

data TIFailure
  = EmptyStack
  | NumberAppliedAsFunction
  | UnboundName Name
  | DeadPointer Addr
  | TooFewArguments Int Int
  | BadArgument Node
  | Unimplemented CoreExpr
    deriving (Eq, Show)

type TI sig m = ( Member (State TIMachine) sig
                , Member (Reader Env) sig
                , Member (Writer Stats) sig
                , Member (Error TIFailure) sig
                , Effect sig
                , Carrier sig m
                , MonadFix m
                )

runTI :: ReaderC Env (ErrorC TIFailure (WriterC Stats (StateC TIMachine PureC))) a -> (Either TIFailure a, TIMachine, Stats)
runTI = runTI' lowerBound

runTI' :: TIMachine -> ReaderC Env (ErrorC TIFailure (WriterC Stats (StateC TIMachine PureC))) a -> (Either TIFailure a, TIMachine, Stats)
runTI' start go =
  let flatten (a, (b, c)) = (c, a, b)
  in flatten . run . runState start . runWriter . runError . runReader @Env mempty $ go

-- Register an item in the heap
store :: TI sig m => Node -> m Addr
store n = do
  (new, item) <- gets (Heap.alloc n . heap)
  modify (\m -> m { heap = new })
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
        Active  -> step *> go

stackHead :: TI sig m => m Addr
stackHead = gets (listToMaybe . stack) >>= maybeM (throwError EmptyStack)

step :: TI sig m => m ()
step = stackHead >>= find >>= \case
  NNum n -> numStep n
  NAp f x -> apStep f x
  NSupercomb name args body -> scStep name args body
  NInd addr -> indStep addr

indStep :: TI sig m => Addr -> m ()
indStep a = modify (\m -> m { stack = a : stack m })

numStep :: TI sig m => Int -> m ()
numStep _ = throwError NumberAppliedAsFunction

-- As per the unwind rule, we look leftwards and downwards
-- to get the current thing to apply, so we only look in 'f'.
apStep :: TI sig m => Addr -> Addr -> m ()
apStep f _x = modify (\m -> m { stack = f : stack m })

lookup :: TI sig m => Name -> m Addr
lookup n = asks (Env.lookup n) >>= maybeM (throwError (UnboundName n))

find :: TI sig m => Addr -> m Node
find a = gets (Heap.lookup a . heap) >>= maybeM (throwError (DeadPointer a))

getargs :: TI sig m => m [Addr]
getargs = gets stack >>= \case
  []         -> throwError EmptyStack
  (_sc:items) -> traverse go items
    where go addr = find addr >>= \case
            NAp _fun arg -> pure arg
            NInd arg     -> pure arg
            n            -> throwError (BadArgument n)



-- UPDATES WILL BE PERFORMED HERE
scStep :: TI sig m => Name -> [Name] -> CoreExpr -> m ()
scStep _name args body = do
  given <- gets (pred . length . stack)
  let needed = length args
  when (given < needed) (throwError (TooFewArguments given needed))
  Stats.depth given

  -- Bind argument names to addresses
  argBindings <- Env.fromBindings args <$> getargs
  newEnviron  <- mappend argBindings <$> ask
  h           <- gets heap
  let (newHeap, result) = run $ runReader newEnviron $ runState h $ instantiate body
  modify (\m -> m { heap = newHeap })


  -- Discard arguments from stack (including root)
  st <- gets stack
  let (currArgs, rest) = splitAt (length args + 1) st

  -- Push result onto stack
  modify (\m -> m { stack = result : rest
                  , heap  = Heap.update (last currArgs) (NInd result) (heap m)
                  })

  Stats.reduction

compile :: TI sig m => CoreProgram -> m [TIMachine]
compile ps = do
  toplevels <- Env.fromList <$> traverse allocateSC (unProgram (preludeDefs <> ps))
  local (mappend toplevels) $ do
    main <- lookup "main"
    modify (\m -> m { stack = [main] })
    eval

execute :: TI sig m => CoreProgram -> m Node
execute p = do
  void $ compile p
  stackHead >>= find

pushEnv :: TI sig m => Env -> m a -> m a
pushEnv e = local (mappend e)

state :: (Member (State s) sig, Carrier sig m) => (s -> (s, a)) -> m a
state go = do
  (st, res) <- go <$> get
  res <$ put st

instantiate :: CoreExpr -> StateC (Heap Node) (ReaderC Env PureC) Addr
instantiate e = case e of
  Expr.Num i  -> state (Heap.alloc (NNum i))
  Expr.Var n  -> do
    item <- Env.lookup n <$> ask
    pure (fromMaybe (error (show n)) item)
  Expr.Ap f x -> do
    fore <- instantiate f
    aft  <- instantiate x
    state (Heap.alloc (NAp fore aft))

  Let Non binds bod -> do
    -- Straightforward, nonrecursive lets
    newVals <- traverse (instantiate . snd) binds
    let newBindings = Env.fromList (NonEmpty.zip (fmap fst binds) newVals)
    local (newBindings <>) (instantiate bod)

  Let Rec binds bod -> mdo
    -- newVals is defined in terms of newBindings, and vice versa
    -- laziness is absolute witchcraft and I hate/love it
    newVals     <- traverse (local (newBindings <>) . instantiate . snd) binds
    newBindings <- pure (Env.fromList (NonEmpty.zip (fmap fst binds) newVals))
    local (newBindings <>) (instantiate bod)


  other -> error ("unimplemented: " <> show other)
