{-# LANGUAGE BlockArguments, ConstraintKinds, FlexibleContexts, FlexibleInstances, LambdaCase, OverloadedLists,
             OverloadedStrings, RecordWildCards, TupleSections, TypeApplications, NamedFieldPuns #-}

module Miller.TI where

import Doors hiding (find)
import Prelude hiding (lookup)

import Control.Effect
import Control.Effect.Error
import Control.Effect.Reader
import Control.Effect.State
import Control.Effect.Writer


import           Miller.Expr as Expr
import           Miller.Stats (Stats)
import qualified Miller.Stats as Stats
import qualified Miller.TI.Env as Env
import           Miller.TI.Heap (Heap, Addr)
import qualified Miller.TI.Heap as Heap

-- The dump records the state of the spine prior to the evaluation of
-- an argument of a strict primitive. Currently unused.
data Dump = Dump deriving (Eq, Show)

type Env = Env.Env Addr

data TIMachine = TIMachine
  { stack   :: [Addr]
  , dump    :: Dump
  , heap    :: Heap Node -- maps Addr to Node
  } deriving (Eq, Show)

instance Lower TIMachine where
  lowerBound = TIMachine [] Dump lowerBound

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
    deriving (Eq, Show)

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

data TIFailure
  = EmptyStack
  | NumberAppliedAsFunction
  | UnboundName Name
  | DeadPointer Addr
  | BadArgument Node
  | Unimplemented
    deriving (Eq, Show)

type TI sig m = ( Member (State TIMachine) sig
                , Member (Reader Env) sig
                , Member (Writer Stats) sig
                , Member (Error TIFailure) sig
                , Effect sig
                , Carrier sig m
                )

runTI :: ReaderC Env (ErrorC TIFailure (WriterC Stats (StateC TIMachine PureC))) a -> (Either TIFailure a, TIMachine, Stats)
runTI = runTI' lowerBound

runTI' :: TIMachine -> ReaderC Env (ErrorC TIFailure (WriterC Stats (StateC TIMachine PureC))) a -> (Either TIFailure a, TIMachine, Stats)
runTI' start go =
  let (mach, (stats, res)) = run . runState start . runWriter . runError . runReader @Env mempty $ go
  in (res, mach, stats)

-- Register an item in the heap
store :: TI sig m => Node -> m Addr
store n = do
  (new, item) <- gets (Heap.alloc n . heap)
  modify (\m -> m { heap = new})
  pure item

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
        Active  -> step *> Stats.step *> go

stackHead :: TI sig m => m Addr
stackHead = gets (listToMaybe . stack) >>= maybeM (throwError EmptyStack)

step :: TI sig m => m ()
step = stackHead >>= find >>= \case
  NNum n -> numStep n
  NAp f x -> apStep f x
  NSupercomb name args body -> scStep name args body
    -- NSupercomb sc args body -> scStep sc args body

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

getargs :: TI sig m => [Name] -> m [Addr]
getargs args = gets stack >>= \case
  []         -> throwError EmptyStack
  (sc:items) -> traverse go items
    where go addr = find addr >>= \case
            NAp _fun arg -> pure arg
            n            -> throwError (BadArgument n)



-- UPDATES WILL BE PERFORMED HERE
scStep :: TI sig m => Name -> [Name] -> CoreExpr -> m ()
scStep name args body = do
  -- Bind argument names to addresses
  argBindings <- Env.fromBindings args <$> getargs args
  result <- local (mappend argBindings) (instantiate body)

  -- Discard arguments from stack (including root)
  newStack <- gets (drop (length args + 1) . stack)
  -- Push result onto stack
  modify (\m -> m { stack = result : newStack })

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

instantiate :: TI sig m => CoreExpr -> m Addr
instantiate e = case e of
  Expr.Num i  -> store (NNum i)
  Expr.Var n  -> lookup n
  Expr.Ap f x -> do
    fore <- instantiate f
    aft  <- instantiate x
    store (NAp fore aft)
  Let Non binds bod -> do
    let newBindings = Env.fromList binds
    newEnv <- traverse instantiate newBindings
    local (mappend newEnv) (instantiate bod)

-- getArgs :: TI sig m => m [Addr]
-- getArgs = do
--   stk <- gets stack
--   case stk of
--     [] -> pure []
--     (sc:args) -> for args $ \addr -> do
--       NAp fun arg <- lookup addr
--       pure arg




--   dropped <- gets @Stack (drop (length args + 1))
--   let
--   modify (\m -> m { stack = _newStack
--                   , heap  = _newHeap
--                   })

-- compile :: CoreProgram -> TIMachine
-- compile (Program ps) = run . runState (lowerBound @TIMachine) $ do
--   ()
--   start <- lookupGlobal "main"









-- import Debug.Trace
-- import Data.Semigroup (Max (..))

-- import           Data.Text.Prettyprint.Doc as Pretty
-- import           Miller.Expr
-- import           Miller.Parser (parseProgram)
-- import           Miller.Stats (Stats)
-- import qualified Miller.Stats as Stats
-- import qualified Miller.TI.Env as Env
-- import           Miller.TI.Heap (Addr)
-- import qualified Miller.TI.Heap as Heap
-- import qualified Text.Trifecta as Parser



-- instance Pretty Node where
--   pretty = \case
--     Ply f x      -> pretty f <> "." <> pretty x
--     Super n xs _ -> pretty n <> Pretty.tupled (pretty <$> xs)
--     Leaf n       -> pretty n
--     Indirect a   -> "?" <> pretty a

-- isDataNode :: Node -> Bool
-- isDataNode (Leaf _) = True
-- isDataNode _        = False

-- type Env  = Env.Env Addr
-- type Heap = Heap.Heap Node

-- type Stack = [Addr]

-- data Dump = Dump

-- data Fatal
--   = NotDefined Name
--   | EmptyStack
--   | AddrNotFound Addr
--   | NumAppliedAsFunction Int
--   | TooFewArguments Name
--   | InfiniteLoop CoreExpr
--   | ExpectedSCPly Node
--     deriving (Eq, Show)

-- instance Pretty Fatal where pretty = viaShow

-- instance Pretty a => Pretty (Either Fatal a) where
--   pretty (Left f)  = "ERROR:" <+> pretty f
--   pretty (Right v) = "SUCCESS:" <+> pretty v

-- notDefined :: (Member (Error Fatal) sig, Carrier sig m) => Name -> m a
-- notDefined = throwError . NotDefined

-- type Machine sig m
--   = ( Member (State Stack)  sig
--     , Member (State Heap) sig
--     , Member (State Dump)   sig
--     , Member (Reader Env) sig
--     , Member (Writer Stats) sig
--     , Member (Error Fatal)  sig
--     , Effect sig
--     , Carrier sig m
--     , Monad m
--     )

-- data Result a = Result
--   { _stack  :: Stack
--   , _heap   :: Heap
--   , _stats  :: Stats
--   , _result :: Maybe a
--   , _error  :: Maybe Fatal
--   } deriving (Show)

-- instance Pretty a => Pretty (Result a) where
--   pretty Result {..} = vcat [ "result:  " <> pretty _result
--                             , "stack:   " <> pretty _stack
--                             , "heap:    " <> pretty _heap
--                             , maybe "" pretty _error
--                             , "***"
--                             , pretty _stats
--                             ]

-- assemble :: Monoid a => (Stack, (Heap, Either Fatal (Stats, a))) -> Result a
-- assemble (s, (h, Right (t, e))) = Result s h t (Just e) Nothing
-- assemble (s, (h, Left f)) = Result s h mempty Nothing (Just f)


-- runTemplate :: CoreProgram -> Doc ()
-- runTemplate
--   = pretty @(Result _)
--   . assemble
--   . run
--   . runReader @Env lowerBound
--   . runState @Stack lowerBound
--   . runState @Heap lowerBound
--   . evalState Dump
--   . runError @Fatal
--   . runWriter @Stats
--   . compile

-- testMiller :: String -> Doc ()
-- testMiller s = case Parser.parseString parseProgram mempty s of
--   Parser.Success a -> runTemplate a
--   Parser.Failure f -> "result: " <> viaShow (Parser._errDoc f)

-- alloc :: Machine sig m => Node -> m Addr
-- alloc n = do
--   (newHeap, addr) <- gets (Heap.alloc n)
--   put @Heap newHeap

--   Stats.allocation
--   pure addr

-- buildInitialHeap :: Machine sig m => CoreProgram -> m Env
-- buildInitialHeap defns = execState @Env mempty $ do
--   for_ (unProgram defns) $ \(Defn name args body) -> do
--     addr <- alloc (Super name args body)
--     modify (Env.insert name addr)

-- compile :: Machine sig m => CoreProgram -> m Env
-- compile program = do
--   globs <- buildInitialHeap (program <> preludeDefs)
--   local (const globs) $ do
--     asks (Env.lookup "main") >>= maybe (throwError (NotDefined "main")) (put @Stack . pure)
--     eval *> ask

-- eval :: Machine sig m => m ()
-- eval = do
--   final <- isFinal
--   unless final (step *> eval)

-- step :: Machine sig m => m ()
-- step = do
--   Stats.step
--   stack <- get @Stack
--   when (null stack) (throwError EmptyStack)
--   lookupHeap (head stack) >>= \case
--     Leaf n                -> throwError (NumAppliedAsFunction n)
--     Ply a _b              -> modify @Stack (a:)
--     Indirect addr         -> modify @Stack tail *> modify @Stack (addr:)
--     Super name args body -> do
--       when (length args >= length stack) $
--         throwError (TooFewArguments name)

--       newBindings <- Env.fromBindings args <$> pendingArguments

--       result <- local (Env.union newBindings) (instantiate body)

--       let newStack = result : drop (succ (length args)) stack
--       put @Stack newStack

-- pendingArguments :: Machine sig m => m [Addr]
-- pendingArguments = do
--   pending <- tail <$> get @Stack
--   forM pending $ \a -> do
--     res <- lookupHeap a
--     case res of
--       Ply _ arg -> pure arg
--       _         -> throwError (ExpectedSCPly res)

-- instantiate :: Machine sig m => CoreExpr -> m Addr
-- instantiate ex =
--   let go it = local @Word succ $ case it of
--         Num i  -> alloc (Leaf i)
--         Var n  -> asks (Env.lookup n) >>= maybeM (notDefined n)
--         Let Non bindings bod -> introduce bindings bod
--         Let Rec bindings bod -> do
--           oldenv <- ask
--           let prealloc n = Env.insert (fst n) Heap.unallocated
--           let augenv = foldr prealloc oldenv bindings
--           local (Env.union augenv) $ introduce bindings (Let Non bindings bod)
--         Ap f x -> do
--           Stats.reduction
--           asks @Word Max >>= tell
--           alloc =<< Ply <$> go f <*> go x

--       introduce bindings bod = do
--         newenv <- Env.fromList <$> traverse (traverse go) bindings
--         local (Env.union newenv) (go bod)
--   in do
--     (water, res) <- runReader @Word 0 . runWriter @(Max Word) $ go ex
--     Stats.depth water
--     pure res



-- lookupHeap :: Machine sig m => Addr -> m Node
-- lookupHeap a = gets (Heap.lookup a) >>= maybe (throwError (AddrNotFound a)) pure

-- isFinal :: Machine sig m => m Bool
-- isFinal = get @Stack >>= \case
--   []  -> throwError EmptyStack
--   [a] -> isDataNode <$> lookupHeap a
--   _   -> pure False
