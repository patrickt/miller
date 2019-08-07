{-# LANGUAGE BlockArguments, ConstraintKinds, FlexibleContexts, FlexibleInstances, LambdaCase, OverloadedLists,
             OverloadedStrings, RecordWildCards, TupleSections, TypeApplications #-}

module Miller.TI where

import Doors

-- import Control.Effect
-- import Control.Effect.Error
-- import Control.Effect.Reader
-- import Control.Effect.State
-- import Control.Effect.Writer
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

-- data Node
--   = Ply Addr Addr
--   | Super Name [Name] CoreExpr
--   | Leaf Int
--   | Indirect Addr
--     deriving (Eq, Show)

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


-- preludeDefs :: CoreProgram
-- preludeDefs = [ Defn "I" ["x"] (Var "x")
--               , Defn "K" ["y", "z"] (Var "y")
--               , Defn "K1" ["y", "z"] (Var "z")
--               , Defn "S" ["a", "b", "c"] ((Var "a" <> Var "c") <> (Var "b" <> Var "c"))
--               , Defn "D" ["f", "g", "h"] (Var "f" <> (Var "g" <> Var "h"))]

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
