{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List (nub)
import Doors
import GHC.Stack
import Hedgehog hiding (Var (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Gen qualified as Gen (ensure)
import Hedgehog.Internal.Property (failWith)
import Hedgehog.Range qualified as Range
import Optics.Getter
import Miller.Expr as Expr
import Miller.Parser as Parser hiding (parse)
import Miller.Pretty as Pretty
import Miller.Stats qualified as Stats
import Miller.TI qualified as TI
import Miller.TI.Env qualified as Env
import Miller.TI.Heap qualified as Heap
import Text.Trifecta as Trifecta
import Text.Trifecta.Result qualified as Result

xPlusY :: CoreExpr
xPlusY = Ap (Ap (Var "+") (Var "x")) (Var "y")

double :: CoreProgram
double =
  Program
    [ Defn "main" [] (Ap (Var "double") (Num 21)),
      Defn "double" ["x"] (Ap (Ap (Var "add") (Var "x")) (Var "x"))
    ]

parse :: Parser a -> String -> Either String a
parse p = Trifecta.foldResult (Left . show) Right . parseString (p <* eof) mempty

parseFile :: Parser a -> FilePath -> IO (Either String a)
parseFile p f = Trifecta.foldResult (Left . show) Right <$> Trifecta.parseFromFileEx p f

name :: Gen Name
name = Name <$> Gen.ensure (`notElem` Parser.keywords) go
  where
    go = Gen.prune (Gen.text (Range.linear 3 6) Gen.alpha)

applying :: Gen CoreExpr
applying = Gen.recursive Gen.choice recurs nonrecurs
  where
    recurs = [Var <$> name, Num <$> Gen.integral (Range.linear 1 10)]
    nonrecurs = [Gen.subterm2 (Var <$> name) applying Ap]

testCase :: HasCallStack => PropertyT IO () -> Property
testCase = withTests 1 . property

prop_parens_in_nested_app :: Property
prop_parens_in_nested_app = testCase $ do
  let ex1 = Ap (Ap (Var "f") (Ap (Var "g") (Var "x"))) (Ap (Var "h") (Var "y"))
  showExpr ex1 === "f (g x) (h y)"
  tripping ex1 Pretty.showExpr (parse parseExpr)

assertParses :: (HasCallStack, MonadTest m) => Parser a -> String -> m a
assertParses p s = foldResult describe pure (parseString (p <* eof) mempty s)
  where
    describe info = withFrozenCallStack $ failWith Nothing $ renderDoc (() <$ _errDoc info)

prop_associativity_works :: HasCallStack => Property
prop_associativity_works = testCase $ do
  res <- assertParses parseExpr "f g h i"
  res === ((Var "f" $$ Var "g") $$ Var "h") $$ Var "i"
  res === Var "f" $$ Var "g" $$ Var "h" $$ Var "i"

prop_int_exprs_parse :: Property
prop_int_exprs_parse = testCase $ do
  add <- assertParses parseExpr "1 * 2 * 3"
  add === Num 1 $* Num 2 $* Num 3
  mul <- assertParses parseExpr "1 + 2 + 3"
  mul === Num 1 $+ Num 2 $+ Num 3
  mix <- assertParses parseExpr "1 - 2 + 4"
  mix === Num 1 $- Num 2 $+ Num 4
  par <- assertParses parseExpr "1 - (2 + 4)"
  par === Num 1 $- (Num 2 $+ Num 4)

prop_negate_works :: Property
prop_negate_works = testCase $ do
  neg <- assertParses parseExpr "-1"
  neg === Unary Neg (Num 1)

prop_operator_precedence_works :: Property
prop_operator_precedence_works = testCase $ do
  mixed <- assertParses parseExpr "5 * 2 + 1"
  mixed === (Num 5 $* Num 2) $+ Num 1

prop_expressions_roundtrip :: Property
prop_expressions_roundtrip = property $ do
  expr <- forAll applying
  tripping expr Pretty.showExpr (parse parseExpr)

prop_fixtures_roundtrip :: Property
prop_fixtures_roundtrip = testCase $ do
  let go f = do
        item <- liftIO (readFile f) >>= assertParses parseProgram
        tripping item Pretty.showProgram (parse (parseProgram <* eof))

  go "examples/double.mac"
  go "examples/pair.mac"

----- TI tests

prop_machine_stopped :: Property
prop_machine_stopped = testCase (TI.machineStatus TI.stoppedMachine === TI.Stopped)

prop_ti_allocateSC_registers_heap_entry :: Property
prop_ti_allocateSC_registers_heap_entry = property $ do
  names <- nub <$> forAll (Gen.list (Range.constant 5 25) name)
  let count = length names
  let (eRes, mach, _stats) = TI.runTI $
        for names $ \name -> TI.allocateSC (Defn name [] (Num 1))

  res <- evalEither eRes
  Heap.count (view TI.heap mach) === count

  for_ res $ \(name, addr) -> do
    Heap.lookup addr (view TI.heap mach) === Just (TI.NSupercomb name [] (Num 1))

prop_eval_finalized_machine_is_noop :: Property
prop_eval_finalized_machine_is_noop = testCase $ do
  let (res, mach, stats) = TI.runTI' TI.stoppedMachine TI.eval
  footnote (Pretty.renderShow mach)
  res === Right [TI.stoppedMachine]
  Stats.steps stats === 0

prop_instantiate_fails_when_applying_two_naturals :: Property
prop_instantiate_fails_when_applying_two_naturals = testCase $ do
  let (eRes, _mach, _stats) =
        TI.runTI $
          TI.compile [Defn "main" [] (Expr.Ap (Expr.Num 1) (Expr.Num 2))]

  eRes === Left TI.NumberAppliedAsFunction

assertExecutesAs :: MonadTest m => TI.Node -> Expr.CoreProgram -> m ()
assertExecutesAs n p = do
  let (eRes, mach, stats) = TI.runTI (TI.execute p)
  annotate (Pretty.renderShow mach)
  footnote (Pretty.renderShow stats)
  res <- evalEither eRes
  res === n

assertFailsWith :: MonadTest m => TI.TIFailure -> Expr.CoreProgram -> m ()
assertFailsWith e p = do
  let (eRes, mach, stats) = TI.runTI (TI.execute p)
  footnote (show mach)
  footnote (show stats)
  eRes === Left e

prog :: MonadTest m => String -> m CoreProgram
prog = assertParses parseProgram

prop_handles_too_few_args :: Property
prop_handles_too_few_args = testCase $ do
  prog "main = S 1" >>= assertFailsWith (TI.TooFewArguments 1 3)
  prog "main = S 1 2" >>= assertFailsWith (TI.TooFewArguments 2 3)

prop_works_with_examples :: Property
prop_works_with_examples = testCase $ do
  prog "main = S K K 3" >>= assertExecutesAs (TI.NNum 3)
  prog "main = let three = 3 in S K K 3" >>= assertExecutesAs (TI.NNum 3)
  prog "main = letrec three = 3; tres = three in S K K tres" >>= assertExecutesAs (TI.NNum 3)

  pair <- liftIO (parseFile parseProgram "examples/pair.mac") >>= evalEither
  assertExecutesAs (TI.NNum 4) pair

prop_adds_updating :: Property
prop_adds_updating = testCase $ do
  prog "id x = x\nmain = twice twice id 3" >>= assertExecutesAs (TI.NNum 3)

main :: IO ()
main = void (checkParallel $$(discover))
