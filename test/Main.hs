{-# LANGUAGE OverloadedLists, OverloadedStrings, TemplateHaskell #-}

module Main where

import Doors

import Hedgehog hiding (Var (..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen (ensure)
import qualified Hedgehog.Range as Range
import Text.Trifecta as Trifecta
import Data.List (nub)

import Miller.Expr as Expr
import Miller.Pretty as Pretty
import Miller.Parser as Parser
import qualified Miller.TI as TI
import qualified Miller.TI.Heap as Heap
import qualified Miller.Stats as Stats
import qualified Miller.TI.Env as Env


xPlusY :: CoreExpr
xPlusY = Ap (Ap (Var "+") (Var "x")) (Var "y")

double :: CoreProgram
double = Program [ Defn "main" [] (Ap (Var "double") (Num 21))
               , Defn "double" ["x"] (Ap (Ap (Var "add") (Var "x")) (Var "x"))
               ]


parse :: Parser a -> String -> Either String a
parse p = Trifecta.foldResult (Left . show) Right . parseString (p <* eof) mempty

parseFile :: Parser a -> FilePath -> IO (Either String a)
parseFile p f = Trifecta.foldResult (Left . show) Right <$> Trifecta.parseFromFileEx p f

name :: Gen Name
name = Name <$> Gen.ensure (`notElem` Parser.keywords) go
  where go = Gen.prune (Gen.text (Range.linear 3 6) Gen.alpha)

additive :: Gen CoreExpr
additive = Gen.recursive Gen.choice recurs nonrecurs
  where
    recurs    = [Var <$> name, Num <$> Gen.integral (Range.linear 1 10)]
    nonrecurs = [Gen.subterm2 (Var <$> name) additive Ap]

testCase = withTests 1 . property

prop_parens_in_nested_app :: Property
prop_parens_in_nested_app = testCase $ do
  let ex1 = Ap (Ap (Var "f") (Ap (Var "g") (Var "x"))) (Ap (Var "h") (Var "y"))
  showExpr ex1 === "f (g x) (h y)"
  tripping ex1 Pretty.showExpr (parse parseExpr)

prop_expressions_roundtrip :: Property
prop_expressions_roundtrip = property $ do
  expr <- forAll additive
  tripping expr Pretty.showExpr (parse parseExpr)

prop_fixtures_roundtrip :: Property
prop_fixtures_roundtrip = testCase $ do
  let go f = do
        item <- liftIO (parseFile parseProgram f) >>= Hedgehog.evalEither
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
        for names $ \name -> do
          addr <- TI.allocateSC (Defn name [] (Num 1))
          pure (name, addr)

  res <- evalEither eRes
  Heap.count (TI.heap mach) === count

  for_ res $ \(name, addr) -> do
    Env.lookup name (TI.globals mach) === Just addr
    Heap.lookup addr (TI.heap mach) === Just (TI.NSupercomb name [] (Num 1))

prop_eval_finalized_machine_is_noop :: Property
prop_eval_finalized_machine_is_noop = testCase $ do
  let (res, mach, stats) = TI.runTI' TI.stoppedMachine TI.eval
  res === Right [TI.stoppedMachine]
  Stats.steps stats === 0

prop_instantiate_fails_when_applying_two_naturals :: Property
prop_instantiate_fails_when_applying_two_naturals = testCase $ do
  let (eRes, _mach, _stats) = TI.runTI $
        TI.compile [Defn "main" [] (Expr.Ap (Expr.Num 1) (Expr.Num 2))]

  eRes === Left TI.NumberAppliedAsFunction

assertExecutesAs :: MonadTest m => TI.Node -> Expr.CoreProgram -> m ()
assertExecutesAs n p = do
  let (eRes, mach, stats) = TI.runTI (TI.execute p)
  footnote (show mach)
  footnote (show stats)
  res <- evalEither eRes
  res === n

prog :: MonadTest m => String -> m CoreProgram
prog p = evalEither (Trifecta.foldResult (Left . show) Right . parseString (parseProgram <* eof) mempty $ p)

prop_works_with_examples :: Property
prop_works_with_examples = testCase $
  prog "main = S K K 3" >>= assertExecutesAs (TI.NNum 3)

main :: IO ()
main = void (checkParallel $$(discover))
