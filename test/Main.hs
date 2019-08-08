{-# LANGUAGE OverloadedLists, OverloadedStrings, TemplateHaskell #-}

module Main where

import Doors

import Hedgehog hiding (Var (..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen (ensure)
import qualified Hedgehog.Range as Range
import Text.Trifecta as Trifecta

import Miller.Expr
import Miller.Pretty as Pretty
import Miller.Parser as Parser

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

prop_parens_in_nested_app :: Property
prop_parens_in_nested_app = withTests 1 . property $ do
  let ex1 = Ap (Ap (Var "f") (Ap (Var "g") (Var "x"))) (Ap (Var "h") (Var "y"))
  showExpr ex1 === "f (g x) (h y)"
  tripping ex1 Pretty.showExpr (parse parseExpr)

prop_expressions_roundtrip :: Property
prop_expressions_roundtrip = property $ do
  expr <- forAll additive
  tripping expr Pretty.showExpr (parse parseExpr)

prop_fixtures_roundtrip :: Property
prop_fixtures_roundtrip = withTests 1 . property $ do
  let go f = do
        item <- liftIO (parseFile parseProgram f) >>= Hedgehog.evalEither
        tripping item Pretty.showProgram (parse (parseProgram <* eof))

  go "examples/double.mac"
  go "examples/pair.mac"

main :: IO ()
main = void (checkParallel $$(discover))
