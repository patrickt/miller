{-# LANGUAGE OverloadedStrings #-}

module Miller.Parser where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import           GHC.Exts
import           Text.Parser.Char
import           Text.Parser.Token
import           Text.Parser.Token.Style (haskellIdents)
import           Text.Trifecta

import Miller.Expr

parseProgram :: Parser CoreProgram
parseProgram = Program <$> parseDefn `sepEndByNonEmpty` newline

parseDefn :: Parser CoreDefn
parseDefn = Defn <$> parseName
                 <*> many parseName
                 <*> (token (char '=') *> parseExpr)

parseAtomic :: Parser CoreExpr
parseAtomic = choice
  [ Num . fromIntegral <$> decimal
  , Var <$> parseName
  ] <?> "atomic expression"

parseComplex :: Parser CoreExpr
parseComplex = undefined

parseExpr :: Parser CoreExpr
parseExpr = parseAtomic

parseName :: Parser Name
parseName = Name <$> ident haskellIdents
