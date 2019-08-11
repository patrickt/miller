{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Miller.Parser
  ( parseExpr
  , parseProgram
  , keywords
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.List.NonEmpty
import           Data.Text (Text)
import           Text.Parser.Expression
import           Text.Parser.Token as Token
import qualified Text.Parser.Token.Highlight as HL
import           Text.Trifecta

import Miller.Expr

keywords :: [Text]
keywords = ["let", "letrec", "in", "case"]

identStyle :: (Alternative m, CharParsing m) => Token.IdentifierStyle m
identStyle = IdentifierStyle "identifier" letter (alphaNum <|> char '\'') kws HL.Identifier HL.ReservedIdentifier where
  kws = ["let", "letrec", "in", "case", "*", "+"]

int :: (Monad m, TokenParsing m) => m Int
int = fromIntegral <$> Token.natural

reserved :: (Monad m, TokenParsing m) => Text -> m ()
reserved = Token.reserveText identStyle

parseName :: (Monad m, TokenParsing m) => m Name
parseName = Name <$> Token.ident identStyle

parseDefn :: (Monad m, TokenParsing m) => m CoreDefn
parseDefn = Defn <$> parseName <*> many parseName <*> (equals *> parseExpr)

parseAtomic :: (Monad m, TokenParsing m) => m CoreExpr
parseAtomic = choice
  [ parens parseExpr
  , Let  <$> parseRec <*> bindings <*> (reserved "in" *> parseExpr)
  , Case <$> (reserved "case" *> parseExpr <* reserved "of") <*> cases
  , Lam  <$> (lambda *> many parseName) <*> (symbol "->" *> parseExpr)
  , Num <$> int
  , Var <$> parseName
  ]

operators :: (Monad m, TokenParsing m) => OperatorTable m CoreExpr
operators =
  let binary tok typ = Infix (Binary typ <$ reserved tok) AssocLeft
  in [ [Infix (pure Ap) AssocLeft]
     , [binary "*" Mul ]
     , [binary "+" Add ]
     ]

parseExpr :: (Monad m, TokenParsing m) => m CoreExpr
parseExpr = buildExpressionParser operators parseAtomic

equals :: TokenParsing m => m ()
equals = void (symbolic '=')

bindings :: (Monad m, TokenParsing m) => m (NonEmpty (Name, CoreExpr))
bindings = go `sepByNonEmpty` (symbolic ';') where
  go = liftA2 (,) parseName (equals *> parseExpr)

cases :: (Monad m, TokenParsing m) => m (NonEmpty (Int, [Name], CoreExpr))
cases = go `sepByNonEmpty` (symbolic ';') where
  go = liftA3 (,,) int (many parseName) (symbol "->" *> parseExpr)

parseRec :: (Monad m, TokenParsing m) => m Rec
parseRec = choice [ Non <$ reserved "let"
                  , Rec <$ reserved "letrec"
                  ]

lambda :: TokenParsing m => m Char
lambda = token (oneOf "\\λ")

parseProgram :: (Monad m, TokenParsing m) => m CoreProgram
parseProgram = toplevel (runUnlined (Program <$> parseDefn `sepEndByNonEmpty` newline))

toplevel :: (Monad m, TokenParsing m) => m a -> m a
toplevel f = f <* whiteSpace <* eof
