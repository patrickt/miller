{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Miller.Parser where

import           Control.Applicative
import           Control.Monad
import Data.List.NonEmpty
-- import qualified Data.Text as T
-- import           GHC.Exts
-- import           Text.Parser.Char
-- import           Text.Parser.Token
-- import           Text.Parser.Token.Style (haskellIdents)
-- import           Text.Trifecta
import Data.Text (Text)
import Text.Parser.Token as Token
import Text.Trifecta
import qualified Text.Parser.Token.Highlight as HL

import Miller.Expr

identStyle :: (Alternative m, CharParsing m) => Token.IdentifierStyle m
identStyle = IdentifierStyle "miller" letter (alphaNum <|> char '\'') kws HL.Identifier HL.ReservedIdentifier where
  kws = ["let", "letrec", "in"]

reserved :: (Monad m, TokenParsing m) => Text -> m ()
reserved = Token.reserveText identStyle

parseName :: (Monad m, TokenParsing m) => m Name
parseName = Name <$> Token.ident identStyle

parseDefn :: (Monad m, TokenParsing m) => m CoreDefn
parseDefn = Defn <$> parseName <*> many parseName <*> (equals *> parseExpr)

parseExpr :: (Monad m, TokenParsing m) => m CoreExpr
parseExpr = choice
  [ Let <$> parseRec <*> bindings <*> (reserved "in" *> parseExpr)
  , parseAtomic
  ]

equals :: TokenParsing m => m ()
equals = void (symbolic '=')

parseAtomic :: (Monad m, TokenParsing m) => m CoreExpr
parseAtomic = choice
  [ Num . fromIntegral <$> Token.integer
  , Var                <$> parseName
  ]

bindings :: (Monad m, TokenParsing m) => m (NonEmpty (Name, CoreExpr))
bindings = go `sepByNonEmpty` (symbolic ',') where
  go = liftA2 (,) parseName (equals *> parseExpr)

parseRec :: (Monad m, TokenParsing m) => m Rec
parseRec = choice [ Non <$ reserved "let"
                  , Rec <$ reserved "letrec"
                  ]

lambda :: CharParsing m => m Char
lambda = oneOf "\\λ"

parseProgram :: (Monad m, TokenParsing m) => m CoreProgram
parseProgram = toplevel (runUnlined (Program <$> parseDefn `sepEndByNonEmpty` newline))

toplevel :: (Monad m, TokenParsing m) => m a -> m a
toplevel f = f <* whiteSpace <* eof

-- parseProgram :: Parser CoreProgram
-- parseProgram = Program <$> parseDefn `sepEndByNonEmpty` newline

-- parseDefn :: Parser CoreDefn
-- parseDefn = Defn <$> parseName
--                  <*> many parseName
--                  <*> (token (char '=') *> parseExpr)

-- parseAtomic :: Parser CoreExpr
-- parseAtomic = choice
--   [ Num . fromIntegral <$> decimal
--   , Var <$> parseName
--   ] <?> "atomic expression"

-- parseComplex :: Parser CoreExpr
-- parseComplex = undefined

-- parseExpr :: Parser CoreExpr
-- parseExpr = parseAtomic

-- parseName :: Parser Name
-- parseName = Name <$> ident haskellIdents
