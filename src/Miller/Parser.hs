{-# LANGUAGE OverloadedStrings #-}

module Miller.Parser where

import qualified Data.Text as T
import           Text.Parser.Char
import           Text.Trifecta

import Miller.Expr

parseProgram :: Parser CoreProgram
parseProgram = Program <$> (parseDefn `sepByNonEmpty` some (char '\n'))

parseDefn :: Parser CoreDefn
parseDefn = Defn <$> parseName
                 <*> (parseName `sepEndBy` char ' ')
                 <*> (string "=" *> spaces *> parseExpr)

parseExpr :: Parser CoreExpr
parseExpr = undefined

parseName :: Parser Name
parseName = Name . T.pack <$> some alphaNum
