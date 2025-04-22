{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic
import Miller.Parser (parseIO)
import Miller.Pretty (showProgram, renderShow, renderShow)
import Miller.TI qualified as TI

data Cli
  = Parse FilePath
  | Ast FilePath
  | Run FilePath
    deriving stock (Show, Eq, Generic)
    deriving anyclass ParseRecord

run :: Cli -> IO ()
run (Run file) = do
  eAST <- parseIO file
  case eAST of
    Left err -> fail err
    Right p -> do
      let (eRes, mach, stats) = TI.runTI (TI.execute p)
      print eRes
      putStrLn "***"
      putStrLn (renderShow mach)
      putStrLn "***"
      putStrLn (renderShow stats)
run (Ast file) = do
  eAST <- parseIO file
  case eAST of
    Left err -> fail err
    Right p -> putStrLn (renderShow p)
run (Parse file) = do
  eAST <- parseIO file
  case eAST of
    Left err -> fail err
    Right p -> putStrLn (showProgram p)

main :: IO ()
main = do
  opts <- getRecord "miller: a little ML"
  run opts
