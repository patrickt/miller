{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miller.Parser (parseIO)
import Miller.Pretty (showProgram, renderShow, renderShow)
import Miller.TI qualified as TI
import Miller.TI.Node qualified as Node
import Options.Applicative qualified as Opt
import Doors

data Cli
  = Parse FilePath
  | Ast FilePath
  | Run FilePath Bool Bool
    deriving stock (Show, Eq)

-- I hate this library.
cliParser :: Opt.Parser Cli
cliParser = Opt.hsubparser (mconcat [parseCommand, astCommand, runCommand])
  where
    cmd name p desc = Opt.command name (Opt.info p (Opt.progDesc desc))
    file = Opt.argument Opt.str (Opt.metavar "[FILENAME]")
    doRun
      = Run
      <$> file
      <*> Opt.switch (Opt.long "debug" <> Opt.help "Enable debugger")
      <*> Opt.switch (Opt.long "stats" <> Opt.help "Print stats")
    parseCommand = cmd "parse" (Parse <$> file) "parse and print source"
    astCommand = cmd "ast" (Ast <$> file) "parse and print AST"
    runCommand = cmd "run" doRun "run some source"

run :: Cli -> IO ()
run (Run file dbg showStats) = do
  eAST <- parseIO file
  case eAST of
    Left err -> fail err
    Right p -> do
      let executor = if dbg then TI.debugTI else TI.runTI
      (eRes, mach, stats) <- executor (TI.execute p)
      case eRes of
        Left err -> putStrLn ("error: " <> show err)
        Right (Node.NNum i) -> print i
        Right other -> print other
      when showStats $ do
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
  opts <- Opt.execParser (Opt.info (cliParser Opt.<**> Opt.helper) Opt.fullDesc)
  run opts
