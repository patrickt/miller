{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Main where

import Miller.Expr
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

add2 :: CoreProgram
add2 = [ Defn "two" [] (Var "succ" <> Num 1) ]

main :: IO ()
main = do
  putStrLn "\n\n\n\n"
  putDoc (pretty add2)
  putStrLn "\n\n\n\n"
