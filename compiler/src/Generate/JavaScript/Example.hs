{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Example
  (test)
  where

--Beispiel Konstruktor um die pretty Funktion zu testen.
import Generate.JavaScript.CStmt 


example :: Stmt
example = Block [Var "String" "str" (String "hi"), Var "Bool" "boolean" (Bool False)]

test1 :: String
test1 = pretty example
