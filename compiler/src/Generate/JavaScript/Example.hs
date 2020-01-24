{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Example
  (example)
  where

--Beispiel Konstruktor um die pretty Funktion zu testen.
import Generate.JavaScript.CStmt 


example :: Stmt
example = Var "str" "Hi"
