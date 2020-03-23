{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Example
  (test1)
  where

--Beispiel Konstruktor um die pretty Funktion zu testen.
import Generate.C.Builder 


example1 :: Stmt
example1 = Block [Function "Void"  "testFunc" (CommaStmt [(Decl "Bool" "boolean"),(Decl "Bool" "boolean2") ]) (Var "String" "str" (String "hi")) ,Var "String" "str" (String "hi"), Var "Bool" "boolean" (Bool False), IfStmt (Prefix PrefixNot (Bool True)) (Var "String" "str" (String "hi")) (Var "String" "str" (String "hi")), Var "Double" "double1" (Double 1.0)]


test1 :: String
test1 = pretty example1
