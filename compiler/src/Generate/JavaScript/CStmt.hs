{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.CStmt
  (Expr(..), pretty,examplee, Stmt(..))
  where


-- Expressions
data Expr
    = String String
    | Null


-- STATEMENTS

data Stmt
    = Block [Stmt]
    | EmptyStmt
    | Var String String

examplee :: Stmt
examplee = Var "str" "Hi"

--Die Funktion soll ein Stmt nehmen und diesen in ein C-Program in Form eines Strings schreiben.
pretty :: Stmt -> String
pretty statement =
  case statement of
    Var name expr ->
       "string " ++ name ++ " = " ++ expr ++ ";\n"
