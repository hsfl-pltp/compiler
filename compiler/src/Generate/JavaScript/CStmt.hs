{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.CStmt
  (Expr(..), pretty, Stmt(..))
  where
import Data.Typeable

-- Expressions
data Expr
    = String String
    | Null
    | Bool Bool




-- STATEMENTS

data Stmt
    = Block [Stmt]
    | EmptyStmt
    | Var String String Expr
    


--Die Funktion soll ein Stmt nehmen und diesen in ein C-Program in Form eines Strings schreiben.
pretty :: Stmt -> String
pretty statement =
  case statement of
    Var dataType name expr ->
      case dataType of
        "String" ->
          "string" ++ " " ++ name ++ " = " ++ (prettyExpr expr) ++ ";\n"

        "Bool" ->
          "bool" ++ " " ++ name ++ " = " ++ (prettyExpr expr) ++ ";\n"
    Block array ->
      unwords (map pretty array)


--Die Funktion wandelt ein Argument des typen Expr in einen String um.
prettyExpr :: Expr -> String
prettyExpr expression =
  case expression of
    Bool bool ->
     case bool of
       True ->  "true"
       False -> "false"
    String string -> string
    Null -> "null"
