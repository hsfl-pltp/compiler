{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.CStmt
  (Expr(..), pretty, Stmt(..), PrefixOp(..))
  where
import Data.Typeable
import Data.ByteString.Builder as B

-- Expressions
data Expr
    = String String
    | Null 
    | Bool Bool
    | Integer Integer
    | Double Double
    | If Expr Expr Expr
    | Prefix PrefixOp Expr
    | Infix InfixOp Expr Expr




-- STATEMENTS

data Stmt
    = Block [Stmt]
    | EmptyStmt
    | Var String String Expr
    | IfStmt Expr Stmt Stmt
    


--Die Funktion soll ein Stmt nehmen und diesen in ein C-Program in Form eines Strings schreiben.
pretty :: Stmt -> String
pretty statement =
  case statement of
    Var dataType name expr ->
      case dataType of
        "Integer" -> "int " ++ name ++ " = " ++ (prettyExpr expr) ++ ";\n"
        "Double" -> "double " ++ name ++ " = " ++ (prettyExpr expr) ++ ";\n"
        "String" ->
          "string" ++ " " ++ name ++ " = " ++ (prettyExpr expr) ++ ";\n"

        "Bool" ->
          "bool" ++ " " ++ name ++ " = " ++ (prettyExpr expr) ++ ";\n"
    Block array ->
      concat (map pretty array)
    IfStmt condition thenStmt elseStmt ->
      concat
        ["if (", (prettyExpr condition), ") {\n"
        , (pretty thenStmt)
        , "} else {\n"
        , (pretty elseStmt) ,"}\n"
        ]


--Die Funktion wandelt ein Argument des typen Expr in einen String um.
prettyExpr :: Expr -> String
prettyExpr expression =
  case expression of
    
    Bool bool ->
     case bool of
       True ->  "true"
       False -> "false"
    
    String string -> string

    If infixExpr expr1 expr2 -> (prettyExpr expr1) ++ " " ++ (prettyExpr infixExpr) ++ " " ++ (prettyExpr expr2)
    
    Null -> "null"

    Integer integer -> (show integer)

    Double double -> (show double)

    Infix infixoperator expr1 expr2 -> (prettyExpr expr1) ++ " " ++ (prettyInfix infixoperator) ++ " " ++ (prettyExpr expr2)

    Prefix prefixOperator expr1 -> (prettyPrefix prefixOperator) ++ (prettyExpr expr1)



data InfixOp
  = OpAdd -- +
  | OpSub -- -
  | OpMul -- *
  | OpDiv -- /
  | OpMod -- %
  | OpEq -- ===
  | OpNe -- !==
  | OpLt -- <
  | OpLe -- <=
  | OpGt -- >
  | OpGe -- >=
  | OpAnd -- &&
  | OpOr  -- ||
  | OpBitwiseAnd -- &
  | OpBitwiseXor -- ^
  | OpBitwiseOr  -- |
  | OpLShift     -- <<
  | OpSpRShift   -- >>
  | OpZfRShift   -- >>>


data PrefixOp
  = PrefixNot        -- !
  | PrefixNegate     -- -
  | PrefixComplement -- ~+


prettyInfix :: InfixOp -> String
prettyInfix minfix =
  case minfix of 
    OpAdd -> " + "
    OpSub -> " - "
    OpMul -> " * " -- *
    OpDiv -> " / " -- /
    OpMod -> " % "
    OpEq -> " == "
    OpNe -> " != "
    OpLt -> " < "
    OpLe -> " <= "
    OpGt -> " > "
    OpGe -> " >= "
    OpAnd -> " && "
    OpOr -> " || "
    OpBitwiseAnd -> " & "
    OpBitwiseXor -> " ^ "
    OpBitwiseOr  -> " | "
    OpLShift     -> " << "
    OpSpRShift   -> " >> "
    OpZfRShift   -> " >>> "

prettyPrefix :: PrefixOp -> String
prettyPrefix mprefix =
  case mprefix of 
    PrefixNot -> "!"
    PrefixNegate -> "-"
    PrefixComplement -> "~+"
