{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.C.Builder
  ( Expr(..)
  , pretty
  , Stmt(..)
  , PrefixOp(..)
  ) where

import           Data.ByteString.Builder as B
import qualified Data.List               as List

-- Expressions
data Expr
  = String Builder
  | Comma [Expr]
  | Null
  | Bool Bool
  | Integer Builder
  | Double Builder
  | If Expr Expr Expr
  | While Expr Expr Expr
  | Prefix PrefixOp Expr
  | Infix InfixOp Expr Expr

-- STATEMENTS
data Stmt
  = Block [Stmt]
  | EmptyStmt
  | Var String Builder Expr
  | Decl String Builder
  | Const Expr
  | Return Expr
  | IfStmt Expr Stmt Stmt
  | WhileStmt Expr Stmt
  | Function String Builder Stmt Stmt -- first Stmt is CommaStmt
  | CommaStmt [Stmt] -- [Stmt] is Decl

-- Converts a datatype in form of a String to the equivelant C-datatype.
-- Also returned as a String.
prettyDataType :: String -> Builder
prettyDataType dataType =
  case dataType of
    "Integer" -> "int"
    "Double"  -> "double"
    "String"  -> "string"
    "Bool"    -> "bool"
    "Void"    -> "void"

--This function takes a Stmt and converts it into a C-program as a string.
pretty :: Stmt -> Builder
pretty statement =
  case statement of
    Var dataType name expr ->
      mconcat
        [(prettyDataType dataType), " ", name, " = ", (prettyExpr expr), ";\n"]
    Block array -> mconcat (map pretty array)
    Const constExpr -> mconcat ["const", (prettyExpr constExpr), ";\n"]
    Decl dataType name -> mconcat [(prettyDataType dataType), " ", name]
    IfStmt condition thenStmt elseStmt ->
      mconcat
        [ "if("
        , prettyExpr condition
        , ") {\n"
        , pretty thenStmt
        , "} else {\n"
        , pretty elseStmt
        , "}\n"
        ]
    Return expr ->
      mconcat
        ["return ", (prettyExpr expr)]
    WhileStmt condition loopStmt ->
      mconcat
        ["while(", (prettyExpr condition), ") {\n", (pretty loopStmt), "}"]
    CommaStmt commastmt ->
      mconcat (List.intersperse ", " (map pretty commastmt))
    Function dataType name commastmt body ->
      mconcat
        [ prettyDataType dataType
        , " "
        , name
        , "("
        , pretty commastmt
        , ") {"
        , pretty body
        , "}"
        ]
    EmptyStmt -> error "Not supported EmptyStmt"

--Converts an argument of the type Expr into a String.
prettyExpr :: Expr -> Builder
prettyExpr expression =
  case expression of
    Bool bool ->
      case bool of
        True  -> "true"
        False -> "false"
    If infixExpr expr1 expr2 ->
      mconcat
        [prettyExpr expr1, " ", prettyExpr infixExpr, " ", prettyExpr expr2]
    Null -> "null"
    Integer integer -> integer
    Double double -> double
    Infix infixoperator expr1 expr2 ->
      mconcat
        [ prettyExpr expr1
        , " "
        , prettyInfix infixoperator
        , " "
        , prettyExpr expr2
        ]
    Prefix prefixOperator expr1 ->
      mconcat [prettyPrefix prefixOperator, prettyExpr expr1]
    String _ -> error "Not supported String"
    Comma _ -> error "Not supported Comma"
    While _ _ _ -> error "Not supported While"

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
  | OpOr -- ||
  | OpBitwiseAnd -- &
  | OpBitwiseXor -- ^
  | OpBitwiseOr -- |
  | OpLShift -- <<
  | OpSpRShift -- >>
  | OpZfRShift -- >>>

data PrefixOp
  = PrefixNot -- !
  | PrefixNegate -- -
  | PrefixComplement -- ~+

prettyInfix :: InfixOp -> Builder
prettyInfix minfix =
  case minfix of
    OpAdd        -> " + "
    OpSub        -> " - "
    OpMul        -> " * " -- *
    OpDiv        -> " / " -- /
    OpMod        -> " % "
    OpEq         -> " == "
    OpNe         -> " != "
    OpLt         -> " < "
    OpLe         -> " <= "
    OpGt         -> " > "
    OpGe         -> " >= "
    OpAnd        -> " && "
    OpOr         -> " || "
    OpBitwiseAnd -> " & "
    OpBitwiseXor -> " ^ "
    OpBitwiseOr  -> " | "
    OpLShift     -> " << "
    OpSpRShift   -> " >> "
    OpZfRShift   -> " >>> "

prettyPrefix :: PrefixOp -> Builder
prettyPrefix mprefix =
  case mprefix of
    PrefixNot        -> "!"
    PrefixNegate     -> "-"
    PrefixComplement -> "~+"
