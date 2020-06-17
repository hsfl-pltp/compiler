{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.Arduino.Builder
  ( Expr(..)
  , pretty
  , prettyExpr
  , Stmt(..)
  , PrefixOp(..)
  , InfixOp(..)
  ) where

import           Data.ByteString.Builder as B
import qualified Data.List               as List
import           Generate.Arduino.Name   (Name)
import qualified Generate.Arduino.Name   as Name

-- Expressions
data Expr
  = String Builder
  | Null
  | Ref Name
  | Bool Bool
  | Integer Builder
  | Int Int
  | Double Builder
  | If Expr Expr Expr
  | While Expr Expr Expr
  | Prefix PrefixOp Expr
  | Object [(Name, Expr)]
  | Call Expr [Expr]
  | Infix InfixOp Expr Expr
  | Function (Maybe Name) [Name] [Stmt]
  | Enum Name Expr


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
  | FunctionStmt Name [Name] [Stmt]
  | EnumStmt Name Expr


-- Converts a datatype in form of a String to the equivelant C-datatype.
-- Also returned as a String.
prettyDataType :: String -> Builder
prettyDataType dataType =
  case dataType of
    "String"  -> "string"
    "Bool"    -> "bool"
    "Integer" -> "int"
    "Double"  -> "double"
    "Void"    -> "void"
    "Enum"    -> "enum"
    -- Dummy case used because type information is missing
    "any"     -> "void*"

--This function takes a Stmt and converts it into a C-program as a string.
pretty :: Stmt -> Builder
pretty statement =
  case statement of
    Block array -> mconcat (map pretty array)
    EmptyStmt -> error "Not supported EmptyStmt"
    Var dataType name expr ->
      mconcat
        [(prettyDataType dataType), " ", name, " = ", (prettyExpr expr), ";\n"]
    Decl dataType name -> mconcat [(prettyDataType dataType), " ", name]
    Const constExpr -> mconcat ["const", (prettyExpr constExpr), ";\n"]
    Return expr -> mconcat ["return ", (prettyExpr expr)]
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
    WhileStmt condition loopStmt ->
      mconcat
        ["while(", (prettyExpr condition), ") {\n", (pretty loopStmt), "}"]
    FunctionStmt name args stmts ->
      mconcat
        [ "void "
        , Name.toBuilder name
        , "("
        , commaSep (map Name.toBuilder args)
        , ") {\n"
        , fromStmtBlock stmts
        , "}\n"
        ]
    EnumStmt name exprs ->
      mconcat (mconcat ((mconcat ["enum ", Name.toBuilder name]) : ([prettyExpr exprs])) : [ "\n"])


fromStmtBlock :: [Stmt] -> Builder
fromStmtBlock stmts = mconcat (map pretty stmts)

--Converts an argument of the type Expr into a String.
prettyExpr :: Expr -> Builder
prettyExpr expression =
  case expression of
    String string -> mconcat [ "\"", string, "\""]
    Null -> "null"
    Ref name -> Name.toBuilder name
    Bool bool ->
      case bool of
        True  -> "true"
        False -> "false"
    Integer integer -> integer
    Int n -> B.intDec n
    Double double -> double
    If infixExpr expr1 expr2 ->
      mconcat
        ["if (", prettyExpr infixExpr,")", "{ \n", prettyExpr expr1, "\n}",  " else { \n ", prettyExpr expr2, "\n }"]
    While _ _ _ -> error "Not supported While"
    Prefix prefixOperator expr1 ->
      mconcat [prettyPrefix prefixOperator, prettyExpr expr1]
    Enum name exprs ->
      mconcat (mconcat ((mconcat ["enum ", Name.toBuilder name]) : ([ prettyExpr exprs])) : [ "\n"])

    Call expr1 exprs ->
      mconcat [ prettyExpr expr1
              , "("
              , fromExprBlock exprs
              , ")"]
    Infix infixoperator expr1 expr2 ->
      mconcat
        [ prettyExpr expr1
        , " "
        , prettyInfix infixoperator
        , " "
        , prettyExpr expr2
        ]
    Function maybeName args stmts ->
      mconcat
        [ "void "
        , maybe mempty Name.toBuilder maybeName
        , "(" <> commaSep (map Name.toBuilder args)
        , ") {\n"
        , fromStmtBlock stmts
        , "}"
        ]


commaSep :: [Builder] -> Builder
commaSep builders = mconcat (List.intersperse ", " builders)

fromExprBlock :: [Expr] -> Builder
fromExprBlock exprs = mconcat (List.intersperse ", " (map prettyExpr exprs))


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
