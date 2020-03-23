{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Builder
  (Expr(..), pretty, Stmt(..), PrefixOp(..))
  where
import Data.Typeable
import Data.ByteString.Builder as B

-- Expressions
data Expr
    = String String
    | Comma [Expr]
    | Null 
    | Bool Bool
    | Integer Integer
    | Double Double
    | If Expr Expr Expr
    | While Expr Expr Expr
    | Prefix PrefixOp Expr
    | Infix InfixOp Expr Expr



-- STATEMENTS

data Stmt
    = Block [Stmt]
    | EmptyStmt
    | Var String String Expr
    | Decl String String
    | Const Expr
    | IfStmt Expr Stmt Stmt
    | WhileStmt Expr Stmt
    | Function String String Stmt Stmt -- first Stmt is CommaStmt
    | CommaStmt [Stmt] -- [Stmt] is Decl
    

-- Converts a datatype in form of a String to the equivelant C-datatype.
-- Also returned as a String.
prettyDataType :: String -> String
prettyDataType dataType  =
  case dataType of
    "Integer" -> "int" 
    "Double" ->  "double" 
    "String" ->  "string" 
    "Bool" -> "bool" 
    "Void" -> "void"
  
--This function takes a Stmt and converts it into a C-program as a string.
pretty :: Stmt -> String
pretty statement =
  case statement of
    Var dataType name expr ->
      (prettyDataType dataType) ++ " " ++ name ++ " = " ++ (prettyExpr expr) ++ ";\n"
    Block array ->
      concat (map pretty array)
    Const constExpr ->
      "const" ++ (prettyExpr constExpr) ++ ";\n"
    Decl dataType name ->
      (prettyDataType dataType) ++ " " ++ name ++";\n"
    IfStmt condition thenStmt elseStmt ->
      concat
        ["if(", (prettyExpr condition), ") {\n"
        , (pretty thenStmt)
        , "} else {\n"
        , (pretty elseStmt) ,"}\n"
        ]
    WhileStmt condition loopStmt ->
      concat
       ["while(", (prettyExpr condition), ") {\n"
       , (pretty loopStmt)
       , "}"
       ]
    -- CommaStmt commastmt -> 
    --   concat (intersperse "," commastmt) 
    Function dataType name commastmt body ->
      concat
        [(prettyDataType dataType)
        , " " ++ name ++ "("
        , (pretty commastmt)
        , ") {"
        , (pretty body)
        , "}"
        ]
      


--Converts an argument of the type Expr into a String.
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
