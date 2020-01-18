{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.CStmt
  (Expr(..), fromStmt)
  where

-- Based on the language-ecmascript package.
-- https://hackage.haskell.org/package/language-ecmascript
-- They did the hard work of reading the spec to figure out
-- how all the types should fit together.

import Prelude hiding (lines)
import Data.ByteString.Builder as B

-- Expressions
data Expr
    = String Builder
    | Null

data LValue
  = LRef String

data Name
  = AnyString

data Case
  = Case Expr [Stmt]
  | Default [Stmt]

    -- STATEMENTS


data Stmt
    = Block [Stmt]
    | EmptyStmt
    | Var Builder Builder


    -- Die Funktion soll ein Statement in einen formatierten String umwandeln,
    -- welcher den string als C-Programm darstellt.
fromStmt :: Stmt -> Builder
fromStmt statement =
  case statement of
    Var name expr ->
       "string " <> name <> " = " <> expr <> ";\n"
