{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.C.Example
  ( test1
  , test2
  , test3
  ) where

import           Data.ByteString.Builder   as B
import           Data.ByteString.Lazy.UTF8 (toString)

--Beispiel Konstruktor um die pretty Funktion zu testen.
import           Generate.C.Builder

import qualified Data.ByteString.Lazy      as L

builderToString :: Builder -> L.ByteString
builderToString builder = B.toLazyByteString builder

example1 :: Stmt
example1 = Var "String" "x" (Bool False)

test1 :: String
test1 = toString (builderToString (pretty example1))

example2 :: Stmt
example2 =
  IfStmt
    (Prefix PrefixNot (Bool True))
    (Var "String" "x" (Bool False))
    (Var "String" "str" (String "hi"))

test2 :: String
test2 = toString (builderToString (pretty example2))

example3 :: Stmt
example3 =
  Block
    [ Function
        "Void"
        "testFunc"
        (CommaStmt [(Decl "Bool" "boolean"), (Decl "Bool" "boolean2")])
        (Var "String" "str" (String "hi"))
    , Var "String" "str" (String "hi")
    , Var "Bool" "boolean" (Bool False)
    , IfStmt
        (Prefix PrefixNot (Bool True))
        (Var "String" "str" (String "hi"))
        (Var "String" "str" (String "hi"))
    ]

test3 :: String
test3 = toString (builderToString (pretty example3))
