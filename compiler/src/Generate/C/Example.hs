{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Example
  (test1)
  where

--Beispiel Konstruktor um die pretty Funktion zu testen.
import Generate.C.Builder 
import Data.ByteString.Builder as B
import Data.Word

import qualified Data.ByteString.Lazy as L
builderToString :: Builder ->  L.ByteString
builderToString builder =
  B.toLazyByteString builder

lazyStringToWord :: L.ByteString -> [Word8]
lazyStringToWord bs = L.unpack bs

example1 :: Stmt
example1 = Block [Function "Void"  "testFunc" (CommaStmt [(Decl "Bool" "boolean"),(Decl "Bool" "boolean2") ]) (Var "String" "str" (String "hi")) ,Var "String" "str" (String "hi"), Var "Bool" "boolean" (Bool False), IfStmt (Prefix PrefixNot (Bool True)) (Var "String" "str" (String "hi")) (Var "String" "str" (String "hi"))]


test1 :: [Word8]
test1 = lazyStringToWord(builderToString (pretty example1))
