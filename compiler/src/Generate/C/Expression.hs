{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Expression
  ()
  where

import Data.ByteString.Builder as B

import qualified Generate.C.Builder as C
import qualified AST.Optimized as Opt

import qualified Elm.String as ES
import qualified Elm.Float as EF

import qualified Data.Utf8 as Utf8


-- generates Expression for Type
generate :: Opt.Expr -> C.Expr
generate expr =
  case expr of
    Opt.Bool bool ->
      C.Bool bool
    Opt.Str string ->
      C.String (convertString string)
    Opt.Int int ->
      C.Double (convertInt int)
    Opt.Float float ->
      C.Double (convertFloat float)
    _ ->
      error (show (expr))

convertString :: ES.String -> B.Builder
convertString string =
  Utf8.toBuilder string

convertInt :: Int -> B.Builder
convertInt int =
  B.intDec int

convertFloat :: EF.Float -> B.Builder
convertFloat float =
  Utf8.toBuilder float
