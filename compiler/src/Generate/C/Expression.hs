{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Expression
  ()
  where

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
-- uncomment when Double type is adjusted
    --Opt.Float float ->
      --C.String (convertFloat float)


convertString :: ES.String -> String
convertString string =
  Utf8.toChars string

convertInt :: Int -> Double
convertInt int =
  fromIntegral int

--convertFloat :: EF.Float -> String
--convertFloat float =
  --Utf8.toChars float
