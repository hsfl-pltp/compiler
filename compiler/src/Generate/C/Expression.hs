{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Expression
  ()
  where


import qualified Generate.C.Builder as C
import qualified AST.Optimized as Opt


-- generiert den Ausdruck des vorliegenden Datentypen
generate :: Opt.Expr -> C.Expr
generate expr =
  case expr of
    Opt.Bool bool ->
      return CExpr (C.Bool bool)
    Opt.String string ->
      return CExpr (C.String string)
    Opt.Int int ->
      return CExpr (C.Integer int)
    Opt.Double double ->
      return CExpr (C.Double double)


data Code
  = CExpr C.Expr
