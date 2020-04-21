{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.C.Expression
  (
  generate,
  codeToExpr,
  Code
  ) where

import           Data.ByteString.Builder as B

import qualified AST.Optimized           as Opt
import qualified Generate.C.Builder      as C

import qualified Elm.Float               as EF
import qualified Elm.String              as ES

import qualified Data.Utf8               as Utf8

-- generates Expression for Type
generate :: Opt.Expr -> Code
generate expr =
  case expr of
    Opt.Bool bool   -> CExpr (C.Bool bool)
    Opt.Str string  -> CExpr (C.String (convertString string))
    Opt.Int int     -> CExpr (C.Double (convertInt int))
    Opt.Float float -> CExpr (C.Double (convertFloat float))
    _               -> error (show expr)

data Code
    = CExpr C.Expr
    | CBlock [C.Stmt]


codeToExpr :: Code -> C.Expr
codeToExpr code =
  case code of
    CExpr expr ->
      expr

    CBlock [ C.Return expr ] ->
      expr

    -- CBlock stmts ->
    --   JS.Call (JS.Function Nothing [] stmts) []


convertString :: ES.String -> B.Builder
convertString string = Utf8.toBuilder string

convertInt :: Int -> B.Builder
convertInt int = B.intDec int

convertFloat :: EF.Float -> B.Builder
convertFloat float = Utf8.toBuilder float
