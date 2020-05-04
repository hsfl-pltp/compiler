{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.Arduino.Expression
  ( generate
  , codeToExpr
  , Code
  ) where

import Data.ByteString.Builder as B

import qualified AST.Optimized as Opt
import qualified Generate.Arduino.Builder as C

import qualified Elm.Float as EF
import qualified Elm.String as ES

import qualified Data.Utf8 as Utf8

generateArduinoExpr :: Opt.Expr -> C.Expr
generateArduinoExpr expression =
  codeToExpr (generate expression)

-- generates Expression for Type
generate :: Opt.Expr -> Code
generate expr =
  case expr of
    Opt.Bool bool -> CExpr (C.Bool bool)
    Opt.Str string -> CExpr (C.String (convertString string))
    Opt.Int int -> CExpr (C.Double (convertInt int))
    Opt.Float float -> CExpr (C.Double (convertFloat float))
    Opt.If branches final -> generateIf branches final
    _ -> error (show expr)

data Code
  = CExpr C.Expr
  | CBlock [C.Stmt]

codeToExpr :: Code -> C.Expr
codeToExpr code =
  case code of
    CExpr expr -> expr
    CBlock [C.Return expr] -> expr
    -- CBlock stmts ->
    --   JS.Call (JS.Function Nothing [] stmts) []

convertString :: ES.String -> B.Builder
convertString string = Utf8.toBuilder string

convertInt :: Int -> B.Builder
convertInt int = B.intDec int

convertFloat :: EF.Float -> B.Builder
convertFloat float = Utf8.toBuilder float


-- GENERATE IFS

generateIf :: [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> Code
generateIf givenBranches givenFinal =
  let
    (branches, final) =
      crushIfs givenBranches givenFinal

    convertBranch (condition, expr) =
      ( generateArduinoExpr condition
      , generate expr
      )

    branchExprs = map convertBranch branches
    finalCode = generate final
  in
  if isBlock finalCode || any (isBlock . snd) branchExprs then
    CBlock [ foldr addStmtIf (codeToStmt finalCode) branchExprs ]
  else
    CExpr $ foldr addExprIf (codeToExpr finalCode) branchExprs


addExprIf :: (C.Expr, Code) -> C.Expr -> C.Expr
addExprIf (condition, branch) final =
  C.If condition (codeToExpr branch) final


addStmtIf :: (C.Expr, Code) -> C.Stmt -> C.Stmt
addStmtIf (condition, branch) final =
  C.IfStmt condition (codeToStmt branch) final


isBlock :: Code -> Bool
isBlock code =
  case code of
    CBlock _ -> True
    CExpr _ -> False


crushIfs :: [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfs branches final =
  crushIfsHelp [] branches final


crushIfsHelp
    :: [(Opt.Expr, Opt.Expr)]
    -> [(Opt.Expr, Opt.Expr)]
    -> Opt.Expr
    -> ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfsHelp visitedBranches unvisitedBranches final =
  case unvisitedBranches of
    [] ->
        case final of
          Opt.If subBranches subFinal ->
              crushIfsHelp visitedBranches subBranches subFinal

          _ ->
              (reverse visitedBranches, final)

    visiting : unvisited ->
        crushIfsHelp (visiting : visitedBranches) unvisited final
 