{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.Arduino.Expression
  ( generate
  , codeToExpr
  , generateMain
  , Code
  ) where
import qualified AST.Canonical as Can
import qualified Elm.Version as V
import Data.ByteString.Builder as B
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Compiler.Type as Type
import qualified AST.Optimized as Opt
import qualified Generate.Arduino.Builder as Arduino
import qualified Data.Name as Name
import qualified Elm.Float as EF
import qualified Elm.String as ES
import qualified Generate.Arduino.Name as ArduinoName
import qualified Data.Utf8 as Utf8
import qualified Generate.Mode as Mode

generateArduinoExpr :: Opt.Expr -> Arduino.Expr
generateArduinoExpr expression =
  codeToExpr (generate expression)

-- generates Expression for Type
generate :: Opt.Expr -> Code
generate expr =
  case expr of
    Opt.Bool bool -> CExpr (Arduino.Bool bool)
    Opt.Str string -> CExpr (Arduino.String (convertString string))
    Opt.Int int -> CExpr (Arduino.Double (convertInt int))
    Opt.Float float -> CExpr (Arduino.Double (convertFloat float))
    Opt.If branches final -> generateIf branches final
    _ -> error (show expr)

data Code
  = CExpr Arduino.Expr
  | CBlock [Arduino.Stmt]

codeToExpr :: Code -> Arduino.Expr
codeToExpr code =
  case code of
    CExpr expr -> expr
    CBlock [Arduino.Return expr] -> expr
    CBlock stmts ->
       Arduino.Call (Arduino.Function Nothing [] stmts) []

codeToStmt :: Code -> Arduino.Stmt
codeToStmt code =
  case code of
    CExpr expr -> Arduino.Return expr
    CBlock [stmt] -> stmt

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


addExprIf :: (Arduino.Expr, Code) -> Arduino.Expr -> Arduino.Expr
addExprIf (condition, branch) final =
  Arduino.If condition (codeToExpr branch) final


addStmtIf :: (Arduino.Expr, Code) -> Arduino.Stmt -> Arduino.Stmt
addStmtIf (condition, branch) final =
  Arduino.IfStmt condition (codeToStmt branch) final


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

-- GENERATE MAIN


generateMain :: Mode.Mode -> ModuleName.Canonical -> Opt.Main -> Arduino.Expr
generateMain mode home main =
  case main of
    Opt.Static ->
      Arduino.Ref (ArduinoName.fromKernel Name.virtualDom "init")
        # Arduino.Ref (ArduinoName.fromGlobal home "main")
        # Arduino.Int 0
        # Arduino.Int 0

    Opt.Dynamic msgType decoder ->
      Arduino.Ref (ArduinoName.fromGlobal home "main")
        # generateArduinoExpr decoder
       -- # toDebugMetadata mode msgType


(#) :: Arduino.Expr -> Arduino.Expr -> Arduino.Expr
(#) func arg =
  Arduino.Call func [arg]


-- toDebugMetadata :: Mode.Mode -> Can.Type -> Arduino.Expr
-- toDebugMetadata mode msgType =
--   case mode of
--     Mode.Prod _ ->
--       Arduino.Integer 0

--     Mode.Dev Nothing ->
--       Arduino.Integer 0

    -- Mode.Dev (Just interfaces) ->
    --   JS.Json $ Encode.object $
    --     [ "versions" ==> Encode.object [ "elm" ==> V.encode V.compiler ]
    --     , "types"    ==> Type.encodeMetadata (Extract.fromMsg interfaces msgType)
    --     ]

 