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
import qualified Reporting.Annotation as A
import Data.ByteString.Builder as B
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Compiler.Type as Type
import qualified AST.Optimized as Opt
import qualified Generate.Arduino.Builder as Arduino
import qualified Data.Name as Name
import qualified Data.List as List
import qualified Data.IntMap as IntMap
import qualified Elm.Float as EF
import qualified Elm.String as ES
import qualified Generate.Arduino.Name as ArduinoName
import qualified Data.Utf8 as Utf8
import qualified Generate.Mode as Mode
import qualified Elm.Package as Pkg
import qualified Data.Index as Index
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
    Opt.VarKernel home name -> CExpr (Arduino.Ref (ArduinoName.fromKernel home name))
    Opt.Call func args -> CExpr (generateCall func args)
    Opt.VarEnum (Opt.Global home name) index ->
        CExpr (Arduino.Enum (ArduinoName.fromLocal name) ( Arduino.Int (Index.toMachine index)))
    Opt.VarDebug name home region unhandledValueName -> CExpr (generateDebug name home region unhandledValueName)
    Opt.Function args body ->
      generateFunction (map ArduinoName.fromLocal args) (generate body)
    Opt.VarLocal name ->
      CExpr $ Arduino.Ref (ArduinoName.fromLocal name)
    Opt.VarGlobal (Opt.Global home name) ->
      CExpr $ Arduino.Ref (ArduinoName.fromGlobal home name) 
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

generateCall :: Opt.Expr -> [Opt.Expr] -> Arduino.Expr
generateCall func args  =
  case func of
    Opt.VarGlobal global@(Opt.Global (ModuleName.Canonical pkg _) _) | pkg == Pkg.core ->
      generateCoreCall global args

    Opt.VarBox _ ->
      case args of
        [arg] ->
          generateArduinoExpr arg
        _ ->
          generateCallHelp func args
    _ ->
      generateCallHelp func args

generateCallHelp :: Opt.Expr -> [Opt.Expr] -> Arduino.Expr
generateCallHelp func args =
  generateNormalCall (generateArduinoExpr func) (map (generateArduinoExpr) args)

generateNormalCall :: Arduino.Expr -> [Arduino.Expr] -> Arduino.Expr
generateNormalCall func args =
  case IntMap.lookup (length args) callHelpers of
    Just helper ->
      Arduino.Call helper (func:args)

    Nothing ->
      List.foldl' (\f a -> Arduino.Call f [a]) func args

callHelpers :: IntMap.IntMap Arduino.Expr
callHelpers =
  IntMap.fromList (map (\n -> (n, Arduino.Ref (ArduinoName.makeA n))) [2..9])

generateGlobalCall :: ModuleName.Canonical -> Name.Name -> [Arduino.Expr] -> Arduino.Expr
generateGlobalCall home name args =
  generateNormalCall (Arduino.Ref (ArduinoName.fromGlobal home name)) args

generateCoreCall :: Opt.Global -> [Opt.Expr] -> Arduino.Expr
generateCoreCall (Opt.Global home@(ModuleName.Canonical _ moduleName) name) args =
    if moduleName == Name.basics then
      generateBasicsCall home name args
    else
      generateGlobalCall home name (map (generateArduinoExpr) args)

generateBasicsCall :: ModuleName.Canonical -> Name.Name -> [Opt.Expr] -> Arduino.Expr
generateBasicsCall home name args =
  case args of
    [elmArg] ->
      let arg = generateArduinoExpr elmArg in
        case name of
          "not"      -> Arduino.Prefix Arduino.PrefixNot arg
          "negate"   -> Arduino.Prefix Arduino.PrefixNegate arg
          "toFloat"  -> arg
          _          -> generateGlobalCall home name [arg]

    [elmLeft, elmRight] ->
      case name of
        -- NOTE: removed "composeL" and "composeR" because of this issue:
        -- https://github.com/elm/compiler/issues/1722
        "append"   -> append elmLeft elmRight
        "apL"      -> generateArduinoExpr (apply elmLeft elmRight)
        "apR"      -> generateArduinoExpr (apply elmRight elmLeft)
        _ ->
          let
            left = generateArduinoExpr elmLeft
            right = generateArduinoExpr elmRight
          in
           case name of
            "add"  -> Arduino.Infix Arduino.OpAdd left right
            _      -> generateGlobalCall home name [left, right]

    _ ->
      generateGlobalCall home name (map (generateArduinoExpr) args)


apply :: Opt.Expr -> Opt.Expr -> Opt.Expr
apply func value =
  case func of
    Opt.Accessor field ->
      Opt.Access value field

    Opt.Call f args ->
      Opt.Call f (args ++ [value])

    _ ->
      Opt.Call func [value]

append :: Opt.Expr -> Opt.Expr -> Arduino.Expr
append left right =
  let seqs = generateArduinoExpr left : toSeqs right in
  if any isStringLiteral seqs then
    foldr1 (Arduino.Infix Arduino.OpAdd) seqs
  else
    foldr1 arduinoAppend seqs


arduinoAppend :: Arduino.Expr -> Arduino.Expr -> Arduino.Expr
arduinoAppend a b =
  Arduino.Call (Arduino.Ref (ArduinoName.fromKernel Name.utils "ap")) [a, b]


toSeqs :: Opt.Expr -> [Arduino.Expr]
toSeqs expr =
  case expr of
    Opt.Call (Opt.VarGlobal (Opt.Global home "append")) [left, right]
      | home == ModuleName.basics ->
        generateArduinoExpr left : toSeqs right

    _ ->
      [generateArduinoExpr expr]

isStringLiteral :: Arduino.Expr -> Bool
isStringLiteral expr =
  case expr of
    Arduino.String _ ->
      True

    _ ->
      False

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

generateDebug :: Name.Name -> ModuleName.Canonical -> A.Region -> Maybe Name.Name -> Arduino.Expr
generateDebug name (ModuleName.Canonical _ home) region unhandledValueName =
  if name /= "todo" then
    Arduino.Ref (ArduinoName.fromGlobal ModuleName.debug name)
  else
    case unhandledValueName of
      Nothing ->
        Arduino.Call (Arduino.Ref (ArduinoName.fromKernel Name.debug "todo")) $
          [ Arduino.String (Name.toBuilder home)
          , regionToArduinoExpr region
          ]

      Just valueName ->
        Arduino.Call (Arduino.Ref (ArduinoName.fromKernel Name.debug "todoCase")) $
          [ Arduino.String (Name.toBuilder home)
          , regionToArduinoExpr region
          , Arduino.Ref (ArduinoName.fromLocal valueName)
          ]

regionToArduinoExpr :: A.Region -> Arduino.Expr
regionToArduinoExpr (A.Region start end) =
    Arduino.Object
      [ ( ArduinoName.fromLocal "start", positionToArduinoExpr start )
      , ( ArduinoName.fromLocal "end", positionToArduinoExpr end )
      ]

positionToArduinoExpr :: A.Position -> Arduino.Expr
positionToArduinoExpr (A.Position line column) =
    Arduino.Object
      [ ( ArduinoName.fromLocal "line", Arduino.Int (fromIntegral line) )
      , ( ArduinoName.fromLocal "column", Arduino.Int (fromIntegral column) )
      ]

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


generateFunction :: [ArduinoName.Name] -> Code -> Code
generateFunction args body =
  CExpr $ Arduino.Function Nothing args $
    codeToStmtList body


codeToStmtList :: Code -> [Arduino.Stmt]
codeToStmtList code =
  case code of
    CExpr (Arduino.Call (Arduino.Function Nothing [] stmts) []) ->
        stmts

    CExpr expr ->
        [ Arduino.Return expr ]

    CBlock stmts ->
        stmts