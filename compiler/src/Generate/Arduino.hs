{-# LANGUAGE OverloadedStrings #-}

module Generate.Arduino
  ( generate,
  )
where

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as By
import qualified Data.Index as Index
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import Debug.Trace (trace)
import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Generate.Arduino.Builder as Arduino
import qualified Generate.Arduino.Expression as Expr
import qualified Generate.Arduino.Functions as Functions
import qualified Generate.Arduino.Kernel as Kernel
import qualified Generate.Arduino.Name as ArduinoName
import qualified Generate.Boilerplate as BP
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Expression as JSExpr
import qualified Generate.JavaScript.Functions as JSFunctions
import qualified Generate.JavaScript.Name as JsName
import qualified Generate.Mode as Mode
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L
import Prelude hiding (cycle, print)

-- GENERATE
type Graph = Map.Map Opt.Global Opt.Node

type Mains = Map.Map ModuleName.Canonical Opt.Main

generate :: Mode.Mode -> Opt.GlobalGraph -> Mains -> B.Builder
generate mode (Opt.GlobalGraph graph _) mains =
  let state = Map.foldrWithKey (addMain mode graph) emptyState mains
   in Functions.functions
        <> Kernel.kernel
        <> stateToBuilder state
        <> BP.sandwichArduino (perfNote mode) (toMainNames mode mains)

addMain ::
  Mode.Mode -> Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain mode graph home _ state =
  addGlobal mode graph state (Opt.Global home "main")

perfNote :: Mode.Mode -> B.Builder
perfNote mode =
  case mode of
    Mode.Prod _ -> ""
    Mode.Dev Nothing -> "Serial.println(\"Compiled in DEV mode.\");"
    Mode.Dev (Just _) -> "Serial.println(\"Compiled in DEBUG mode.\");"

-- GRAPH TRAVERSAL STATE
emptyState :: State
emptyState = State mempty [] Set.empty

data State = State
  { _revKernels :: [B.Builder],
    _revBuilders :: [B.Builder],
    _seenGlobals :: Set.Set Opt.Global
  }

stateToBuilder :: State -> B.Builder
stateToBuilder (State revKernels revBuilders _) =
  prependBuilders revKernels (prependBuilders revBuilders mempty)

prependBuilders :: [B.Builder] -> B.Builder -> B.Builder
prependBuilders revBuilders monolith =
  List.foldl' (\m b -> b <> m) monolith revBuilders

-- ADD DEPENDENCIES
addGlobal :: Mode.Mode -> Graph -> State -> Opt.Global -> State
addGlobal mode graph state@(State revKernels builders seen) global =
  if Set.member global seen
    then state
    else
      addGlobalHelp mode graph global $
        State revKernels builders (Set.insert global seen)

addGlobalHelp :: Mode.Mode -> Graph -> Opt.Global -> State -> State
addGlobalHelp mode graph global state =
  let addDeps deps someState = Set.foldl' (addGlobal mode graph) someState deps
   in case graph ! global of
        Opt.Define expr deps ->
          addStmt (addDeps deps state) (var global (Expr.generate expr))
        -- For testing purposes we ignore the kernel code
        Opt.Kernel chunks dep -> state
        Opt.Enum index -> addStmt state (generateEnum mode global index)
        Opt.Box ->
          addStmt
            (addGlobal mode graph state identity)
            (generateBox mode global)
        expr -> error ("unsupported argument: " ++ show expr)

addStmt :: State -> Arduino.Stmt -> State
addStmt state stmt = addBuilder state (Arduino.stmtToBuilder stmt)

addBuilder :: State -> B.Builder -> State
addBuilder (State revKernels revBuilders seen) builder =
  State revKernels (builder : revBuilders) seen

addKernel :: State -> B.Builder -> State
addKernel (State revKernels revBuilders seen) kernel =
  State (kernel : revKernels) revBuilders seen

{-# NOINLINE identity #-}
identity :: Opt.Global
identity = Opt.Global ModuleName.basics Name.identity

var :: Opt.Global -> Expr.Code -> Arduino.Stmt
var (Opt.Global home name) code =
  Arduino.Var "any" (ArduinoName.fromGlobal home name) (Expr.codeToExpr code)

isDebugger :: Opt.Global -> Bool
isDebugger (Opt.Global (ModuleName.Canonical _ home) _) = home == Name.debugger

-- GENERATE ENUM
generateEnum :: Mode.Mode -> Opt.Global -> Index.ZeroBased -> Arduino.Stmt
generateEnum mode global@(Opt.Global home name) index =
  Arduino.Var "any" (ArduinoName.fromGlobal home name) $
    case mode of
      Mode.Dev _ -> Expr.codeToExpr (Expr.generateCtor mode global index 0)
      Mode.Prod _ -> Arduino.Int (Index.toMachine index)

-- GENERATE BOX
generateBox :: Mode.Mode -> Opt.Global -> Arduino.Stmt
generateBox mode global@(Opt.Global home name) =
  Arduino.Var "any" (ArduinoName.fromGlobal home name) $
    case mode of
      Mode.Dev _ -> Expr.codeToExpr (Expr.generateCtor mode global Index.first 1)
      Mode.Prod _ -> Arduino.Ref Arduino.Global (ArduinoName.fromGlobal ModuleName.basics Name.identity)

-- GENERATE KERNEL
generateKernel :: Mode.Mode -> [K.Chunk] -> B.Builder
generateKernel mode chunks = List.foldr (addChunk mode) mempty chunks

addChunk :: Mode.Mode -> K.Chunk -> B.Builder -> B.Builder
addChunk mode chunk builder =
  case chunk of
    K.JS javascript -> B.byteString javascript <> builder
    K.ElmVar home name ->
      builder
    K.JsVar home name ->
      builder
    K.ElmField name ->
      JsName.toBuilder (JSExpr.generateField mode name) <> builder
    K.JsField int -> JsName.toBuilder (JsName.fromInt int) <> builder
    K.JsEnum int -> B.intDec int <> builder
    K.Debug ->
      case mode of
        Mode.Dev _ -> builder
        Mode.Prod _ -> "_UNUSED" <> builder
    K.Prod ->
      case mode of
        Mode.Dev _ -> "_UNUSED" <> builder
        Mode.Prod _ -> builder

-- MAIN EXPORTS
toMainNames :: Mode.Mode -> Mains -> B.Builder
toMainNames mode mains =
  let mainNames =
        generateMainNames mode (Map.foldrWithKey addToTrie emptyTrie mains)
   in mainNames

generateMainNames :: Mode.Mode -> Trie -> B.Builder
generateMainNames mode (Trie maybeMain subs) =
  let starter end =
        case maybeMain of
          Nothing -> ""
          Just (home, main) ->
            Arduino.exprToBuilder (Expr.generateMain mode home main)
   in case Map.toList subs of
        [] -> starter ""
        (name, subTrie) : otherSubTries -> generateMainNames mode subTrie

addSubTries :: Mode.Mode -> B.Builder -> (Name.Name, Trie) -> B.Builder
addSubTries mode end (name, trie) =
  ",'" <> Utf8.toBuilder name <> "':" <> generateMainNames mode trie <> end

-- BUILD TRIES
data Trie = Trie
  { _main :: Maybe (ModuleName.Canonical, Opt.Main),
    _subs :: Map.Map Name.Name Trie
  }

emptyTrie :: Trie
emptyTrie = Trie Nothing Map.empty

addToTrie :: ModuleName.Canonical -> Opt.Main -> Trie -> Trie
addToTrie home@(ModuleName.Canonical _ moduleName) main trie =
  merge trie $ segmentsToTrie home (Name.splitDots moduleName) main

segmentsToTrie :: ModuleName.Canonical -> [Name.Name] -> Opt.Main -> Trie
segmentsToTrie home segments main =
  case segments of
    [] -> Trie (Just (home, main)) Map.empty
    segment : otherSegments ->
      Trie
        Nothing
        (Map.singleton segment (segmentsToTrie home otherSegments main))

merge :: Trie -> Trie -> Trie
merge (Trie main1 subs1) (Trie main2 subs2) =
  Trie (checkedMerge main1 main2) (Map.unionWith merge subs1 subs2)

checkedMerge :: Maybe a -> Maybe a -> Maybe a
checkedMerge a b =
  case (a, b) of
    (Nothing, main) -> main
    (main, Nothing) -> main
    (Just _, Just _) -> error "cannot have two modules with the same name"
