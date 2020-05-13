{-# LANGUAGE OverloadedStrings #-}

module Generate.Arduino
  ( generate
  ) where

import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import qualified Debug.Trace as T
import Prelude hiding (cycle, print)

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Data.Index as Index
import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Generate.Arduino.Builder as Arduino
import qualified Generate.Arduino.Expression as Expr
import qualified Generate.Arduino.Name as ArduinoName
import qualified Generate.Mode as Mode
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L

-- GENERATE
type Graph = Map.Map Opt.Global Opt.Node

type Mains = Map.Map ModuleName.Canonical Opt.Main

generate :: Mode.Mode -> Opt.GlobalGraph -> Mains -> B.Builder
generate mode (Opt.GlobalGraph graph _) mains =
  let state = Map.foldrWithKey (addMain mode graph) emptyState mains
   in "(function(scope){\n'use strict';"
      -- <> Functions.functions
       <>
      perfNote mode <>
      stateToBuilder state <> toMainExports mode mains <> "}(this));"

addMain ::
     Mode.Mode -> Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain mode graph home _ state =
  addGlobal mode graph state (Opt.Global home "main")

perfNote :: Mode.Mode -> B.Builder
perfNote mode =
  case mode of
    Mode.Prod _ -> ""
    Mode.Dev Nothing -> "serial.print('Compiled in DEV mode.')"
    Mode.Dev (Just _) -> "serial.print('Compiled in DEBUG mode.')"

-- GRAPH TRAVERSAL STATE
emptyState :: State
emptyState = State mempty [] Set.empty

data State =
  State
    { _revKernels :: [B.Builder]
    , _revBuilders :: [B.Builder]
    , _seenGlobals :: Set.Set Opt.Global
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
    else addGlobalHelp mode graph global $
         State revKernels builders (Set.insert global seen)

addGlobalHelp :: Mode.Mode -> Graph -> Opt.Global -> State -> State
addGlobalHelp mode graph global state =
  let addDeps deps someState = Set.foldl' (addGlobal mode graph) someState deps
   in case graph ! global of
        Opt.Define expr deps ->
          addStmt (addDeps deps state) (var global (Expr.generate expr))
    -- For testing purposes we ignore the kernel code
        Opt.Kernel chunks deps -- T.trace (show (Opt.Kernel chunks deps)) state
         -> state
        expr -> error ("unsupported argument: " ++ show expr)

addStmt :: State -> Arduino.Stmt -> State
addStmt state stmt = addBuilder state (Arduino.pretty stmt)

addBuilder :: State -> B.Builder -> State
addBuilder (State revKernels revBuilders seen) builder =
  State revKernels (builder : revBuilders) seen

var :: Opt.Global -> Expr.Code -> Arduino.Stmt
var (Opt.Global home name) code =
  Arduino.Var
    "any"
    (ArduinoName.toBuilder (ArduinoName.fromGlobal home name))
    (Expr.codeToExpr code)

-- MAIN EXPORTS
toMainExports :: Mode.Mode -> Mains -> B.Builder
toMainExports mode mains =
  let export = ArduinoName.fromKernel Name.platform "export"
      exports =
        generateExports mode (Map.foldrWithKey addToTrie emptyTrie mains)
   in ArduinoName.toBuilder export <> "(" <> exports <> ");"

generateExports :: Mode.Mode -> Trie -> B.Builder
generateExports mode (Trie maybeMain subs) =
  let starter end =
        case maybeMain of
          Nothing -> "{"
          Just (home, main) ->
            "{'init':" <>
            Arduino.prettyExpr (Expr.generateMain mode home main) <> end
   in case Map.toList subs of
        [] -> starter "" <> "}"
        (name, subTrie):otherSubTries ->
          starter "," <>
          "'" <>
          Utf8.toBuilder name <>
          "':" <>
          generateExports mode subTrie <>
          List.foldl' (addSubTrie mode) "}" otherSubTries

addSubTrie :: Mode.Mode -> B.Builder -> (Name.Name, Trie) -> B.Builder
addSubTrie mode end (name, trie) =
  ",'" <> Utf8.toBuilder name <> "':" <> generateExports mode trie <> end

-- BUILD TRIES
data Trie =
  Trie
    { _main :: Maybe (ModuleName.Canonical, Opt.Main)
    , _subs :: Map.Map Name.Name Trie
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
    segment:otherSegments ->
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
