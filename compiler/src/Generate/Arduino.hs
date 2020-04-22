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
import Prelude hiding (cycle, print)

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Data.Index as Index
import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Generate.Arduino.Builder as Arduino
import qualified Generate.Arduino.Expression as Expr
import qualified Generate.Arduino.Name as CName
import qualified Generate.Mode as Mode
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L

-- GENERATE
type Graph = Map.Map Opt.Global Opt.Node

type Mains = Map.Map ModuleName.Canonical Opt.Main

generate :: Mode.Mode -> Opt.GlobalGraph -> Mains -> B.Builder
generate mode (Opt.GlobalGraph graph _) mains = perfNote mode
  -- let
  --   state = Map.foldrWithKey (addMain mode graph) emptyState mains
  -- in
  -- "(function(scope){\n'use strict';"
  -- <> Functions.functions
  -- <> perfNote mode
  -- <> stateToBuilder state
  -- <> toMainExports mode mains
  -- <> "}(this));"

addMain ::
     Mode.Mode -> Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain mode graph home _ state =
  addGlobal mode graph state (Opt.Global home "main")

perfNote :: Mode.Mode -> B.Builder
perfNote mode =
  case mode of
    Mode.Prod _ -> ""
    Mode.Dev Nothing -> "serial.print('Compiled in DEV mode.')"
    Mode.Dev (Just _) -> "serial.print('Compiled in DEV mode.')"

-- GRAPH TRAVERSAL STATE
data State =
  State
    { _revKernels :: [B.Builder]
    , _revBuilders :: [B.Builder]
    , _seenGlobals :: Set.Set Opt.Global
    }

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

addStmt :: State -> Arduino.Stmt -> State
addStmt state stmt = addBuilder state (Arduino.pretty stmt)

addBuilder :: State -> B.Builder -> State
addBuilder (State revKernels revBuilders seen) builder =
  State revKernels (builder : revBuilders) seen

var :: Opt.Global -> Expr.Code -> Arduino.Stmt
var (Opt.Global home name) code =
  Arduino.Var
    "any"
    (CName.toBuilder (CName.fromGlobal home name))
    (Expr.codeToExpr code)
