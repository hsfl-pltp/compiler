{-# LANGUAGE OverloadedStrings #-}
module Generate.C
  ( generate
  )
  where

import qualified Data.ByteString.Builder         as B
import qualified Data.List                       as List
import           Data.Map                        ((!))
import qualified Data.Map                        as Map
import           Data.Monoid                     ((<>))
import qualified Data.Name                       as Name
import qualified Data.Set                        as Set
import qualified Data.Utf8                       as Utf8
import           Prelude                         hiding (cycle, print)

import qualified AST.Canonical                   as Can
import qualified AST.Optimized                   as Opt
import qualified Data.Index                      as Index
import qualified Elm.Kernel                      as K
import qualified Elm.ModuleName                  as ModuleName
import qualified Generate.C.Builder              as JS
import qualified Generate.C.Expression           as Expr
import qualified Generate.Mode                   as Mode
import qualified Reporting.Doc                   as D
import qualified Reporting.Render.Type           as RT
import qualified Reporting.Render.Type.Localizer as L


-- GENERATE


type Graph = Map.Map Opt.Global Opt.Node
type Mains = Map.Map ModuleName.Canonical Opt.Main


generate :: Mode.Mode -> Opt.GlobalGraph -> Mains -> B.Builder
generate mode (Opt.GlobalGraph graph _) mains = undefined
  -- let
  --   state = Map.foldrWithKey (addMain mode graph) emptyState mains
  -- in
  -- "(function(scope){\n'use strict';"
  -- <> Functions.functions
  -- <> perfNote mode
  -- <> stateToBuilder state
  -- <> toMainExports mode mains
  -- <> "}(this));"

perfNote :: Mode.Mode -> B.Builder
perfNote mode =
  case mode of
    Mode.Prod _ ->
      ""
    Mode.Dev Nothing ->
      "serial.print('Compiled in DEV mode. Follow the advice at "
      <> B.stringUtf8 (D.makeNakedLink "optimize")
      <> " for better performance and smaller assets.')"
    Mode.Dev (Just _) ->
      "serial.print('Compiled in DEV mode. Follow the advice at "
      <> B.stringUtf8 (D.makeNakedLink "optimize")
      <> " for better performance and smaller assets.')'"
