{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Arduino.Name
  ( Name(..)
    ,fromGlobal
  )
  where

import Data.Monoid ((<>))
import qualified Data.ByteString.Builder as B
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import Data.Word (Word8)

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
-- NAME


newtype Name =
  Name { toBuilder :: B.Builder }


fromGlobal :: ModuleName.Canonical -> Name.Name -> Name
fromGlobal home name =
  Name $ homeToBuilder home <> usd <> Name.toBuilder name

{-# INLINE homeToBuilder #-}
homeToBuilder :: ModuleName.Canonical -> B.Builder
homeToBuilder (ModuleName.Canonical (Pkg.Name author project) home) =
  usd <>
  Utf8.toEscapedBuilder 0x2D {- - -} 0x5F {- _ -} author
  <> usd <>
  Utf8.toEscapedBuilder 0x2D {- - -} 0x5F {- _ -} project
  <> usd <>
  Utf8.toEscapedBuilder 0x2E {- . -} 0x24 {- $ -} home

-- TEMPORARY NAMES

usd :: B.Builder
usd =
  Name.toBuilder Name.dollar
