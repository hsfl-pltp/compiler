{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.Arduino.Name
  ( Name(..)
  , fromGlobal
  , fromLocal
  ) where

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import Data.Word (Word8)

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

-- NAME
newtype Name =
  Name
    { toBuilder :: B.Builder
    }

fromGlobal :: ModuleName.Canonical -> Name.Name -> Name
fromGlobal home name = Name $ homeToBuilder home <> usd <> Name.toBuilder name

{-# INLINE homeToBuilder #-}
homeToBuilder :: ModuleName.Canonical -> B.Builder
homeToBuilder (ModuleName.Canonical (Pkg.Name author project) home) =
  usd <>
  Utf8.toEscapedBuilder 0x2D 0x5F author {- - -}
   {- _ -}
   <>
  usd <>
  Utf8.toEscapedBuilder 0x2D 0x5F project {- - -}
   {- _ -}
   <>
  usd <> Utf8.toEscapedBuilder 0x2E 0x24 home
   {- $ -}

-- TEMPORARY NAMES
usd :: B.Builder
usd = Name.toBuilder Name.dollar

fromLocal :: Name.Name -> Name
fromLocal name =
  if Set.member name reservedNames
    then Name ("_" <> Name.toBuilder name)
    else Name (Name.toBuilder name)

-- RESERVED NAMES
{-# NOINLINE reservedNames #-}
reservedNames :: Set.Set Name.Name
reservedNames = Set.union cReservedWords elmReservedWords

cReservedWords :: Set.Set Name.Name
cReservedWords =
  Set.fromList
    [ "auto"
    , "break"
    , "case"
    , "char"
    , "const"
    , "continue"
    , "default"
    , "do"
    , "int"
    , "long"
    , "register"
    , "return"
    , "short"
    , "signed"
    , "sizeof"
    , "static"
    , "struct"
    , "switch"
    , "typedef"
    , "union"
    , "unsigned"
    , "void"
    , "volatile"
    , "while"
    , "double"
    , "else"
    , "enum"
    , "extern"
    , "float"
    , "for"
    , "goto"
    , "if"
    , "and"
    , "and_eq"
    , "alignas"
    , "alignof"
    , "asm"
    , "auto"
    , "bitand"
    , "bitor"
    , "bool"
    , "break"
    , "catch"
    , "char16_t"
    , "char32_t"
    , "class"
    , "compl"
    , "constexpr"
    , "const_cast"
    , "decltype"
    , "double"
    , "delete"
    , "dynamic_cast"
    , "explicit"
    , "export"
    , "false"
    , "friend"
    , "inline"
    , "mutable"
    , "namespace"
    , "new"
    , "noexcept"
    , "not"
    , "not_eq"
    , "nullptr"
    , "operator"
    , "or"
    , "or_eq"
    , "private"
    , "protected"
    , "public"
    , "reinterpret_cast"
    , "sizeof"
    , "static_assert"
    , "static_cast"
    , "template"
    , "this"
    , "thread_local"
    , "throw"
    , "true"
    , "try"
    , "typedef"
    , "typeid"
    , "typename"
    , "using"
    , "virtual"
    , "wchar_t"
    , "xor"
    , "xor_eq"
    ]

elmReservedWords :: Set.Set Name.Name
elmReservedWords =
  Set.fromList
    [ "F2"
    , "F3"
    , "F4"
    , "F5"
    , "F6"
    , "F7"
    , "F8"
    , "F9"
    , "A2"
    , "A3"
    , "A4"
    , "A5"
    , "A6"
    , "A7"
    , "A8"
    , "A9"
    ]
