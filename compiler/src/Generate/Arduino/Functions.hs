{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Generate.Arduino.Functions
  ( functions
  ) where

import qualified Data.ByteString.Builder as B
import Text.RawString.QQ (r)

-- FUNCTIONS
functions :: B.Builder
functions =
  [r|
  #define A1(f, a) Utils_apply(f, 1, (void* []){a})
  #define A2(f, a, b) Utils_apply(f, 2, (void* []){a, b})


void* Utils_apply(void* fun, int n_applied, void* applied[]) {
  void** args;
  
}

|]
