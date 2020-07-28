{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Generate.Arduino.Functions
  ( functions
  ) where

import qualified Data.ByteString.Builder as B
import Text.RawString.QQ (r)

-- FUNCTIONS
functions :: B.Builder
functions = [r| |]
