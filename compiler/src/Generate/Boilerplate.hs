{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Boilerplate
  ( sandwichArduino
  )
  where


import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import Text.RawString.QQ (r)



-- SANDWICH


sandwichArduino :: B.Builder -> B.Builder
sandwichArduino ccode =
  [r|

void setup() {
|] <> ccode <> [r|
//The Code here will only be executed once 
}

void loop() {
  // put your main code here, to run repeatedly:

}|]