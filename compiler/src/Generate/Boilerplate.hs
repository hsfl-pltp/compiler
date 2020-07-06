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


sandwichArduino :: B.Builder -> B.Builder -> B.Builder
sandwichArduino ccode mode =
  [r|
#include <stdlib.h> 
void setup() {
Serial.begin(9600);
|] <> mode <> [r|
|] <> ccode <> [r|
//The Code here will only be executed once 
}

void loop() {
  // put your main code here, to run repeatedly:

}|]