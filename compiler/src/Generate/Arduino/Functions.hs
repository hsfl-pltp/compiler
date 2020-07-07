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


typedef struct {
  float value;
} Elmfloat;

Elmfloat* NewElmFloat(float value) {
  Elmfloat* p = sizeof(Elmfloat);
  p->value = value;
  return p;
};

float VoidToFloat (void* erg){
  return *((float *) erg);
}

static void* _Basics_add(void* n, void* m) {
  Elmfloat* pa = n;
  Elmfloat* pb = m;
  float ia = pa->value;
  float ib = pb->value;
  float i = ia + ib;
  return NewElmFloat(i);
}

|]
