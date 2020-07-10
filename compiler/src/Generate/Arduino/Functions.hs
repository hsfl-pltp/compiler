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
} ElmFloat;

ElmFloat* _Basics_newElmFloat(float value) {
    ElmFloat* p = malloc(sizeof(ElmFloat));
    p->value = value;
    return p;
};

float _Basics_voidToFloat (void* pointer){
    return *((float *) pointer);
}

static void* _Basics_add(void* n, void* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia + ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_mul(void* n, void* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia * ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_div(void* n, void* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia / ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_sub(void* n, void* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia - ib;
    return _Basics_newElmFloat(i);
}

static void* _Debug_log(String n, void* m) {
    Serial.begin(9600);
    Serial.print(n);
    Serial.print(" ");
    Serial.println(_Basics_voidToFloat(m));
    return m;    
} 

|]
