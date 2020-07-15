{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Generate.Arduino.Kernel
  ( kernel
  ) where

import qualified Data.ByteString.Builder as B
import Text.RawString.QQ (r)

kernel :: B.Builder
kernel =
  [r|
typedef enum {
    Tag_Float,            // 0
} Tag;

typedef struct {
    float value;
    Tag tag;
} ElmFloat;

typedef union {
    ElmFloat elm_float;
} ElmValue;


ElmFloat* _Basics_newElmFloat(float value) {
    ElmFloat* p = (ElmFloat*)malloc(sizeof(ElmFloat));
    p->value = value;
    p->tag = Tag_Float;
    return p;
}

float _Basics_voidToFloat (void* pointer){
    return *((float *) pointer);
}

static void* _Basics_add(ElmFloat* n, ElmFloat* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia + ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_mul(ElmFloat* n, ElmFloat* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia * ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_div(ElmFloat* n, ElmFloat* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia / ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_sub(ElmFloat* n, ElmFloat* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia - ib;
    return _Basics_newElmFloat(i);
}

static void* _Debug_log__Prod(String n, void* m) {
    return m;
}

static void* _Debug_log(String n, void* m) {
    ElmValue* v = m;
    String output = n + " ";

    if(v->elm_float.tag == Tag_Float){
        output = output + _Basics_voidToFloat(m);
    } else {
        output = "No valid type";
    }

    Serial.begin(9600);
    Serial.println(output);
    return m;
}
|]
