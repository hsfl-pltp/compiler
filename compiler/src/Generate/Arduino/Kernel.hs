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

static void* _Basics_add(void* n, void* m) {
    ElmFloat* pa = (ElmFloat*)n;
    ElmFloat* pb = (ElmFloat*)m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia + ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_mul(void* n, void* m) {
    ElmFloat* pa = (ElmFloat*)n;
    ElmFloat* pb = (ElmFloat*)m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia * ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_div(void* n, void* m) {
    ElmFloat* pa = (ElmFloat*)n;
    ElmFloat* pb = (ElmFloat*)m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia / ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_sub(void* n, void* m) {
    ElmFloat* pa = (ElmFloat*)n;
    ElmFloat* pb = (ElmFloat*)m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia - ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_modBy(void* modulus, void* x) {
    ElmFloat* pa = (ElmFloat*)modulus;
    ElmFloat* pb = (ElmFloat*)x;
    int ia = pa->value;
    int ib = pb->value;
    int i = ib % ia;
    if(ia == 0){
      exit(EXIT_FAILURE);
    }
    else {
      if(i > 0 && ia < 0 || i < 0 && ia > 0){
        i = ia + i;
        return _Basics_newElmFloat(i);
      }
      else {
        return _Basics_newElmFloat(i);
      }
    }
    return _Basics_newElmFloat(i);
}

static void* _Debug_log__Prod(String n, void* m) {
    return m;
}

static void* _Debug_log(String n, void* m) {
    ElmValue* v = (ElmValue*)m;
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
