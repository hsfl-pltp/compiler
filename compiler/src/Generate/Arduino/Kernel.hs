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
    Tag_Bool,             // 1
} Tag;

typedef struct {
    float value;
    Tag tag;
} ElmFloat;

typedef struct {
    bool value;
    Tag tag;
} ElmBool;

typedef union {
    ElmFloat elm_float;
    ElmBool elm_bool;
} ElmValue;


ElmValue* _Basics_newElmBool(bool value) {
    ElmBool* p = (ElmBool*)malloc(sizeof(ElmBool));
    p->value = value;
    p->tag = Tag_Bool;
    return (ElmValue*)p;
}

ElmValue* _Basics_newElmFloat(float value) {
    ElmFloat* p = (ElmFloat*)malloc(sizeof(ElmFloat));
    p->value = value;
    p->tag = Tag_Float;
    return (ElmValue*)p;
}

float _Basics_ElmValueToFloat (ElmValue* pointer) {
    return *((float *) pointer);
}

bool _Basics_ElmValueToBool (ElmValue* pointer) {
    return *((bool *) pointer);
}

static ElmValue* _Basics_add(ElmValue* n, ElmValue* m) {
    ElmFloat* pa = (ElmFloat*)n;
    ElmFloat* pb = (ElmFloat*)m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia + ib;
    return _Basics_newElmFloat(i);
}

static ElmValue* _Basics_mul(ElmValue* n, ElmValue* m) {
    ElmFloat* pa = (ElmFloat*)n;
    ElmFloat* pb = (ElmFloat*)m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia * ib;
    return _Basics_newElmFloat(i);
}

static ElmValue* _Basics_div(ElmValue* n, ElmValue* m) {
    ElmFloat* pa = (ElmFloat*)n;
    ElmFloat* pb = (ElmFloat*)m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia / ib;
    return _Basics_newElmFloat(i);
}

static ElmValue* _Basics_sub(ElmValue* n, ElmValue* m) {
    ElmFloat* pa = (ElmFloat*)n;
    ElmFloat* pb = (ElmFloat*)m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia - ib;
    return _Basics_newElmFloat(i);
}

static ElmValue* _Basics_modBy(ElmValue* modulus, ElmValue* x) {
    ElmFloat* pa = (ElmFloat*)modulus;
    ElmFloat* pb = (ElmFloat*)x;
    int ia = pa->value;
    int ib = pb->value;
    int i = ib % ia;
    if(ia == 0) {
        exit(EXIT_FAILURE);
    } else {
        if(i > 0 && ia < 0 || i < 0 && ia > 0) {
            i = ia + i;
            return _Basics_newElmFloat(i);
        } else {
            return _Basics_newElmFloat(i);
        } 
    }
    return _Basics_newElmFloat(i);
}

// EQUALITY
static ElmValue* _Utils_equal(ElmValue* x, ElmValue* y) {
    return reinterpret_cast<ElmValue *> (static_cast<int> (x==y)); 
}


static ElmValue* _Debug_log__Prod(String n, ElmValue* v) {
    return v;
}

static ElmValue* _Debug_log(String n, ElmValue* v) {
    String output = n + " ";

    if(v->elm_float.tag == Tag_Float){
        output = output + _Basics_ElmValueToFloat(v);
    } else if (v->elm_bool.tag == Tag_Bool) {
        if(v->elm_bool.value){
            output = output + "True";
        } else {
            output = output + "False";
        }
    } else {
        output = "No valid type";
    }

    Serial.begin(9600);
    Serial.println(output);
    return v;
}

|]
