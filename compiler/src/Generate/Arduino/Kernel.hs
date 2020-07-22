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


ElmBool* _Basics_newElmBool(bool value) {
    ElmBool* p = (ElmBool*)malloc(sizeof(ElmFloat));
    p->value = value;
    p->tag = Tag_Bool;
    return p;
}

ElmFloat* _Basics_newElmFloat(float value) {
    ElmFloat* p = (ElmFloat*)malloc(sizeof(ElmFloat));
    p->value = value;
    p->tag = Tag_Float;
    return p;
}

float _Basics_voidToFloat (void* pointer) {
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
static void* _Utils_equal(void* x, void* y) {
    return reinterpret_cast<void *> (static_cast<int> (x==y)); 
}


static void* _Debug_log__Prod(String n, void* m) {
    return m;
}

static void* _Debug_log(String n, void* m) {
    ElmValue* v = (ElmValue*)m;
    String output = n + " ";

    if(v->elm_float.tag == Tag_Float){
        output = output + _Basics_voidToFloat(m);
    } else if (v->elm_bool.tag == Tag_Bool) {
        if(v->elm_bool.value){
            output = output + "True";
        } else {
            output = output + "false";
        }
    } else {
        output = "No valid type";
    }

    Serial.begin(9600);
    Serial.println(output);
    return m;
}

|]
