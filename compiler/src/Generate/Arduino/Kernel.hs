{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Generate.Arduino.Kernel
  ( kernel
  ) where

import qualified Data.ByteString.Builder as B
import Text.RawString.QQ (r)

kernel :: B.Builder
kernel =
  [r|
#include <ArxSmartPtr.h>

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

arx::shared_ptr<ElmValue> _Basics_newElmBool(bool value) {
    ElmBool p;
    p.value = value;
    p.tag = Tag_Bool;
    arx::shared_ptr<ElmValue> ev {new ElmValue};
    ev->elm_bool.value = value;
    ev->elm_bool.tag = Tag_Bool;
    return ev;
}

arx::shared_ptr<ElmValue> _Basics_newElmFloat(float value) {
    ElmFloat p;
    p.value = value;
    p.tag = Tag_Float;
    arx::shared_ptr<ElmValue> ev {new ElmValue};
    ev->elm_float.value = value;
    ev->elm_float.tag = Tag_Float;
    return ev;
}

float _Basics_ElmValueToFloat (arx::shared_ptr<ElmValue> pointer) {
    return pointer->elm_float.value;
}

bool _Basics_ElmValueToBool (arx::shared_ptr<ElmValue> pointer) {
    return pointer->elm_bool.value;
}

static arx::shared_ptr<ElmValue> _Basics_add(arx::shared_ptr<ElmValue> n, arx::shared_ptr<ElmValue> m) {
    ElmFloat a = n -> elm_float;
    ElmFloat b = m -> elm_float;
    return _Basics_newElmFloat(a.value + b.value);
}

static arx::shared_ptr<ElmValue> _Basics_mul(arx::shared_ptr<ElmValue> n, arx::shared_ptr<ElmValue> m) {
    ElmFloat a = n -> elm_float;
    ElmFloat b = m -> elm_float;
    return _Basics_newElmFloat(a.value * b.value);
}

static arx::shared_ptr<ElmValue> _Basics_div(arx::shared_ptr<ElmValue> n, arx::shared_ptr<ElmValue> m) {
    ElmFloat a = n -> elm_float;
    ElmFloat b = m -> elm_float;
    return _Basics_newElmFloat(a.value / b.value);
}

static arx::shared_ptr<ElmValue> _Basics_sub(arx::shared_ptr<ElmValue> n, arx::shared_ptr<ElmValue> m) {
    ElmFloat a = n -> elm_float;
    ElmFloat b = m -> elm_float;
    return _Basics_newElmFloat(a.value - b.value);
}

static arx::shared_ptr<ElmValue> _Basics_modBy(arx::shared_ptr<ElmValue> modulus, arx::shared_ptr<ElmValue> x) {
    ElmFloat pa = modulus -> elm_float;
    ElmFloat pb = x -> elm_float;
    int ia = pa.value;
    int ib = pb.value;
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


static arx::shared_ptr<ElmValue> _Debug_log__Prod(String n, arx::shared_ptr<ElmValue> v) {
    return v;
}

static arx::shared_ptr<ElmValue> _Debug_log(String n, arx::shared_ptr<ElmValue> v) {
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
