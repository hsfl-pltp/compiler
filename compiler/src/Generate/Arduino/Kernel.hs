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

typedef struct ElmValue
{
  float value;
  bool b;
  Tag tag;

  ElmValue(float value) : value(value), tag(Tag_Float)//Constructor
  {
    Serial.print("Cons Float");
  }
  ElmValue(bool b) : b(b), tag(Tag_Bool)//Constructor
  {
    Serial.print("Cons Bool");
  }
  ~ElmValue()//Destructor
  {
    Serial.print("Des");
  }
} ElmValue;


arx::shared_ptr<ElmValue> _Basics_newElmBool(bool value) {
    arx::shared_ptr<ElmValue> p {new ElmValue(value)};
    return p;
}

arx::shared_ptr<ElmValue> _Basics_newElmFloat(float value) {
    arx::shared_ptr<ElmValue> p {new ElmValue(value)};
    return p;
}

float _Basics_ElmValueToFloat (arx::shared_ptr<ElmValue> pointer) {
    return pointer->value;
}

bool _Basics_ElmValueToBool (arx::shared_ptr<ElmValue> pointer) {
    return pointer->b;
}

static arx::shared_ptr<ElmValue> _Basics_add(arx::shared_ptr<ElmValue> n, arx::shared_ptr<ElmValue> m) {
    return _Basics_newElmFloat(n->value + m->value);
}

static arx::shared_ptr<ElmValue> _Basics_mul(arx::shared_ptr<ElmValue> n, arx::shared_ptr<ElmValue> m) {
    return _Basics_newElmFloat(n->value * m->value);
}

static arx::shared_ptr<ElmValue> _Basics_div(arx::shared_ptr<ElmValue> n, arx::shared_ptr<ElmValue> m) {
    return _Basics_newElmFloat(n->value / m->value);
}

static arx::shared_ptr<ElmValue> _Basics_sub(arx::shared_ptr<ElmValue> n, arx::shared_ptr<ElmValue> m) {
    return _Basics_newElmFloat(n->value - m->value);
}

static arx::shared_ptr<ElmValue> _Basics_modBy(arx::shared_ptr<ElmValue> modulus, arx::shared_ptr<ElmValue> x) {
    int ia = modulus->value;
    int ib = x->value;
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

    if(v->tag == Tag_Float){
        output = output + _Basics_ElmValueToFloat(v);
    } else if (v->tag == Tag_Bool) {
        if(v->value){
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
