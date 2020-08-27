{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Generate.Arduino.Kernel
  ( kernel,
  )
where

import qualified Data.ByteString.Builder as B
import Text.RawString.QQ (r)

kernel :: B.Builder
kernel =
  [r|
#include <ArxSmartPtr.h>


class ElmValue {
public:
  virtual String Debug() = 0;
  virtual ~ElmValue() {}
};


class ElmBool : public ElmValue {
private:
  bool bool_;

public:
  ElmBool(bool b) {
    bool_ = b;
  }

  bool GetBool() {
    return bool_;
  }

  String Debug() {
    return bool_ ? "True" : "False";
  }
};


class ElmFloat : public ElmValue {
private:
  float float_;

public:
  ElmFloat(float f) {
    float_ = f;
  }

  float GetFloat() {
    return float_;
  }

  String Debug() {
    return String(float_);
  }
};


class ElmConstr : public ElmValue {
private:
  const String &name_;
  arx::shared_ptr<ElmValue>* args_;
  int size_;

public:
  ElmConstr(const String &name, arx::shared_ptr<ElmValue>* args, int size) : name_(name) {
    args_ = args;
    size_ = size;
  }

  ~ElmConstr() {
    delete[] args_;
  }

  String Debug() {
    String res = "";
    for (int i = 0; i < size_; i++) {
      res += args_[i]->Debug();
    }
    return res;
  }
};


typedef struct {
  const String& key;
  arx::shared_ptr<ElmValue> value;
} Entry;


class ElmRecord : public ElmValue {
private:
  arx::shared_ptr<Entry>* args_;
  int size_;

public:
  ElmRecord(arx::shared_ptr<Entry>* args, int size) {
    args_ = args;
    size_ = size;
  }

  ~ElmRecord() {
    delete[] args_;
  }

  String Debug() {
    String res = "{";
    for (int i = 0; i < size_ - 1; i++) {
      arx::shared_ptr<Entry> entry = args_[i];
      res += String(entry->key);
      res += " = ";
      res += entry->value->Debug();
      res += ", ";
    }
    arx::shared_ptr<Entry> last_entry = args_[size_ - 1];
    res += String(last_entry->key);
    res += " = ";
    res += last_entry->value->Debug();
    res += "}";
    return res;
  }

  arx::shared_ptr<ElmValue> GetField(String field) {
    for (int i = 0; i < size_; i++) {
      arx::shared_ptr<Entry> entry = args_[i];
      if (entry->key == field) {
        return entry->value;
      }
    }
  }
};


static arx::shared_ptr<ElmValue> Bool(bool b) {
  return arx::shared_ptr<ElmValue>(new ElmBool(b));
}

static arx::shared_ptr<ElmValue> Float(float f) {
  return arx::shared_ptr<ElmValue>(new ElmFloat(f));
}

static arx::shared_ptr<ElmValue> Constr(const String& name, arx::shared_ptr<ElmValue>* args, int size) {
  return arx::shared_ptr<ElmValue>(new ElmConstr(name, args, size));
}

static arx::shared_ptr<ElmValue> Record(arx::shared_ptr<Entry>* args, int size) {
  return arx::shared_ptr<ElmValue>(new ElmRecord(args, size));
}


static bool GetBool(arx::shared_ptr<ElmValue> ev) {
  arx::shared_ptr<ElmBool> eb = arx::static_pointer_cast<ElmBool>(ev);
  return eb->GetBool();
}

static float GetFloat(arx::shared_ptr<ElmValue> ev) {
  arx::shared_ptr<ElmFloat> ef = arx::static_pointer_cast<ElmFloat>(ev);
  return ef->GetFloat();
}

static arx::shared_ptr<ElmValue> GetField(arx::shared_ptr<ElmValue> ev, String field) {
  arx::shared_ptr<ElmRecord> ef = arx::static_pointer_cast<ElmRecord>(ev);
  return ef->GetField(field);
}


static arx::shared_ptr<ElmValue> _Basics_add(arx::shared_ptr<ElmValue> ev1, arx::shared_ptr<ElmValue> ev2) {
  return Float(GetFloat(ev1) + GetFloat(ev2));
}

static arx::shared_ptr<ElmValue> _Basics_mul(arx::shared_ptr<ElmValue> ev1, arx::shared_ptr<ElmValue> ev2) {
  return Float(GetFloat(ev1) * GetFloat(ev2));
}

static arx::shared_ptr<ElmValue> _Basics_div(arx::shared_ptr<ElmValue> ev1, arx::shared_ptr<ElmValue> ev2) {
  return Float(GetFloat(ev1) / GetFloat(ev2));
}

static arx::shared_ptr<ElmValue> _Basics_sub(arx::shared_ptr<ElmValue> ev1, arx::shared_ptr<ElmValue> ev2) {
  return Float(GetFloat(ev1) - GetFloat(ev2));
}


static void digitalPin(int pin, bool value) {
  digitalWrite(pin, value ? HIGH : LOW);
}


static arx::shared_ptr<ElmValue> _Basics_modBy(arx::shared_ptr<ElmValue> ev1, arx::shared_ptr<ElmValue> ev2) {
  int i1 = GetFloat(ev1);
  int i2 = GetFloat(ev2);
  if (i2 == 0) {
    exit(EXIT_FAILURE);
  } else {
    int res = i1 % i2;
    if(res > 0 && i1 < 0 || res < 0 && i2 > 0) {
      res = i1 + res;
      return Float(res);
    } else {
      return Float(res);
    }
  }
}

static arx::shared_ptr<ElmValue> _Debug_log__Prod(const String& str, arx::shared_ptr<ElmValue> ev) {
  return ev;
}

static arx::shared_ptr<ElmValue> _Debug_log(const String& str, arx::shared_ptr<ElmValue> ev) {
  Serial.begin(9600);
  Serial.println(str);
  Serial.println(ev->Debug());
  return ev;
}

|]
