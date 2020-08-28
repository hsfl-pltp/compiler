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
  virtual ~ElmValue() {}
  virtual String Debug() = 0;
  virtual bool Equal(arx::shared_ptr<ElmValue> ev) = 0;
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

  bool Equal(arx::shared_ptr<ElmValue> ev) {
    arx::shared_ptr<ElmBool> eb = arx::static_pointer_cast<ElmBool>(ev);
    return bool_ == eb->bool_;
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

  bool Equal(arx::shared_ptr<ElmValue> ev) {
    arx::shared_ptr<ElmFloat> ef = arx::static_pointer_cast<ElmFloat>(ev);
    return float_ == ef->float_;
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

  bool Equal(arx::shared_ptr<ElmValue> ev) {
    arx::shared_ptr<ElmConstr> ec = arx::static_pointer_cast<ElmConstr>(ev);
    if (name_ == ec->name_) {
      for (int i = 0; i < size_; i++) {
        if (!args_[i]->Equal(ec->args_[i])) {
          return false;
        }
      }
      return true;
    } else {
      return false;
    }
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

  bool Equal(arx::shared_ptr<ElmValue> ev) {
    arx::shared_ptr<ElmRecord> ec = arx::static_pointer_cast<ElmRecord>(ev);
    for (int i = 0; i < size_; i++) {
      if (!args_[i]->value->Equal(ec->args_[i]->value)) {
        return false;
      }
      return true;
    }
  }

  arx::shared_ptr<ElmValue> GetField(String field) {
    for (int i = 0; i < size_; i++) {
      arx::shared_ptr<Entry> entry = args_[i];
      if (entry->key == field) {
        return entry->value;
      }
    }
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
};


class Utils {
public:

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


  static arx::shared_ptr<ElmValue> Equal(arx::shared_ptr<ElmValue> ev1, arx::shared_ptr<ElmValue> ev2) {
    return Bool(ev1->Equal(ev2));
  }
};


class Basics {
public:

  static arx::shared_ptr<ElmValue> Add(arx::shared_ptr<ElmValue> ev1, arx::shared_ptr<ElmValue> ev2) {
    return Utils::Float(Utils::GetFloat(ev1) + Utils::GetFloat(ev2));
  }

  static arx::shared_ptr<ElmValue> Mul(arx::shared_ptr<ElmValue> ev1, arx::shared_ptr<ElmValue> ev2) {
    return Utils::Float(Utils::GetFloat(ev1) * Utils::GetFloat(ev2));
  }

  static arx::shared_ptr<ElmValue> Div(arx::shared_ptr<ElmValue> ev1, arx::shared_ptr<ElmValue> ev2) {
    return Utils::Float(Utils::GetFloat(ev1) / Utils::GetFloat(ev2));
  }

  static arx::shared_ptr<ElmValue> Sub(arx::shared_ptr<ElmValue> ev1, arx::shared_ptr<ElmValue> ev2) {
    return Utils::Float(Utils::GetFloat(ev1) - Utils::GetFloat(ev2));
  }


  static arx::shared_ptr<ElmValue> ModBy(arx::shared_ptr<ElmValue> ev1, arx::shared_ptr<ElmValue> ev2) {
    int i1 = Utils::GetFloat(ev1);
    int i2 = Utils::GetFloat(ev2);
    if (i2 == 0) {
      exit(EXIT_FAILURE);
    } else {
      int res = i1 % i2;
      if(res > 0 && i1 < 0 || res < 0 && i2 > 0) {
        res = i1 + res;
        return Utils::Float(res);
      } else {
        return Utils::Float(res);
      }
    }
  }
};


class Debug {
public:

  static arx::shared_ptr<ElmValue> Log__Prod(const String& str, arx::shared_ptr<ElmValue> ev) {
    return ev;
  }

  static arx::shared_ptr<ElmValue> Log(const String& str, arx::shared_ptr<ElmValue> ev) {
    Serial.begin(9600);
    Serial.println(str);
    Serial.println(ev->Debug());
    return ev;
  }
};


class Output {
public:

  static void DigitalPin(int pin, bool value) {
    digitalWrite(pin, value ? HIGH : LOW);
  }
};

|]
