#pragma once

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value)  (AS_OBJ(value)->type)

#define IS_STRING(value) is_obj_type(value, OBJ_STRING)

#define AS_STRING(value)  ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
  OBJ_STRING,
} ObjType;

struct Obj {
  ObjType type;
  struct Obj* next;
};

/// NOTE:
/// ObjString can be cast to Obj and Obj can be cast to Objstring
/// as the first byte of ObjString is Obj it self,
/// this is called [Type Punning / Struct Inheritance]. 
/// hence every objstring is an obj
struct ObjString {
  Obj obj;
  int length;
  char* chars;
  uint32_t hash;
};

ObjString* take_string(char* chars, int length);
ObjString* copy_string(const char* chars, int length);
void print_object(Value value);

static inline bool is_obj_type(Value value, ObjType type) {
  return IS_OBJECT(value) && AS_OBJ(value)->type == type;
}
