#pragma once

#include "chunk.h"
#include "table.h"
#include "value.h"

#define STACK_MAX 256

typedef struct {
  Chunk* chunk;
  /// ip [instruction pointer ] points to the next instruction to be executed not the current
  uint8_t* ip; 
  Value stack[STACK_MAX];
  /// points just past the top value on stack
  Value* stack_top; 
  // hashset of every string helpful for == operator, STRING INTERNING  
  // any string in this map guarenteed to be textually distinct from all strings.
  Table strings; 
  // head of linked list of every object vm ever creats
  Obj* objects; 
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR,
} InterpretResult;

extern VM vm;

void init_vm();
void free_vm();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

