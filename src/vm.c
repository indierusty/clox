#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "object.h"
#include "memory.h"
#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "table.h"
#include "value.h"
#include "vm.h"

VM vm;

static void reset_stack() {
    vm.stack_top = vm.stack;    
}

static void runtime_error(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputc('\n', stderr);

    size_t instruction_index = vm.ip - vm.chunk->code - 1;
    int line = vm.chunk->lines[instruction_index];
    fprintf(stderr, "[line %d] in script\n", line);
    reset_stack();
}

void init_vm() {
    reset_stack();   
    vm.objects = NULL;
    init_table(&vm.globals);
    init_table(&vm.strings);
}

void free_vm() {
    free_table(&vm.globals);
    free_table(&vm.strings);
    free_objects();
}

void push(Value value) {
    *vm.stack_top = value;
    vm.stack_top++;
}

Value pop() {
    vm.stack_top--;
    return *vm.stack_top;
}

static Value peek(int distance) {
    return vm.stack_top[-1 - distance];
}

static bool is_falsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !(AS_BOOL(value)));
}

static void concatenate() {
    ObjString* b = AS_STRING(pop());
    ObjString* a = AS_STRING(pop());

    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = take_string(chars, length);
    push(OBJ_VAL(result));
}

static inline uint8_t read_byte()  {
    // NOTE: as vm.ip points to bytecode in chunk, [read and shift a byte ahead]
    return *vm.ip++;
}

static inline Value read_constant() {
    return vm.chunk->constants.values[read_byte()];
}

static inline uint16_t read_short() {
    vm.ip += 2;
    return (uint16_t)((vm.ip[-2] << 8) | vm.ip[-1]);
}

static inline ObjString* read_string() {
    return AS_STRING(read_constant());
}

static inline void print_stack() {
    printf("          ");
    for (Value* slot = vm.stack; slot < vm.stack_top; slot++) {
        printf("[ ");
        print_value(*slot);
        printf(" ]");
    }
    printf("\n");
}

static InterpretResult run() {

#define BINARY_OP(value_type, op) \
    do { \
      if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
        runtime_error("Operands must be numbers."); \
        return INTERPRET_RUNTIME_ERROR; \
      } \
      double b = AS_NUMBER(pop()); \
      double a = AS_NUMBER(pop()); \
      push(value_type(a op b)); \
    } while (false)    

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION 
        print_stack();
        dissassemble_instruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif

        uint8_t instruction;
        switch (instruction = read_byte()) {
            case OP_CONSTANT: {
                Value constant = read_constant();
                push(constant);
                break;
            }
            case OP_NIL:     push(NIL_VAL); break;
            case OP_TRUE:    push(BOOL_VAL(true)); break;
            case OP_FALSE:   push(BOOL_VAL(false)); break;
            case OP_POP:     pop(); break;
            case OP_GET_LOCAL: {
                uint8_t slot = read_byte();
                push(vm.stack[slot]);
                break;
            }
            case OP_GET_GLOBAL: {
                // get the name of variable to read
                ObjString* name = read_string();
                // read the value of viriable to value;
                Value value;
                if (!table_get(&vm.globals, name, &value)) {
                    runtime_error("Undefined variable '%s'", name->chars);
                }
                // push value onto stack
                push(value);
                break;
            }
            case OP_DEFINE_GLOBAL: {
                ObjString* name = read_string();
                // get the value from stack and store key-value in vm.globals
                // NOTE: overwrites previous declaration of same global variable, hence we redefine globals
                table_set(&vm.globals, name, peek(0));
                pop();
                break;
            }
            case OP_SET_LOCAL: {
                uint8_t slot = read_byte();
                vm.stack[slot] = peek(0);
                break;
            }
            case OP_SET_GLOBAL: {
                // get the name of variable to write
                ObjString* name = read_string();
                // read value from stack 'peek(0)' and set value to it
                if (table_set(&vm.globals, name, peek(0))) {
                    // if key/var_name wasn't present then
                    table_delete(&vm.globals, name);
                    runtime_error("Undefined variable '%s'", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                // NOTE: we don't pop() the value on stack as assignment in lox is expr 
                break;
            }
            case OP_EQUAL:   {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(values_equal(a, b)));
                break;
            }
            case OP_GREATER:     BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:        BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD:         {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate();
                } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());
                    push(NUMBER_VAL(a + b));
                } else {
                    runtime_error("Operands must be two numbers or two strings.");
                  return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SUBTRACT:    BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY:    BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIVIDE:      BINARY_OP(NUMBER_VAL, /); break;
            case OP_NOT:     push(BOOL_VAL(is_falsey(pop()))); break;
            case OP_NEGATE: {
                if (!IS_NUMBER(peek(0))) {
                    runtime_error("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(NUMBER_VAL(-AS_NUMBER(pop()))); 
                break;
            }
            case OP_PRINT: {
                print_value(pop());
                printf("\n");
                break;
            }
            case OP_JUMP: {
                uint16_t offset = read_short();
                vm.ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                // read the 16 bit offset and NOTE: also set ip two bytes ahead
                uint16_t offset = read_short();
                // if false then move ip to end of if block
                if (is_falsey(peek(0))) vm.ip += offset;
                // else ip is already 2 byte ahead which will 
                // continue execution if block
                break;
            }
            case OP_LOOP: {
                uint16_t offset = read_short();
                vm.ip -= offset;
                break;
            }
            case OP_RETURN: {
                // exit interpreter
                return INTERPRET_OK;
            }
        }
    }
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    Chunk chunk;
    init_chunk(&chunk);

    if (!compile(source, &chunk)) {
        free_chunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;

    InterpretResult result = run();

    free_chunk(&chunk);
    return result;
}
 