#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "value.h"
#include "vm.h"

VM vm;

static void reset_stack() {
    vm.stack_top = vm.stack;    
}

void init_vm() {
    reset_stack();   
}

void free_vm() {
    
}

void push(Value value) {
    *vm.stack_top = value;
    vm.stack_top++;
}

Value pop() {
    vm.stack_top--;
    return *vm.stack_top;
}

static inline uint8_t read_byte()  {
    // NOTE: as vm.ip points to bytecode in chunk, [read and shift a byte ahead]
    return *vm.ip++;
}

static inline Value read_constant() {
    return vm.chunk->constants.values[read_byte()];
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
    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION 
        print_stack();
        dissassemble_instruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif
#define BINARY_OP(op) \
        do { \
            double b = pop(); \
            double a = pop(); \
            push(a op b); \
        } while (false)

        uint8_t instruction;
        switch (instruction = read_byte()) {
            case OP_CONSTANT: {
                Value constant = read_constant();
                push(constant);
                break;
            }
            case OP_ADD:         BINARY_OP(+); break;
            case OP_SUBTRACT:    BINARY_OP(-); break;
            case OP_MULTIPLY:    BINARY_OP(*); break;
            case OP_DIVIDE:      BINARY_OP(/); break;
            case OP_NEGATE: push(-pop()); break;
            case OP_RETURN: {
                return INTERPRET_OK;
            }
        }
    }
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    compile(source);
    return INTERPRET_OK;
}
 