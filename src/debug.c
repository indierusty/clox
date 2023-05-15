#include <stdint.h>
#include <stdio.h>

#include "chunk.h"
#include "debug.h"
#include "value.h"

void disassemble_chunk(Chunk *chunk, const char *name) {
    printf("== %s ==\n", name);

    int offset = 0;
    while (offset < chunk->count) {
        offset = dissassemble_instruction(chunk, offset);
    }
}

static int constant_instruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant_index = chunk->code[offset + 1];
    printf("%-16s %4d '", name, constant_index);
    print_value(chunk->constants.values[constant_index]);
    printf("'\n");

    // 2 means this opcode take one oprand together of len 2
    return offset + 2;
}

static int simple_instruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

int dissassemble_instruction(Chunk *chunk, int offset) {
    printf("%04d ", offset);

    /// print line number [if same as above just print ' |' else print number]
    if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
        printf("   | ");
    } else {
        printf("%4d ", chunk->lines[offset]);
    }

    uint8_t instruction = chunk->code[offset];

    switch (instruction) {
        case OP_CONSTANT: return constant_instruction("OP_CONSTANT", chunk, offset);
        case OP_ADD: return simple_instruction("OP_ADD", offset);
        case OP_SUBTRACT: return simple_instruction("OP_SUBTRACT", offset);
        case OP_MULTIPLY: return simple_instruction("OP_MULTIPLY", offset);
        case OP_DIVIDE: return simple_instruction("OP_DIVIDE", offset);
        case OP_NEGATE: return simple_instruction("OP_NEGATE", offset);
        case OP_RETURN: return simple_instruction("OP_RETURN", offset);
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}
