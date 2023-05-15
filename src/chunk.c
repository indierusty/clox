#include "chunk.h"
#include "memory.h"
#include "value.h"

void init_chunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    init_value_array(&chunk->constants);
}

void free_chunk(Chunk* chunk) {
    FREE_ARRAY(sizeof(uint8_t), chunk->code, chunk->capacity);
    FREE_ARRAY(sizeof(int), chunk->lines, chunk->capacity);
    free_value_array(&chunk->constants);
    init_chunk(chunk);
}

void write_chunk(Chunk *chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int old_capacity = chunk->capacity; 
        chunk->capacity = GROW_CAPACITY(old_capacity);
        chunk->code = (uint8_t*)GROW_ARRAY(
            sizeof(uint8_t), chunk->code, old_capacity, chunk->capacity);
        chunk->lines= (int*)GROW_ARRAY(
            sizeof(int), chunk->lines, old_capacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count += 1;
}

/// add & return index of value added in constant array
int add_constant(Chunk* chunk, Value value) {
    write_value_array(&chunk->constants, value);
    return chunk->constants.count - 1;
}
