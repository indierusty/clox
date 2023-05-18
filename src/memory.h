#pragma once

#include "common.h"

#define ALLOCATE(type, count) \
    (type*)reallocate(NULL, 0, sizeof(type) * (count))

#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity) * 2)

// NOTE: returns a void pointer, must be cast to original type 
#define GROW_ARRAY(type, pointer, old_size, new_size) \
    (type*)reallocate(pointer, (sizeof(type)) * (old_size), (sizeof(type)) * (new_size))

#define FREE_ARRAY(type, pointer, old_size) \
    (type*)reallocate(pointer, (sizeof(type)) * (old_size), 0)

void* reallocate(void* pointer, size_t old_size, size_t new_size);
void free_objects();
    
