#pragma once

#include "common.h"

#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity) * 2)

// NOTE: returns a void pointer, must be cast to original type 
#define GROW_ARRAY(type_size, pointer, old_size, new_size) \
    reallocate(pointer, type_size * (old_size), type_size * (new_size))

#define FREE_ARRAY(type_size, pointer, old_size) \
    reallocate(pointer, type_size * (old_size), 0)

void* reallocate(void* pointer, size_t old_size, size_t new_size);
    
