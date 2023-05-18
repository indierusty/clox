#include <stdlib.h>

#include "object.h"
#include "memory.h"
#include "vm.h"

/*
oldSize	    newSize	                Operation

0	        Non‑zero	            Allocate new block.
Non‑zero	0	                    Free allocation.
Non‑zero	Smaller than oldSize	Shrink existing allocation.
Non‑zero	Larger than oldSize	    Grow existing allocation.
*/

void* reallocate(void* pointer, size_t old_size, size_t new_size) {
    if (new_size == 0) {
        free(pointer);
        return NULL;
    }    

    void* result = realloc(pointer, new_size);
    if (result == NULL) exit(1);
    return result;
}

static void free_object(Obj* object) {
    switch (object->type) {
        case OBJ_STRING: {
            ObjString* string = (ObjString*)object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
    }
}

void free_objects() {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        free_object(object);
        object = next;
    }
}
