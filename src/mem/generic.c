#include <stdlib.h>

#include "../defines.h"
#include "generic.h"

void* generic_alloc(void* ctx, size_t size) {
    (void)ctx;
    return malloc(size);
}

void* generic_realloc(void* ctx, void* ptr, size_t new_size) {
    (void)ctx;
    return realloc(ptr, new_size);
}

void* generic_free(void* ctx, void* ptr) {
    (void)ctx;
    free(ptr);
    return NULL;
}

Allocator generic_allocator_create(void) {
    Allocator allocator = {
        .ctx = NULL,
        .free = generic_free,
        .alloc = generic_alloc,
        .realloc = generic_realloc,
    };
    return allocator;
}
