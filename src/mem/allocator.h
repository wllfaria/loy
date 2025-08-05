#ifndef _ALLOCATOR_H
#define _ALLOCATOR_H

#include "../defines.h"

typedef void* (*FreeFn)(void* ctx, void* ptr);
typedef void* (*AllocFn)(void* ctx, u64 size);
typedef void* (*ReallocFn)(void* ctx, void* ptr, u64 new_size);

typedef struct {
    void*     ctx;
    FreeFn    free;
    AllocFn   alloc;
    ReallocFn realloc;
} Allocator;

typedef union {
    long long   ll;
    long double ld;
    void*       p;
} max_align_t;

#endif
