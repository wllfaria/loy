#ifndef _ARENA_H
#define _ARENA_H

#include "../defines.h"
#include "allocator.h"

typedef struct Region {
    void*          start;
    u64            cursor;
    u64            size;
    struct Region* next;
} Region;

typedef struct {
    Region* regions;
} Arena;

Allocator arena_create(void);
void arena_destroy(Arena* arena);
void arena_reset(Arena* arena);

void* __arena_alloc(Arena* arena, u64 size, u64 align);

#define arena_alloc_many(arena, type, count) \
    ((type*)__arena_alloc((arena), sizeof(type) * (count), alignof(type)))

#define arena_alloc(arena, type) arena_alloc_many(arena, type, 1)

#endif
