#include "arena.h"
#include "../defines.h"

#define BYTE        1
#define KILOBYTE    (BYTE * 1024)
#define MEGABYTE    (KILOBYTE * 1024)
#define REGION_SIZE (MEGABYTE * 1)

static void* arena_alloc_fn(void* ctx, u64 size) {
    return __arena_alloc((Arena*)ctx, size, alignof(max_align_t));
}

static void* arena_realloc_fn(void* ctx, void* ptr, u64 new_size) {
    (void)ptr;
    // Arenas don't support realloc in place. Just allocate a new block.
    void* new_ptr = __arena_alloc((Arena*)ctx, new_size, alignof(max_align_t));
    // NOTE: We don't copy old data because we don't know its size.
    // Users of arena should not expect realloc to preserve contents.
    return new_ptr;
}

static void* arena_free_fn(void* ctx, void* ptr) {
    (void)ctx;
    (void)ptr;
    // Arena doesn't support freeing individual blocks. So do nothing here
    return NULL;
}

static u64 ensure_alignment(u64 cursor, u64 align) {
    return (cursor + (align - 1)) & ~(align - 1);
}

static Region* arena_region_create(u64 size) {
    Region* region = malloc(sizeof(Region));
    if(region == NULL) return NULL;

    u64 actual_size = LOY_MAX(REGION_SIZE, size);
    void* start_ptr = malloc(actual_size);
    if(start_ptr == NULL) {
        free(region);
        return NULL;
    }

    region->cursor = 0;
    region->size = actual_size;
    region->next = NULL;
    region->start = start_ptr;
    return region;
}

static Region* arena_region_get_current(Arena* arena) {
    LOY_ASSERT_NE(arena->regions, NULL, "arena has no base region");
    Region* curr = arena->regions;
    while(curr->next) {
        curr = curr->next;
    }
    return curr;
}

static void arena_append_region(Arena* arena, Region* region) {
    Region* curr = arena_region_get_current(arena);
    curr->next = region;
}

LoyResult arena_create(Allocator* out) {
    Arena* arena = malloc(sizeof(Arena));
    if(arena == NULL) return LOY_ERROR_ALLOC;

    arena->regions = arena_region_create(REGION_SIZE);
    if(arena->regions == NULL) return LOY_ERROR_ALLOC;

    Allocator allocator = {
        .ctx = arena,
        .free = arena_free_fn,
        .alloc = arena_alloc_fn,
        .realloc = arena_realloc_fn,
    };

    *out = allocator;
    return LOY_OK;
}

void* __arena_alloc(Arena* arena, u64 size, u64 align) {
    LOY_ASSERT((align & (align - 1)) == 0, "align must be a power of two");
    Region* region = arena_region_get_current(arena);

    u64 base = (u64)region->start + region->cursor;
    u64 aligned_ptr = ensure_alignment(base, align);
    u64 new_cursor = aligned_ptr - (u64)region->start;

    if(new_cursor + size > region->size) {
        Region* new_region = arena_region_create(size);
        if(new_region == NULL) return NULL;

        arena_append_region(arena, new_region);
        return __arena_alloc(arena, size, align);
    }

    region->cursor = new_cursor + size;
    return (void*)aligned_ptr;
}

void arena_destroy(Arena* arena) {
    Region* curr = arena->regions;

    while(curr) {
        Region* next = curr->next;
        free(curr->start);
        free(curr);
        curr = next;
    }

    free(arena);
}

void arena_reset(Arena* arena) {
    Region* curr = arena->regions;

    while(curr) {
        curr->cursor = 0;
        curr = curr->next;
    }
}
