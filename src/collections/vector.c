#include "vector.h"
#include "../defines.h"
#include "../string/string_builder.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

static u64 vector_min_cap_for_size(u64 t_size) {
    if(t_size == 1) {
        return 8;
    } else if(t_size <= 1024) {
        return 4;
    } else {
        return 1;
    }
}

inline static void vector_grow_amortized(
    Vector* vec,
    u64 additional,
    u64 t_size
) {
    u64 required_cap = vec->len + additional;

    if(required_cap < vec->len) {
        // TODO: this is an overflow.
    }

    u64 min_cap = LOY_MAX(vec->cap * 2, required_cap);
    min_cap = LOY_MAX(vector_min_cap_for_size(t_size), min_cap);

    void* new;
    new = vec->allocator->alloc(vec->allocator->ctx, t_size * min_cap);
    memcpy(new, vec->buf, vec->len * t_size);
    vec->allocator->free(vec->allocator->ctx, vec->buf);
    vec->buf = new;
    vec->cap = min_cap;
}

static void vector_resize_inner(Vector* vec, u64 additional, u64 t_size) {
    // Edge case of initializing a vector for the first time
    if(vec->len == 0 && vec->cap == 0 && vec->buf == NULL) {
        vec->cap = vector_min_cap_for_size(t_size);
        Allocator* allocator = vec->allocator;
        vec->buf = allocator->alloc(allocator->ctx, t_size * vec->cap);
        return;
    }

    // Edge case when the vector was initialized with a fixed capacity
    if(vec->len == 0 && vec->fixed_cap && vec->buf == NULL) {
        Allocator* allocator = vec->allocator;
        vec->buf = allocator->alloc(allocator->ctx, t_size * vec->cap);
        return;
    }

    // Only resize a vector that doesn't have a fixed capacity.
    if(vec->len + additional <= vec->cap) return;

    if(vec->fixed_cap) {
        fprintf(stderr, "Attempted to grow vector with fixed capacity\n");
        exit(EXIT_FAILURE);
    }

    vector_grow_amortized(vec, additional, t_size);
}

static void vector_push_inner(Vector* vec, void* item, u64 t_size) {
    assert(vec != NULL);

    // Ensuring `item` is a valid pointer
    if(item == NULL) {
        printf("null pointer value passed to vector_push");
        exit(EXIT_SUCCESS);
    }

    // Vector already contains sized items, so we must ensure we don't allow
    // pushing something with incorrect sizes
    if(vec->t_size != 0 && vec->t_size != t_size) {
        printf("invalid sized pointer passed to vector_push\n");
        exit(EXIT_SUCCESS);
    }

    if(vec->t_size == 0) vec->t_size = t_size;
    if(!vec->is_initialized) vec->is_initialized = true;

    vector_resize_inner(vec, 1, t_size);
    memcpy((u8*)vec->buf + vec->len * vec->t_size, item, vec->t_size);
    vec->len++;
}

void __vec_push(Vector* vec, void* item, u64 item_size) {
    vector_push_inner(vec, item, item_size);
}

Vector vector_create(Allocator* allocator) {
    Vector vec = {
        .buf = NULL,
        .len = 0,
        .cap = 0,
        .t_size = 0,
        .fixed_cap = false,
        .is_ptr = false,
        .is_initialized = false,
        .allocator = allocator,
    };

    return vec;
}

Vector vector_create_with_capacity(Allocator* allocator, u64 capacity) {
    Vector vec = vector_create(allocator);
    vec.fixed_cap = true;
    vec.cap = capacity;
    return vec;
}

void vector_destroy(Vector* vec) {
    if(vec->buf) {
        vec->allocator->free(vec->allocator->ctx, vec->buf);
    }
}

void* vector_get(Vector* vec, u64 idx) {
    LOY_ASSERT(vec != NULL, "invalid vector pointer");
    if(idx >= vec->len) return NULL;
    void* item = (u8*)vec->buf + idx * vec->t_size;
    if(vec->is_ptr) return *(void**)item;
    return item;
}

void* vector_get_last(Vector* vec) {
    if(vec->len == 0) return NULL;
    return vector_get(vec, vec->len - 1);
}

void vector_inspect(Allocator* allocator, Vector* vec, FmtFn fmt_fn) {
    StringBuilder builder = string_builder_create(allocator);
    string_builder_write_string(&builder, "Vector{\n");
    string_builder_write_format(&builder, "    len: %llu,\n", vec->len);
    string_builder_write_format(&builder, "    cap: %llu,\n", vec->cap);
    string_builder_write_string(&builder, "    buf: [\n");

    for(u64 i = 0; i < vec->len; i++) {
        void* raw = vector_get(vec, i);
        char* formatted_item = fmt_fn(allocator, raw, 2);
        string_builder_write_string(&builder, formatted_item);
        string_builder_write_string(&builder, i < vec->len - 1 ? ",\n" : "\n");
    }

    string_builder_write_string(&builder, "    ],\n");
    string_builder_write_string(&builder, "}\n");
    printf("%s", string_builder_to_string(&builder));
}

VectorIter vector_iter(Vector* vec) {
    VectorIter iter = { .vec = vec, .cursor = 0 };

    return iter;
}

void* vector_iter_next(VectorIter* iter) {
    if(iter->cursor >= iter->vec->len) return NULL;
    u64 pos = iter->cursor;
    iter->cursor++;
    return vector_get(iter->vec, pos);
}

void* vector_iter_peek(VectorIter* iter) {
    if(iter->cursor >= iter->vec->len) return NULL;
    return vector_get(iter->vec, iter->cursor);
}
