#ifndef _VECTOR_H
#define _VECTOR_H

#include <stdio.h>

#include "../defines.h"

typedef struct {
    void* buf;
    u64   len;
    u64   cap;
    bool  fixed_cap;
    u64   t_size;
    bool  is_ptr;
    bool  is_initialized;
} Vector;

#define vector_push(vec, value)                                        \
    do {                                                               \
        if((vec)->is_initialized && (vec)->is_ptr) {                   \
            fprintf(stderr, "Attempt to push value to pointer vec\n"); \
            fprintf(stderr, "@ %s:%d\n", __FILE__, __LINE__);          \
            exit(EXIT_FAILURE);                                        \
        }                                                              \
        __typeof__(value) _val = (value);                              \
        extern void __vec_push(Vector*, void*, u64);                   \
        __vec_push((vec), &_val, sizeof(_val));                        \
    } while(0)

#define vector_push_ptr(vec, ptr)                                            \
    do {                                                                     \
        if(!(vec)->is_initialized) (vec)->is_ptr = true;                     \
        if((vec)->is_ptr) {                                                  \
            void* temp_ptr = (void*)ptr;                                     \
            extern void __vec_push(Vector*, void*, u64);                     \
            __vec_push((vec), &temp_ptr, sizeof(void*));                     \
        } else {                                                             \
            fprintf(stderr, "Attempt to push pointer to non-pointer vec\n"); \
            fprintf(stderr, "@ %s:%d\n", __FILE__, __LINE__);                \
            exit(EXIT_FAILURE);                                              \
        }                                                                    \
    } while(0)

Vector vector_create(void);
Vector vector_create_with_capacity(u64 capacity);
void vector_destroy(Vector* vec, FreeFn fn);
void* vector_get(Vector* vec, u64 idx);
void* vector_get_last(Vector* vec);

typedef struct {
    Vector* vec;
    u64     cursor;
} VectorIter;

typedef char* (*FmtFn)(void*, u64);

VectorIter vector_iter(Vector* vec);
void* vector_iter_next(VectorIter* iter);
void* vector_iter_peek(VectorIter* iter);
void vector_inspect(Vector* vec, FmtFn fmt_fn);

#endif
