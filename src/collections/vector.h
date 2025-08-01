#ifndef _VECTOR_H
#define _VECTOR_H

#include "../defines.h"

typedef struct {
    void* buf;
    u64   len;
    u64   cap;
    bool  fixed_cap;
    u64   t_size;
} Vector;

#define vector_push(vec, value)                      \
    do {                                             \
        __typeof__(value) _val = (value);            \
        extern void __vec_push(Vector*, void*, u64); \
        __vec_push((vec), &_val, sizeof(_val));      \
    } while(0)

#define vector_push_ptr(vec, ptr)                    \
    do {                                             \
        void* _ptr_tmp = (void*)(ptr);               \
        extern void __vec_push(Vector*, void*, u64); \
        __vec_push((vec), &_ptr_tmp, sizeof(void*)); \
    } while(0)

Vector vector_create(void);
Vector vector_create_with_capacity(u64 capacity);
void vector_destroy(Vector* vec, FreeFn fn);
void* vector_get(Vector* vec, u64 idx);

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
