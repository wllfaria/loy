#pragma once

#ifndef _DEFINES_H
#define _DEFINES_H

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

typedef unsigned char  u8;
typedef unsigned short u16;
typedef unsigned int   u32;
typedef unsigned long  u64;

typedef char  i8;
typedef short i16;
typedef int   i32;
typedef long  i64;

typedef float  f32;
typedef double f64;

typedef enum {
    LOY_OK = 0,
    LOY_ERROR_ALLOC,
    LOY_ERROR_IO,
    LOY_ERROR_LEXER_INVALID_TOKEN,
    LOY_ERROR_PARSER_INVALID_TOKEN,
} LoyResult;

#define LOY_MAX(a, b) (a > b ? a : b)
#define LOY_MIN(a, b) (a < b ? a : b)

#define LOY_BAIL(msg)               \
    do {                            \
        fprintf(stderr, "%s", msg); \
        exit(EXIT_FAILURE);         \
    } while(0)

#define LOY_ASSERT(cond, ...)                                  \
    do {                                                       \
        if(!(cond)) {                                          \
            fprintf(stderr, "Assertion failed: ");             \
            if(*#__VA_ARGS__) fprintf(stderr, "" __VA_ARGS__); \
            fprintf(stderr, "\n");                             \
            fprintf(stderr, "@ %s:%d\n", __FILE__, __LINE__);  \
            exit(EXIT_FAILURE);                                \
        }                                                      \
    } while(0)                                                 \

#define LOY_ASSERT_EQ(left, right, ...)                            \
    do {                                                           \
        if((left) != (right)) {                                    \
            fprintf(stderr, "Assertion `left == right` failed: "); \
            if(*#__VA_ARGS__) fprintf(stderr, "" __VA_ARGS__);     \
            fprintf(stderr, "\n");                                 \
            fprintf(stderr, "@ %s:%d\n", __FILE__, __LINE__);      \
            exit(EXIT_FAILURE);                                    \
        }                                                          \
    } while(0)

#define LOY_ASSERT_NE(left, right, ...)                            \
    do {                                                           \
        if((left) == (right)) {                                    \
            fprintf(stderr, "Assertion `left != right` failed: "); \
            if(*#__VA_ARGS__) fprintf(stderr, "" __VA_ARGS__);     \
            fprintf(stderr, "\n");                                 \
            fprintf(stderr, "@ %s:%d\n", __FILE__, __LINE__);      \
            exit(EXIT_FAILURE);                                    \
        }                                                          \
    } while(0)

#define ARRAY_LEN(arr) sizeof(arr) / sizeof(arr[0])

#define alignof(type) __alignof__(type)

#define PP_SPACES 4

#if defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)
    #define LIKELY_(x)   __builtin_expect(x, 1)
    #define UNLIKELY_(x) __builtin_expect(x, 0)
#else
    #define LIKELY_(x)   (x)
    #define UNLIKELY_(x) (x)
#endif

#define UNREACHABLE(...)                                     \
    do {                                                     \
        fprintf(stderr, "[FATAL] Entered unreachable code"); \
        if(*#__VA_ARGS__) fprintf(stderr, ": " __VA_ARGS__); \
        fprintf(stderr, ".\n");                              \
        fprintf(stderr, "@ %s:%d\n", __FILE__, __LINE__);    \
        abort();                                             \
    } while(0)

#endif
