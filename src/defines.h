#ifndef _DEFINES_H
#define _DEFINES_H

#include <stdbool.h>
#include <stdlib.h>

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

#define LOY_MAX(a, b) (a > b ? a : b)
#define LOY_MIN(a, b) (a < b ? a : b)

#define PP_SPACES 4

#define UNREACHABLE(...)                                         \
        do {                                                     \
            fprintf(stderr, "[FATAL] Entered unreachable code"); \
            if(*#__VA_ARGS__) fprintf(stderr, ": " __VA_ARGS__); \
            fprintf(stderr, ".\n");                              \
            fprintf(stderr, "@ %s:%d\n", __FILE__, __LINE__);    \
            abort();                                             \
        } while(0)

#endif
