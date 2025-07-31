#ifndef _STRING_SLICE_H
#define _STRING_SLICE_H

#include "../defines.h"
#include <string.h>

typedef struct {
    char* ptr;
    u64   len;
} StringSlice;

u64 string_slice_len(StringSlice slice);
char* string_slice_ptr(StringSlice slice);
StringSlice string_slice_create(char* data);

typedef struct {
    StringSlice slice;
    u64         cursor;
} StringChars;

StringChars string_slice_chars(StringSlice slice);
char string_chars_next(StringChars* chars);
char string_chars_peek(StringChars* chars);

#endif
