#ifndef _STRING_SLICE_H
#define _STRING_SLICE_H

#include "../defines.h"
#include <string.h>

typedef struct {
    const char* ptr;
    u64         len;
} StringSlice;

u64 string_slice_len(StringSlice slice);
const char* string_slice_ptr(StringSlice slice);
StringSlice string_slice_create(const char* data);

StringSlice string_slice_substring(
    StringSlice needle,
    u64 start,
    u64 end
);

StringSlice string_slice_get_line(StringSlice slice, u64 target_line);

typedef struct {
    StringSlice slice;
    u64         cursor;
} StringChars;

StringChars string_slice_chars(StringSlice slice);
char string_chars_next(StringChars* chars);
char string_chars_peek(StringChars* chars);

#endif
