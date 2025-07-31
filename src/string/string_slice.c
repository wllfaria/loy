#include "string_slice.h"
#include <stdio.h>

inline StringSlice string_slice_create(char* data) {
    StringSlice slice = { .ptr = data, .len = strlen(data) };
    return slice;
}

inline u64 string_slice_len(StringSlice slice) {
    return slice.len;
}

inline char* string_slice_ptr(StringSlice slice) {
    return slice.ptr;
}

inline StringChars string_slice_chars(StringSlice slice) {
    StringChars chars = { .slice = slice, .cursor = 0 };
    return chars;
}

inline char string_chars_next(StringChars* chars) {
    if(chars->cursor >= chars->slice.len) {
        return EOF;
    }

    u64 pos = chars->cursor;
    chars->cursor++;
    return chars->slice.ptr[pos];
}

inline char string_chars_peek(StringChars* chars) {
    u64 pos = chars->cursor;
    if(pos >= chars->slice.len) {
        return EOF;
    }

    return chars->slice.ptr[pos];
}
