#include "string_slice.h"
#include <stdio.h>

inline StringSlice string_slice_create(const char* data) {
    StringSlice slice = { .ptr = data, .len = strlen(data) };
    return slice;
}

inline u64 string_slice_len(StringSlice slice) {
    return slice.len;
}

inline const char* string_slice_ptr(StringSlice slice) {
    return slice.ptr;
}

inline StringSlice string_slice_substring(
    StringSlice needle,
    u64 start,
    u64 end
) {
    LOY_ASSERT(end <= needle.len, "byte slice out of bounds");
    const char* ptr = needle.ptr + start;
    StringSlice slice = { .ptr = ptr, .len = end - start };
    return slice;
}

inline StringChars string_slice_chars(StringSlice slice) {
    StringChars chars = { .slice = slice, .cursor = 0 };
    return chars;
}

inline StringSlice string_slice_get_line(StringSlice slice, u64 target_line) {
    u64 curr_line = 0;
    u64 line_start = 0;
    u64 i = 0;

    for(; i < slice.len; i++) {
        if(curr_line == target_line) {
            line_start = i;
            break;
        }

        if(slice.ptr[i] == '\n') curr_line++;
    }

    u64 line_end = slice.len;
    for(; i < slice.len; i++) {
        if(slice.ptr[i] == '\n') {
            line_end = i;
            break;
        }
    }

    StringSlice line_slice = {
        .ptr = slice.ptr + line_start,
        .len = line_end - line_start,
    };

    return line_slice;
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
